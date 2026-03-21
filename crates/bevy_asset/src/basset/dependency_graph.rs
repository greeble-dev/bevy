use std::sync::{PoisonError, RwLock};

use crate::{
    basset::{
        cache::{ActionCacheKey, DependencyCacheKey, DependencyCacheValue, MemoryAndFileCache},
        BassetShared,
    },
    AssetRef, AssetServer,
};
use alloc::{sync::Arc, vec, vec::Vec};
use bevy_platform::collections::HashMap;
use indexmap::IndexMap;
use petgraph::{
    acyclic::Acyclic, data::Build, graph::NodeIndex, prelude::StableDiGraph, visit::EdgeRef,
    Direction,
};

enum InternalGraphNode {
    Valid(ActionCacheKey),
    Unknown,
}

struct InternalGraph {
    graph: Acyclic<StableDiGraph<InternalGraphNode, ()>>,
    path_to_node: HashMap<AssetRef<'static>, NodeIndex>,
}

impl InternalGraph {
    fn set(
        &mut self,
        path: &AssetRef<'static>,
        dependency_key: DependencyCacheKey,
        dependees: &[AssetRef<'static>],
    ) -> Option<ActionCacheKey> {
        // XXX TODO: Validate the existing entry? Or is it an error to set twice?
        // XXX TODO: Try to optimise this by reusing the entry?
        if let Some(existing_node_id) = self.path_to_node.get(path).copied() {
            self.path_to_node.remove(path);
            self.graph.remove_node(existing_node_id);
        }

        // Gather the node id and action key of each dependee, returning `None`
        // if any are absent or unknown.
        let resolved = dependees
            .iter()
            .map(|dependee_path| {
                self.path_to_node
                    .get(dependee_path)
                    .copied()
                    .and_then(|dependee_node_id| {
                        let dependee_node = self
                            .graph
                            .node_weight(dependee_node_id)
                            .expect("Graph node should always exist XXX TODO: Document?");

                        match dependee_node {
                            InternalGraphNode::Valid(action_key) => {
                                Some((dependee_node_id, *action_key))
                            }
                            InternalGraphNode::Unknown => None,
                        }
                    })
            })
            .collect::<Option<Vec<(NodeIndex, ActionCacheKey)>>>();

        let (node_id, action_key) = if let Some(resolved) = resolved {
            let action_key = ActionCacheKey::new(dependency_key, resolved.iter().map(|(_, k)| *k));

            let node_id = self.graph.add_node(InternalGraphNode::Valid(action_key));

            for (dependee_node_id, _) in resolved.iter() {
                self.graph
                    .try_add_edge(node_id, *dependee_node_id, ())
                    .expect("Oops, cycle. XXX TODO");
            }

            (node_id, Some(action_key))
        } else {
            let node_id = self.graph.add_node(InternalGraphNode::Unknown);

            (node_id, None)
        };

        self.path_to_node.insert(path.clone(), node_id);

        action_key
    }

    fn get(&self, path: &AssetRef<'static>) -> Option<ActionCacheKey> {
        self.path_to_node.get(path).and_then(|&node_id| {
            match self.graph.node_weight(node_id).expect("XXX TODO") {
                InternalGraphNode::Valid(action_key) => Some(*action_key),
                InternalGraphNode::Unknown => None,
            }
        })
    }

    fn contains(&self, path: &AssetRef<'static>) -> bool {
        self.path_to_node.contains_key(path)
    }

    fn invalidate(&mut self, path: &AssetRef<'static>) {
        if let Some(initial_node_index) = self.path_to_node.get(path).copied() {
            let mut stack = vec![initial_node_index];

            while let Some(node_index) = stack.pop() {
                stack.extend(
                    self.graph
                        .edges_directed(node_index, Direction::Incoming)
                        .map(|edge| edge.source()),
                );
                self.graph.remove_node(node_index);
            }
        }
    }
}

struct DependencyGraph {
    // XXX TODO: RwLock?
    graph: RwLock<InternalGraph>,
    // XXX TODO?
    //versions: ?
    cache: MemoryAndFileCache<DependencyCacheKey, Arc<DependencyCacheValue>>,
}

impl DependencyGraph {
    async fn action_key(
        &self,
        path: &AssetRef<'static>,
        // XXX TODO: Should we take shared? Or do we contain content cache and anything else?
        // That would fit in with invalidation on file change - we want to invalidate both
        // the dependency graph and the content cache.
        shared: &BassetShared,
        asset_server: &AssetServer,
    ) -> Option<ActionCacheKey> {
        let mut stack = Vec::<(AssetRef<'static>, Option<DependencyCacheKey>)>::new();
        let mut pending = IndexMap::<
            AssetRef<'static>,
            Option<(DependencyCacheKey, Vec<AssetRef<'static>>)>,
        >::new();

        stack.push((path.clone(), None));

        while let Some((path, expected_dependency_key)) = stack.pop() {
            // If we already have a graph node then we're done.
            if self
                .graph
                .read()
                .unwrap_or_else(PoisonError::into_inner)
                .contains(&path)
            {
                continue;
            };

            // XXX TODO: Settings parameter?
            let dependency_key = shared.dependency_key(&path, None, asset_server).await;

            if expected_dependency_key.is_some_and(|e| e != dependency_key) {
                pending.insert(path.clone(), None);
                continue;
            }

            // XXX TODO: Doing separate get and set means looking up the node
            // twice. Avoid?

            // XXX TODO: Await inside read lock?
            if let Some(cached) = self.cache.get(&dependency_key, &path).await {
                let dependee_paths = cached
                    .list()
                    .iter()
                    .map(|(dependee_path, _)| dependee_path.clone())
                    .collect();

                assert!(!pending.contains_key(&path));
                pending.insert(path.clone(), Some((dependency_key, dependee_paths)));

                for (dependee_path, dependee_key) in cached.list() {
                    if !pending.contains_key(dependee_path) {
                        stack.push((dependee_path.clone(), Some(*dependee_key)));
                    }
                }
            } else {
                assert!(!pending.contains_key(&path));
                pending.insert(path.clone(), None);
            }
        }

        if !pending.is_empty() {
            let mut graph = self.graph.write().unwrap_or_else(PoisonError::into_inner);

            for (path, potential) in pending.iter().rev() {
                match potential {
                    Some((dependency_key, dependees)) => {
                        graph.set(&path, *dependency_key, dependees);
                    }
                    None => {
                        graph.invalidate(&path);
                    }
                };
            }
        }

        self.graph
            .read()
            .unwrap_or_else(PoisonError::into_inner)
            .get(path)
    }

    fn register_dependencies(
        &self,
        path: &AssetRef<'static>,
        dependency_key: DependencyCacheKey,
        dependency_value: DependencyCacheValue,
    ) -> Option<ActionCacheKey> {
        // XXX TODO: Avoid allocation or otherwise simplify this?
        let dependee_paths = dependency_value
            .list()
            .iter()
            .map(|(dependee_path, _)| dependee_path.clone())
            .collect::<Vec<_>>();

        let action_key = self
            .graph
            .write()
            .unwrap_or_else(PoisonError::into_inner)
            .set(&path, dependency_key, &dependee_paths);

        self.cache
            .put(dependency_key, Arc::new(dependency_value), path);

        action_key
    }

    fn invalidate(&self, path: &AssetRef<'static>) {
        self.graph
            .write()
            .unwrap_or_else(PoisonError::into_inner)
            .invalidate(path);
    }
}
