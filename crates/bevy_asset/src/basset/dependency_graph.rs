use crate::{
    basset::{
        cache::{
            ActionCacheKey, CacheLoaderDependency, DependencyCacheKey, DependencyCacheValue,
            MemoryAndFileCache,
        },
        DevelopmentActionSource, RootAssetPath, RootAssetRef,
    },
    LoaderDependency,
};
use alloc::{string::ToString, sync::Arc, vec, vec::Vec};
use bevy_platform::collections::HashMap;
use core::fmt::{Debug, Display, Write};
use indexmap::IndexMap;
use petgraph::{
    acyclic::Acyclic, data::Build, graph::NodeIndex, prelude::StableDiGraph, visit::EdgeRef,
    Direction,
};
use std::{
    path::PathBuf,
    sync::{Mutex, PoisonError},
};
use tracing::warn;

#[derive(Debug)]
enum InternalGraphNode {
    Valid(ActionCacheKey, DependencyCacheKey),
    Unknown,
}

#[derive(Default)]
struct InternalGraph {
    graph: Acyclic<StableDiGraph<InternalGraphNode, ()>>,
    // XXX TODO: Nodes that are `LoaderDependency::file` should not have dependee links.
    // Should we verify this?
    path_to_node: HashMap<LoaderDependency, NodeIndex>,
}

impl InternalGraph {
    // XXX TODO: Document and reconsider name. Corresponds to `LoaderDependency::Load`.
    fn set_load(
        &mut self,
        path: RootAssetRef<'static>,
        dependency_key: DependencyCacheKey,
        dependency_value: &DependencyCacheValue,
    ) -> Option<ActionCacheKey> {
        // XXX TODO: Validate the existing entry? Or is it an error to set twice?
        // XXX TODO: Try to optimize this by reusing the entry?
        // XXX TODO: Avoid clone?
        if let Some(existing_node_id) = self
            .path_to_node
            .get(&LoaderDependency::Load(path.clone()))
            .copied()
        {
            return match self
                .graph
                .node_weight(existing_node_id)
                .expect("Graph node should always exist XXX TODO: Document?")
            {
                InternalGraphNode::Valid(existing_action_key, existing_dependency_key) => {
                    // XXX TODO: This can happen if we fail to invalidate on file
                    // changes. Do we need to make that robust or handle it gracefully
                    // here?
                    assert_eq!(*existing_dependency_key, dependency_key);

                    Some(*existing_action_key)
                }
                InternalGraphNode::Unknown => None,
            };
        }

        // Gather the node id and action key of each dependee, returning `None`
        // if any are absent or unknown.
        //
        // XXX TODO: Check that the ordering of the output is reliable, since the
        // ordering affects the action key.
        //
        // XXX TODO: More efficient if we could do `into_iter?`
        let resolved = dependency_value
            .loader_dependees()
            .iter()
            .map(|CacheLoaderDependency(dependee, dependee_key)| {
                if let Some(dependee_node_id) = self.path_to_node.get(dependee)
                    .copied() {
                        let dependee_node = self
                            .graph
                            .node_weight(dependee_node_id)
                            .expect("Graph node should always exist XXX TODO: Document?");

                        match dependee_node {
                            InternalGraphNode::Valid(action_key, existing_dependee_key) => {
                                // XXX TODO: Should go behind validation flag?
                                assert_eq!(dependee_key, existing_dependee_key);
                                Some((dependee_node_id, *action_key))
                            }
                            InternalGraphNode::Unknown => None,
                        }
                    }
                 else {
                    warn!(?dependee, %dependee_key, "Failed to find graph node - were dependencies not registered for this asset?");
                    None
                 }
            })
            .collect::<Option<Vec<(NodeIndex, ActionCacheKey)>>>();

        let (node_id, action_key) = if let Some(resolved) = resolved {
            let action_key = ActionCacheKey::new(dependency_key, resolved.iter().map(|(_, k)| *k));

            let node_id = self
                .graph
                .add_node(InternalGraphNode::Valid(action_key, dependency_key));

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

        // XXX TODO: Duplicates earlier clone?
        self.path_to_node
            .insert(LoaderDependency::Load(path), node_id);

        action_key
    }

    // XXX TODO: Document and reconsider name. Corresponds to `LoaderDependency::File`.
    // XXX TODO: Duplicates a lot of `set_load`. Try refactor?
    fn set_file(
        &mut self,
        path: RootAssetPath<'static>,
        dependency_key: DependencyCacheKey,
    ) -> Option<ActionCacheKey> {
        // XXX TODO: Validate the existing entry? Or is it an error to set twice?
        // XXX TODO: Try to optimize this by reusing the entry?
        // XXX TODO: Avoid clone?
        if let Some(existing_node_id) = self
            .path_to_node
            .get(&LoaderDependency::File(path.clone()))
            .copied()
        {
            return match self
                .graph
                .node_weight(existing_node_id)
                .expect("Graph node should always exist XXX TODO: Document?")
            {
                InternalGraphNode::Valid(existing_action_key, existing_dependency_key) => {
                    // XXX TODO: This can happen if we fail to invalidate on file
                    // changes. Do we need to make that robust or handle it gracefully
                    // here?
                    assert_eq!(*existing_dependency_key, dependency_key);

                    Some(*existing_action_key)
                }
                InternalGraphNode::Unknown => None,
            };
        }

        let action_key = ActionCacheKey::new(dependency_key, core::iter::empty());

        let node_id = self
            .graph
            .add_node(InternalGraphNode::Valid(action_key, dependency_key));

        self.path_to_node
            .insert(LoaderDependency::File(path), node_id);

        Some(action_key)
    }

    fn get(&self, path: &LoaderDependency) -> Option<ActionCacheKey> {
        self.path_to_node.get(path).and_then(|&node_id| {
            match self.graph.node_weight(node_id).expect("XXX TODO") {
                InternalGraphNode::Valid(action_key, _) => Some(*action_key),
                InternalGraphNode::Unknown => None,
            }
        })
    }

    fn contains(&self, path: &LoaderDependency) -> bool {
        self.path_to_node.contains_key(path)
    }

    fn invalidate(&mut self, path: &LoaderDependency) {
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

impl Debug for InternalGraph {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        let node_to_path: HashMap<NodeIndex, &LoaderDependency> =
            HashMap::from_iter(self.path_to_node.iter().map(|(path, node)| (*node, path)));

        // XXX TODO: Maybe petgraph has this built-in somewhere?
        let mut root_nodes = self
            .graph
            .nodes_iter()
            .filter(|&n| {
                self.graph
                    .neighbors_directed(n, Direction::Incoming)
                    .next()
                    .is_none()
            })
            .map(|n| node_to_path[&n].clone())
            .collect::<Vec<_>>();

        root_nodes.sort();

        let mut stack = Vec::<(NodeIndex, usize)>::new();

        for root_node_path in root_nodes.into_iter() {
            let root_node_id = self.path_to_node[&root_node_path];

            // Skip spammy embedded assets. XXX TODO: Rethink at some point.
            if let LoaderDependency::Load(path) = root_node_path
                && path.to_string().starts_with("embedded://")
            {
                continue;
            }

            stack.push((root_node_id, 0));

            while let Some((node_id, depth)) = stack.pop() {
                for _ in 0..(depth + 1) {
                    f.write_str("    ")?;
                }

                f.write_str("+-- ")?;

                if let InternalGraphNode::Valid(action_key, dependency_key) = self.graph[node_id] {
                    Display::fmt(&action_key, f)?;
                    f.write_char('/')?;
                    Display::fmt(&dependency_key, f)?;
                    f.write_char(' ')?;
                } else {
                    f.write_str("unknown/unknown ")?;
                }

                Debug::fmt(node_to_path[&node_id], f)?;

                // XXX TODO: Platform specific newline?
                f.write_char('\n')?;

                for child in self.graph.neighbors_directed(node_id, Direction::Outgoing) {
                    stack.push((child, depth + 1));
                }
            }
        }

        Ok(())
    }
}

pub(crate) struct DependencyGraph {
    // XXX TODO: Would have preferred `RwLock`, but we can't because `petgraph::Acyclic`
    // is not `Sync` due to using `RefCell`.
    graph: Mutex<InternalGraph>,
    // XXX TODO: Loader versions.
    //versions: ?
    cache: Option<MemoryAndFileCache<DependencyCacheKey, Arc<DependencyCacheValue>>>,
}

impl DependencyGraph {
    pub(crate) fn new(dependency_cache_path: Option<PathBuf>, validate: bool) -> Self {
        Self {
            graph: Default::default(),
            // XXX TODO: Add an option to disable the dependency memory cache?
            cache: Some(MemoryAndFileCache::new(
                "dependency_cache",
                dependency_cache_path,
                validate,
            )),
        }
    }

    // XXX TODO: Should this take an `AssetRef` or an `AssetAction2`? Need to
    // think through whether regular asset loads should be cached. Although caching
    // isn't the only client - the dependency graph needs the action keys of non-actions
    // to calculate the action key of actions.
    pub(crate) async fn action_key(
        &self,
        path: &RootAssetRef<'static>,
        // XXX TODO: Should we take shared? Or do we contain content cache and anything else?
        // That would fit in with invalidation on file change - we want to invalidate both
        // the dependency graph and the content cache.
        action_source: &DevelopmentActionSource,
    ) -> Option<ActionCacheKey> {
        if let Some(cache) = &self.cache {
            let mut stack = Vec::<(LoaderDependency, Option<DependencyCacheKey>)>::new();

            // XXX TODO: Document that `IndexMap` is for consistent ordering.
            let mut pending_loads = IndexMap::<
                RootAssetRef<'static>,
                Option<(DependencyCacheKey, Arc<DependencyCacheValue>)>,
            >::new();

            let mut pending_files =
                IndexMap::<RootAssetPath<'static>, Option<DependencyCacheKey>>::new();

            // XXX TODO: Check that we're correct to assume `LoaderDependency::Load`.
            stack.push((LoaderDependency::Load(path.clone()), None));

            while let Some((path, tentative_dependency_key)) = stack.pop() {
                // Early out if we've already hit this path.
                if match &path {
                    LoaderDependency::Load(path) => pending_loads.contains_key(path),
                    LoaderDependency::File(path) => pending_files.contains_key(path),
                } {
                    continue;
                }

                // Early out if we already have a graph node for this path.
                if self
                    .graph
                    .lock()
                    .unwrap_or_else(PoisonError::into_inner)
                    .contains(&path)
                {
                    continue;
                };

                // XXX TODO: Settings parameter?
                let current_dependency_key =
                    action_source.dependency_key_internal(&path, None).await;

                // The tentative dependency key came from the dependency cache. Check
                // if it matches the current file state. If not then invalidate the
                // node.
                if tentative_dependency_key.is_some_and(|e| e != current_dependency_key) {
                    match path {
                        LoaderDependency::Load(path) => {
                            pending_loads.insert(path.clone(), None);
                        }
                        LoaderDependency::File(path) => {
                            pending_files.insert(path.clone(), None);
                        }
                    }

                    continue;
                }

                match path {
                    LoaderDependency::Load(path) => {
                        // XXX TODO: Keep?
                        assert!(!pending_loads.contains_key(&path));

                        if let Some(dependency_value) =
                            cache.get(&current_dependency_key, &path).await
                        {
                            for CacheLoaderDependency(dependee_path, dependee_key) in
                                dependency_value.loader_dependees()
                            {
                                stack.push((dependee_path.clone(), Some(*dependee_key)));
                            }

                            pending_loads.insert(
                                path.clone(),
                                Some((current_dependency_key, dependency_value)),
                            );
                        } else {
                            pending_loads.insert(path.clone(), None);
                        }
                    }
                    LoaderDependency::File(path) => {
                        // XX TODO: Keep?
                        assert!(!pending_files.contains_key(&path));

                        pending_files.insert(path.clone(), Some(current_dependency_key));
                    }
                }
            }

            if !pending_loads.is_empty() || !pending_files.is_empty() {
                let mut graph = self.graph.lock().unwrap_or_else(PoisonError::into_inner);

                // XXX TODO: Document reverse order reasoning.
                for (path, potential) in pending_files.into_iter().rev() {
                    match potential {
                        Some(dependency_key) => {
                            graph.set_file(path, dependency_key);
                        }
                        None => {
                            graph.invalidate(&LoaderDependency::File(path));
                        }
                    };
                }

                // XXX TODO: Document reverse order reasoning.
                for (path, potential) in pending_loads.into_iter().rev() {
                    match potential {
                        Some((dependency_key, dependency_value)) => {
                            graph.set_load(path, dependency_key, &dependency_value);
                        }
                        None => {
                            graph.invalidate(&LoaderDependency::Load(path));
                        }
                    };
                }
            }
        }

        // XXX TODO: Check that we're correct to assume `LoaderDependency::Load`.
        // XXX TODO: Avoid clone?
        self.graph
            .lock()
            .unwrap_or_else(PoisonError::into_inner)
            .get(&LoaderDependency::Load(path.clone()))
    }

    pub(crate) fn register_dependencies_load(
        &self,
        path: &RootAssetRef<'static>,
        dependency_key: DependencyCacheKey,
        dependency_value: DependencyCacheValue,
    ) -> Option<ActionCacheKey> {
        let action_key = self
            .graph
            .lock()
            .unwrap_or_else(PoisonError::into_inner)
            .set_load(path.clone(), dependency_key, &dependency_value);

        if let Some(cache) = &self.cache {
            cache.put(dependency_key, Arc::new(dependency_value), path);
        }

        action_key
    }

    pub(crate) fn register_dependencies_file(
        &self,
        path: &RootAssetPath<'static>,
        dependency_key: DependencyCacheKey,
    ) -> Option<ActionCacheKey> {
        self.graph
            .lock()
            .unwrap_or_else(PoisonError::into_inner)
            .set_file(path.clone(), dependency_key)
    }
}

// XXX TODO: Less hacky debugging?
impl Debug for DependencyGraph {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        Debug::fmt(
            &self.graph.lock().unwrap_or_else(PoisonError::into_inner),
            f,
        )
    }
}
