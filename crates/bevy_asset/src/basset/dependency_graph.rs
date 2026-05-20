use crate::{
    basset::{
        cache::{
            ActionCacheKey, BassetHash, CacheLoaderDependency, ContentCache, DependencyCacheKey,
            DependencyCacheValue, MemoryAndFileCache,
        },
        RootAssetPath, RootAssetRef,
    },
    io::{AssetSources, Reader, VecReader},
    LoaderDependency, ReadAssetBytesError,
};
use alloc::{boxed::Box, string::ToString, sync::Arc, vec, vec::Vec};
use bevy_platform::{collections::HashMap, hash::FixedHasher};
use bevy_reflect::TypeRegistryArc;
use core::{
    fmt::{Debug, Display, Write},
    hash::{BuildHasher, Hash, Hasher},
};
use indexmap::IndexMap;
use petgraph::{
    acyclic::Acyclic, data::Build, graph::NodeIndex, prelude::StableDiGraph, visit::EdgeRef,
    Direction,
};
use std::{
    path::PathBuf,
    sync::{Mutex, MutexGuard, PoisonError},
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
    action_to_node: HashMap<RootAssetRef, (NodeIndex, Arc<DependencyCacheValue>)>,
    file_to_node: HashMap<RootAssetPath<'static>, NodeIndex>,
}

impl InternalGraph {
    fn get_node_id(&self, dependency: &LoaderDependency) -> Option<NodeIndex> {
        match dependency {
            LoaderDependency::Action(action) => {
                self.action_to_node.get(action).map(|(node_id, _)| *node_id)
            }
            LoaderDependency::File(file) => self.file_to_node.get(file).copied(),
        }
    }

    fn set_action(
        &mut self,
        path: RootAssetRef,
        dependency_key: DependencyCacheKey,
        dependency_value: Arc<DependencyCacheValue>,
    ) -> Option<ActionCacheKey> {
        // XXX TODO: Validate the existing entry? Or is it an error to set twice?
        // XXX TODO: Try to optimize this by reusing the entry?
        // XXX TODO: Avoid clone?
        if let Some(existing_node_id) = self.action_to_node.get(&path).map(|(node_id, _)| *node_id)
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
                if let Some(dependee_node_id) = self.get_node_id(dependee) {
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
                    // XXX TODO: Change to error? Or make logging opt-in.
                    warn!(?dependee, %dependee_key, "Failed to find graph node - were dependencies not registered for this asset?");
                    None
                }
            })
            .collect::<Option<Vec<(NodeIndex, ActionCacheKey)>>>();

        let (node_id, action_key) = if let Some(resolved) = resolved {
            // We found valid nodes for all dependees, so we're valid. Create our
            // node and link it to the dependees.

            let action_key = ActionCacheKey::new(
                dependency_key,
                resolved
                    .iter()
                    .map(|(_, k)| *k)
                    .collect::<Vec<_>>()
                    .as_slice(),
            );

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
            // At least one dependency was not in the graph, so we're unknown.

            let node_id = self.graph.add_node(InternalGraphNode::Unknown);

            (node_id, None)
        };

        self.action_to_node
            .insert(path, (node_id, dependency_value));

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
        if let Some(existing_node_id) = self.file_to_node.get(&path).copied() {
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

        let action_key = ActionCacheKey::new(dependency_key, &[]);

        let node_id = self
            .graph
            .add_node(InternalGraphNode::Valid(action_key, dependency_key));

        self.file_to_node.insert(path, node_id);

        Some(action_key)
    }

    fn get(&self, path: &RootAssetRef) -> Option<(ActionCacheKey, Arc<DependencyCacheValue>)> {
        self.action_to_node
            .get(path)
            .and_then(|(node_id, dependency_value)| {
                match self.graph.node_weight(*node_id).expect("XXX TODO") {
                    InternalGraphNode::Valid(action_key, _) => {
                        Some((*action_key, dependency_value.clone()))
                    }
                    InternalGraphNode::Unknown => None,
                }
            })
    }

    fn contains(&self, path: &LoaderDependency) -> bool {
        match path {
            LoaderDependency::Action(action) => self.action_to_node.contains_key(action),
            LoaderDependency::File(file) => self.file_to_node.contains_key(file),
        }
    }

    fn invalidate(&mut self, path: &LoaderDependency) {
        if let Some(initial_node_index) = self.get_node_id(path) {
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
        let node_to_path: HashMap<NodeIndex, LoaderDependency> = HashMap::from_iter(
            self.action_to_node
                .iter()
                .map(|(path, (node, _))| (*node, LoaderDependency::Action(path.clone())))
                .chain(
                    self.file_to_node
                        .iter()
                        .map(|(path, node)| (*node, LoaderDependency::File(path.clone()))),
                ),
        );

        // XXX TODO: Maybe petgraph has this built-in somewhere?
        let root_nodes = self
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

        let mut stack = Vec::<(NodeIndex, usize)>::new();

        for root_node_path in root_nodes.into_iter() {
            // Unwrap is safe - the list of root nodes came from the same collection.
            let root_node_id = self.get_node_id(&root_node_path).unwrap();

            // Skip spammy embedded assets. XXX TODO: Rethink at some point.
            if let LoaderDependency::Action(path) = root_node_path
                && path.to_string().contains("embedded://")
            {
                continue;
            }

            stack.push((root_node_id, 0));
        }

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

            Debug::fmt(&node_to_path[&node_id], f)?;

            // XXX TODO: Platform specific newline?
            f.write_char('\n')?;

            for child in self.graph.neighbors_directed(node_id, Direction::Outgoing) {
                stack.push((child, depth + 1));
            }
        }

        Ok(())
    }
}

pub(crate) struct DependencyGraph {
    // XXX TODO: Would have preferred `RwLock`, but we can't because `petgraph::Acyclic`
    // is not `Sync` due to using `RefCell`.
    graph: Mutex<InternalGraph>,
    dependency_cache: Option<MemoryAndFileCache<DependencyCacheKey, Arc<DependencyCacheValue>>>,
    content_cache: ContentCache,
    // XXX TODO: We should have loader versions here for calculating dependency keys?
}

impl DependencyGraph {
    pub(crate) fn new(
        dependency_cache_path: Option<PathBuf>,
        validate: bool,
        sources: Arc<AssetSources>,
        registry: TypeRegistryArc,
    ) -> Self {
        Self {
            graph: Default::default(),
            // XXX TODO: Add an option to disable the dependency memory cache?
            dependency_cache: Some(MemoryAndFileCache::new(
                "dependency_cache",
                dependency_cache_path,
                validate,
                registry,
            )),
            content_cache: ContentCache::new(sources),
        }
    }

    fn graph(&self) -> MutexGuard<'_, InternalGraph> {
        self.graph.lock().unwrap_or_else(PoisonError::into_inner)
    }

    pub(crate) async fn dependency_key(&self, path: &LoaderDependency) -> DependencyCacheKey {
        match path {
            LoaderDependency::Action(action) => self.action_dependency_key(action),
            LoaderDependency::File(path) => self.file_dependency_key(path).await,
        }
    }

    pub(crate) fn action_dependency_key(&self, action: &RootAssetRef) -> DependencyCacheKey {
        // XXX TODO: Seed hash? Also consider using separate seeds for actions
        // and files just in case.
        let mut hasher = blake3::Hasher::new();

        // XXX TODO: Is this the best way to hash actions?
        let mut action_hasher = FixedHasher.build_hasher();
        Hash::hash(&action, &mut action_hasher);
        Hash::hash(action.action().version(), &mut action_hasher);
        hasher.update(&action_hasher.finish().to_le_bytes());

        DependencyCacheKey(BassetHash::new(*hasher.finalize().as_bytes()))
    }

    // XXX TODO: Review where this is used and make sure we're not introducing
    // any file read race conditions.
    pub(crate) async fn file_dependency_key(
        &self,
        path: &RootAssetPath<'static>,
    ) -> DependencyCacheKey {
        // XXX TODO: Seed hash? Also consider using separate seeds for actions
        // and files just in case.
        let mut hasher = blake3::Hasher::new();

        hasher.update(path.to_string().as_bytes());

        let content_hash = self.content_cache.get(path).await.expect("XXX TODO");
        hasher.update(&content_hash.as_bytes());

        DependencyCacheKey(BassetHash::new(*hasher.finalize().as_bytes()))
    }

    pub(crate) async fn read_asset_bytes(
        &self,
        path: &RootAssetPath<'static>,
        reader: &mut dyn Reader,
    ) -> Result<(Box<dyn Reader>, DependencyCacheKey), ReadAssetBytesError> {
        let (bytes, content_hash) = self.content_cache.read_asset_bytes(path, reader).await?;

        // XXX TODO: Review this function and check that we're matching the other
        // `dependency_key` function. Maybe factor out shared logic?
        let dependency_key = {
            let mut hasher = blake3::Hasher::new();

            hasher.update(path.to_string().as_bytes());
            hasher.update(&content_hash.as_bytes());

            DependencyCacheKey(BassetHash::new(*hasher.finalize().as_bytes()))
        };

        self.graph
            .lock()
            .unwrap_or_else(PoisonError::into_inner)
            .set_file(path.clone(), dependency_key);

        Ok((Box::new(VecReader::new(bytes)), dependency_key))
    }

    pub(crate) async fn action_key(
        &self,
        root_action: &RootAssetRef,
        root_dependency_key: Option<DependencyCacheKey>,
    ) -> Option<(ActionCacheKey, Arc<DependencyCacheValue>)> {
        let Some(cache) = &self.dependency_cache else {
            return None;
        };

        let root_dependency_key =
            root_dependency_key.unwrap_or_else(|| self.action_dependency_key(root_action));

        // Early out if possible.
        if let Some(existing) = self.graph().get(root_action) {
            return Some(existing);
        };

        let mut stack = Vec::<(LoaderDependency, DependencyCacheKey)>::new();

        // XXX TODO: Document that `IndexMap` is for consistent ordering.
        let mut pending_actions =
            IndexMap::<RootAssetRef, Option<(DependencyCacheKey, Arc<DependencyCacheValue>)>>::new(
            );

        let mut pending_files =
            IndexMap::<RootAssetPath<'static>, Option<DependencyCacheKey>>::new();

        // XXX TODO: This duplicates a similar block within the loop below. Refactor?
        let root_pending_action = if let Some(root_dependency_value) =
            cache.get(&root_dependency_key, root_action).await
        {
            for CacheLoaderDependency(dependee_path, dependee_key) in
                root_dependency_value.loader_dependees()
            {
                stack.push((dependee_path.clone(), *dependee_key));
            }

            Some((root_dependency_key, root_dependency_value))
        } else {
            None
        };

        pending_actions.insert(root_action.clone(), root_pending_action);

        while let Some((dependency, predicted_dependency_key)) = stack.pop() {
            // Skip if we've already hit this dependency.
            if match &dependency {
                LoaderDependency::Action(action) => pending_actions.contains_key(action),
                LoaderDependency::File(file) => pending_files.contains_key(file),
            } {
                continue;
            }

            // Skip if we already have a node for this dependency.
            if self.graph().contains(&dependency) {
                continue;
            };

            let current_dependency_key = self.dependency_key(&dependency).await;

            // If the predicted key doesn't match the current file state then
            // invalidate the node.
            if predicted_dependency_key != current_dependency_key {
                match dependency {
                    LoaderDependency::Action(path) => {
                        pending_actions.insert(path.clone(), None);
                    }
                    LoaderDependency::File(path) => {
                        pending_files.insert(path.clone(), None);
                    }
                }

                continue;
            }

            match dependency {
                LoaderDependency::Action(action) => {
                    // XXX TODO: Keep?
                    assert!(!pending_actions.contains_key(&action));

                    let pending_action = if let Some(dependency_value) =
                        cache.get(&current_dependency_key, &action).await
                    {
                        for CacheLoaderDependency(dependee_path, dependee_key) in
                            dependency_value.loader_dependees()
                        {
                            stack.push((dependee_path.clone(), *dependee_key));
                        }

                        Some((current_dependency_key, dependency_value))
                    } else {
                        None
                    };

                    pending_actions.insert(action.clone(), pending_action);
                }
                LoaderDependency::File(file) => {
                    // XX TODO: Keep?
                    assert!(!pending_files.contains_key(&file));

                    pending_files.insert(file.clone(), Some(current_dependency_key));
                }
            }
        }

        if !pending_actions.is_empty() || !pending_files.is_empty() {
            let mut graph = self.graph();

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
            for (path, potential) in pending_actions.into_iter().rev() {
                match potential {
                    Some((dependency_key, dependency_value)) => {
                        graph.set_action(path, dependency_key, dependency_value.clone());
                    }
                    None => {
                        graph.invalidate(&LoaderDependency::Action(path));
                    }
                };
            }
        }

        // XXX TODO: Avoid clone?
        self.graph().get(root_action)
    }

    pub(crate) fn register_dependencies_load(
        &self,
        path: &RootAssetRef,
        dependency_key: DependencyCacheKey,
        dependency_value: DependencyCacheValue,
    ) -> Option<ActionCacheKey> {
        let dependency_value = Arc::new(dependency_value);

        let action_key =
            self.graph()
                .set_action(path.clone(), dependency_key, dependency_value.clone());

        if let Some(cache) = &self.dependency_cache {
            cache.put(dependency_key, dependency_value, path);
        }

        action_key
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
