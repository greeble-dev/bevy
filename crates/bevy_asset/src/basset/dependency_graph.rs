use crate::{
    basset::{
        cache::{
            ActionCacheKey, BassetHash, CacheLoaderDependency, ContentCache, DependencyCacheKey,
            DependencyCacheValue, MemoryAndFileCache,
        },
        FullEnvironment, RootAssetPath, RootAssetRef,
    },
    io::{AssetSources, Reader, VecReader},
    LoaderDependency, ReadAssetBytesError,
};
use alloc::{boxed::Box, string::ToString, sync::Arc, vec, vec::Vec};
use bevy_platform::collections::HashMap;
use bevy_reflect::TypeRegistryArc;
use core::{
    fmt::{Debug, Display, Write},
    hash::{Hash, Hasher},
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

#[derive(Clone, PartialEq, Eq, Debug)]
struct KnownAssetState {
    node_index: NodeIndex,
    action_key: ActionCacheKey,
    dependency_key: DependencyCacheKey,
    dependency_value: Option<Arc<DependencyCacheValue>>,
}

#[derive(Clone, PartialEq, Eq, Debug)]
enum AssetState {
    Unknown,
    Known(KnownAssetState),
}

#[derive(Default)]
struct InternalGraph {
    graph: Acyclic<StableDiGraph<LoaderDependency, ()>>,
    action_to_state: HashMap<RootAssetRef, AssetState>,
    file_to_state: HashMap<RootAssetPath<'static>, AssetState>,
}

impl InternalGraph {
    fn get_asset_state(&self, dependency: &LoaderDependency) -> Option<AssetState> {
        match dependency {
            LoaderDependency::Action(action) => self.action_to_state.get(action).cloned(),
            LoaderDependency::File(file) => self.file_to_state.get(file).cloned(),
        }
    }

    fn set_action(
        &mut self,
        path: RootAssetRef,
        dependency_key: DependencyCacheKey,
        dependency_value: Arc<DependencyCacheValue>,
    ) -> Option<ActionCacheKey> {
        // XXX TODO: Review for correctness. We want an early out here because
        // uncached actions like `LoadPath` will be applied multiple times even
        // if they don't change.
        if let Some(AssetState::Known(existing_state)) = self.action_to_state.get(&path)
            && (existing_state.dependency_key == dependency_key)
        {
            return Some(existing_state.action_key);
        }

        // XXX TODO: Optimize? This repeats the state lookup we just did. Or merge into
        // one `invalidate_file_if_different` function.
        self.invalidate_action(&path);

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
            .map(|CacheLoaderDependency(dependee, dependee_dependency_key)| {
                if let Some(dependee_state) = self.get_asset_state(dependee) {
                    match dependee_state {
                        AssetState::Known(dependee_state) => {
                            // XXX TODO: Should go behind validation flag?
                            assert_eq!(*dependee_dependency_key, dependee_state.dependency_key);
                            Some((dependee_state.node_index, dependee_state.action_key))
                        }
                        AssetState::Unknown => None,
                    }
                }
                else {
                    // XXX TODO: Change to error? Or make logging opt-in.
                    warn!(?dependee, %dependee_dependency_key, "Failed to find state - were dependencies not registered for this asset?");
                    None
                }
            })
            .collect::<Option<Vec<(NodeIndex, ActionCacheKey)>>>();

        if let Some(resolved) = resolved {
            // We found nodes for all dependees. Create our node and link it to
            // the dependees.

            let action_key = ActionCacheKey::new(
                dependency_key,
                resolved
                    .iter()
                    .map(|(_, k)| *k)
                    .collect::<Vec<_>>()
                    .as_slice(),
            );

            let node_id = self.graph.add_node(LoaderDependency::Action(path.clone()));

            for (dependee_node_id, _) in resolved.iter() {
                self.graph
                    .try_add_edge(node_id, *dependee_node_id, ())
                    .expect("Oops, cycle. XXX TODO");
            }

            self.action_to_state.insert(
                path,
                AssetState::Known(KnownAssetState {
                    node_index: node_id,
                    action_key,
                    dependency_key,
                    dependency_value: Some(dependency_value),
                }),
            );

            Some(action_key)
        } else {
            // At least one dependency was not in the graph, so we remain unknown.
            // Our state should have been set to unknown by the call to invalidate
            // near the top.
            assert_eq!(self.action_to_state.get(&path), Some(&AssetState::Unknown));

            None
        }
    }

    // XXX TODO: Document and reconsider name. Corresponds to `LoaderDependency::File`.
    // XXX TODO: Duplicates a lot of `set_load`. Try refactor?
    fn set_file(
        &mut self,
        path: RootAssetPath<'static>,
        dependency_key: DependencyCacheKey,
    ) -> Option<ActionCacheKey> {
        if let Some(AssetState::Known(existing_state)) = self.file_to_state.get(&path)
            && (existing_state.dependency_key == dependency_key)
        {
            return Some(existing_state.action_key);
        }

        // XXX TODO: Optimize? This repeats the state lookup we just did. Or merge into
        // one `invalidate_file_if_different` function.
        self.invalidate_file(&path);

        let action_key = ActionCacheKey::new(dependency_key, &[]);

        let node_id = self.graph.add_node(LoaderDependency::File(path.clone()));

        self.file_to_state.insert(
            path,
            AssetState::Known(KnownAssetState {
                node_index: node_id,
                action_key,
                dependency_key,
                dependency_value: None,
            }),
        );

        Some(action_key)
    }

    fn get_action(
        &self,
        path: &RootAssetRef,
    ) -> Option<(ActionCacheKey, Option<Arc<DependencyCacheValue>>)> {
        self.action_to_state
            .get(path)
            .and_then(|state| match state {
                AssetState::Known(known) => {
                    Some((known.action_key, known.dependency_value.clone()))
                }
                AssetState::Unknown => None,
            })
    }

    fn contains(&self, path: &LoaderDependency) -> bool {
        match path {
            LoaderDependency::Action(action) => self.action_to_state.contains_key(action),
            LoaderDependency::File(file) => self.file_to_state.contains_key(file),
        }
    }

    fn invalidate_action(&mut self, path: &RootAssetRef) {
        // XXX TODO: There's a couple of minor optimizations to make here although
        // they might not be worth the complexity. Firstly, if the action has
        // no dependencies then we could set it straight to `Unknown` and skip
        // the graph searching. Secondly, We can avoid looking up the initial action
        // twice - once to get the initial node index near the top, and one to set it#
        // to `Unknown` near the bottom.
        let Some(initial_node_index) =
            self.action_to_state
                .get(path)
                .and_then(|state| match state {
                    AssetState::Known(state) => Some(state.node_index),
                    _ => None,
                })
        else {
            return;
        };

        let mut stack = vec![initial_node_index];
        let mut cursor = 0;

        while let Some(node_index) = stack.get(cursor) {
            cursor += 1;

            stack.extend(
                self.graph
                    .edges_directed(*node_index, Direction::Incoming)
                    .map(|edge| edge.source()),
            );
        }

        // XXX TODO: Check if there's a more efficient way to bulk remove.
        for node_index in stack.into_iter() {
            let path = self.graph.remove_node(node_index).expect("XXX TODO");

            match path {
                LoaderDependency::Action(action) => {
                    *self.action_to_state.get_mut(&action).expect("XXX TODO") = AssetState::Unknown;
                }
                LoaderDependency::File(file) => {
                    *self.file_to_state.get_mut(&file).expect("XXX TODO") = AssetState::Unknown;
                }
            }
        }
    }

    fn invalidate_file(&mut self, path: &RootAssetPath<'static>) {
        if let Some(state) = self.file_to_state.get_mut(path)
            && let AssetState::Known(known_state) = state
        {
            self.graph
                .remove_node(known_state.node_index)
                .expect("XXX TODO");

            *state = AssetState::Unknown;
        }
    }
}

impl Debug for InternalGraph {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
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
            .collect::<Vec<_>>();

        let mut stack = Vec::<(NodeIndex, usize)>::new();

        for root_node_index in root_nodes.into_iter() {
            // Skip spammy embedded assets. XXX TODO: Rethink at some point.
            if let LoaderDependency::Action(path) =
                self.graph.node_weight(root_node_index).expect("XXX TODO")
                && path.to_string().contains("embedded://")
            {
                continue;
            }

            stack.push((root_node_index, 0));
        }

        while let Some((node_id, depth)) = stack.pop() {
            for _ in 0..(depth + 1) {
                f.write_str("    ")?;
            }

            let path = self.graph.node_weight(node_id).expect("XXX TODO");
            let AssetState::Known(state) = self.get_asset_state(path).expect("XXX TODO") else {
                unreachable!("XXX TODO");
            };

            f.write_str("+-- ")?;
            Display::fmt(&state.action_key, f)?;
            f.write_char('/')?;
            Display::fmt(&state.dependency_key, f)?;
            f.write_char(' ')?;
            Debug::fmt(&self.graph.node_weight(node_id).expect("XXX TODO"), f)?;

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

// Partially implements the `core::hash::Hasher` interface by wrapping
// `blake3::Hasher`. Does *not* support `Hasher::finish`, since that returns
// a `u64` when we want the full 32-byte hash. Instead, call `Blake3Wrapper::finish`.
//
// XXX TODO: Reconsider name? Maybe should be just `BassetHasher`?
struct Blake3Wrapper(blake3::Hasher);

impl Blake3Wrapper {
    fn new() -> Self {
        Self(blake3::Hasher::new())
    }
    fn finish(self) -> BassetHash {
        BassetHash::new(*self.0.finalize().as_bytes())
    }
}

impl Hasher for Blake3Wrapper {
    fn finish(&self) -> u64 {
        unimplemented!("Use Blake3Wrapper::finish");
    }

    fn write(&mut self, bytes: &[u8]) {
        self.0.update(bytes);
    }
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

    pub(crate) async fn dependency_key(
        &self,
        path: &LoaderDependency,
        env: &FullEnvironment,
    ) -> DependencyCacheKey {
        match path {
            LoaderDependency::Action(action) => self.action_dependency_key(action, env),
            LoaderDependency::File(path) => self.file_dependency_key(path).await,
        }
    }

    pub(crate) fn action_dependency_key(
        &self,
        action: &RootAssetRef,
        env: &FullEnvironment,
    ) -> DependencyCacheKey {
        // XXX TODO: Can we can optimize this by combining environment filtering
        // with hashing?
        let filtered_env = env.filter(action.action()).expect("XXX TODO");

        // XXX TODO: Seed hash? Also consider using separate seeds for actions
        // and files just in case.
        // XXX TODO: Review choice of `blake3`.
        let mut hasher = Blake3Wrapper::new();

        Hash::hash(&action, &mut hasher);
        Hash::hash(action.action().version(), &mut hasher);
        Hash::hash(&filtered_env, &mut hasher);

        DependencyCacheKey(hasher.finish())
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
        env: &FullEnvironment,
    ) -> Option<(ActionCacheKey, Option<Arc<DependencyCacheValue>>)> {
        let Some(cache) = &self.dependency_cache else {
            return None;
        };

        let root_dependency_key =
            root_dependency_key.unwrap_or_else(|| self.action_dependency_key(root_action, env));

        // Early out if possible.
        if let Some(existing) = self.graph().get_action(root_action) {
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

            let current_dependency_key = self.dependency_key(&dependency, env).await;

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
                        graph.invalidate_file(&path);
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
                        graph.invalidate_action(&path);
                    }
                };
            }
        }

        // XXX TODO: Avoid clone?
        self.graph().get_action(root_action)
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
