use crate::{
    basset::{
        cache::{ActionCacheKey, BassetHash, ContentCache, ContentHash},
        ActionDependencies, ApplyContext, DependencyLoading, ErasedBassetAction,
        ErasedBassetActionFunction, FullEnvironment, MissingActionFunctionError, RootAssetPath,
        RootAssetRef,
    },
    io::AssetSources,
    AssetLoadError, AssetServer, LoaderDependency,
};
use alloc::{boxed::Box, format, string::ToString, sync::Arc, vec, vec::Vec};
use bevy_platform::collections::HashMap;
use core::{
    fmt::{Debug, Display, Write},
    hash::Hasher,
};
use indexmap::IndexMap;
use petgraph::{
    acyclic::Acyclic, data::Build, graph::NodeIndex, prelude::StableDiGraph, visit::EdgeRef,
    Direction,
};
use std::sync::{Mutex, MutexGuard, PoisonError};
use tracing::warn;

#[derive(Clone, PartialEq, Eq, Debug)]
struct KnownAssetState {
    node_index: NodeIndex,
    action_key: ActionCacheKey,
    // XXX TODO: This is not needed for file states. Consider separate state types
    // for actions and files.
    value_dependees: Vec<LoaderDependency>,
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
        env: &FullEnvironment,
        value_dependees: Vec<LoaderDependency>,
    ) -> Option<ActionCacheKey> {
        // XXX TODO: Review for correctness. We want an early out here because
        // uncached actions like `LoadPath` will be applied multiple times even
        // if they don't change.
        if let Some(AssetState::Known(existing_state)) = self.action_to_state.get(&path) {
            return Some(existing_state.action_key);
        }

        // Gather the node id and action key of each dependee, returning `None`
        // if any are absent or unknown.
        //
        // XXX TODO: Check that the ordering of the output is reliable, since the
        // ordering affects the action key.
        //
        // XXX TODO: More efficient if we could do `into_iter?`
        let resolved = value_dependees
            .iter()
            .map(|dependee| {
                if let Some(dependee_state) = self.get_asset_state(dependee) {
                    match dependee_state {
                        AssetState::Known(dependee_state) => {
                            Some((dependee_state.node_index, dependee_state.action_key))
                        }
                        AssetState::Unknown => None,
                    }
                } else {
                    // XXX TODO: Change to error? Or make logging opt-in.
                    warn!(
                        ?dependee,
                        "Failed to find state - were dependencies not registered for this asset?"
                    );
                    None
                }
            })
            .collect::<Option<Vec<(NodeIndex, ActionCacheKey)>>>();

        if let Some(resolved) = resolved {
            // We found nodes for all dependees. Create our node and link it to
            // the dependees.

            let action_key = ActionCacheKey::from_action(
                &path,
                env,
                resolved
                    .iter()
                    .map(|(_, k)| *k)
                    .collect::<Vec<_>>()
                    .as_slice(),
            );

            // XXX TODO: Verify node doesn't already exist?
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
                    value_dependees,
                }),
            );

            Some(action_key)
        } else {
            // At least one dependency was not in the graph, so we remain unknown.
            // Our state should have been set to unknown by the call to invalidate
            // near the top.
            //
            // XXX TODO: This assertion can fail if we hit the "failed to find
            // state" warning above. Review and see if we have a logic bug, or
            // maybe that warning should be an error and we never reach here.
            assert_eq!(self.action_to_state.get(&path), Some(&AssetState::Unknown));

            None
        }
    }

    fn set_file(
        &mut self,
        path: RootAssetPath<'static>,
        content_hash: ContentHash,
    ) -> Option<ActionCacheKey> {
        // XXX TODO: Review for correctness. Possible race condition?
        if let Some(AssetState::Known(existing_state)) = self.file_to_state.get(&path) {
            return Some(existing_state.action_key);
        }

        let action_key = ActionCacheKey::from_file(&path, content_hash);

        let node_id = self.graph.add_node(LoaderDependency::File(path.clone()));

        self.file_to_state.insert(
            path,
            AssetState::Known(KnownAssetState {
                node_index: node_id,
                action_key,
                value_dependees: Vec::new(),
            }),
        );

        Some(action_key)
    }

    fn get_action(&self, path: &RootAssetRef) -> Option<(ActionCacheKey, Vec<LoaderDependency>)> {
        self.action_to_state
            .get(path)
            .and_then(|state| match state {
                AssetState::Known(known) => Some((known.action_key, known.value_dependees.clone())),
                AssetState::Unknown => None,
            })
    }

    fn contains(&self, path: &LoaderDependency) -> bool {
        match path {
            LoaderDependency::Action(action) => self.action_to_state.contains_key(action),
            LoaderDependency::File(file) => self.file_to_state.contains_key(file),
        }
    }

    #[expect(dead_code, reason = "XXX TODO")]
    fn invalidate_node(&mut self, initial_node: NodeIndex) {
        // XXX TODO: Could optimize this by first checking if the node has no
        // incoming dependencies, in which case it can be set straight to `Unknown`
        // without constructing a stack.
        let mut stack = vec![initial_node];
        let mut cursor = 0;

        while let Some(node) = stack.get(cursor) {
            cursor += 1;

            stack.extend(
                self.graph
                    .edges_directed(*node, Direction::Incoming)
                    .map(|edge| edge.source()),
            );
        }

        // XXX TODO: Check if there's a more efficient way to bulk remove.
        for node in stack.into_iter() {
            let path = self.graph.remove_node(node).expect("XXX TODO");

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

    #[expect(dead_code, reason = "XXX TODO")]
    fn invalidate_action(&mut self, path: &RootAssetRef) {
        // XXX TODO: `invalidate_node` repeats this lookup when setting the
        // state to `Unknown`. Could avoid the double lookup, but would that add
        // too much complication?
        if let Some(node) = self
            .action_to_state
            .get(path)
            .and_then(|state| match state {
                AssetState::Known(state) => Some(state.node_index),
                _ => None,
            })
        {
            self.invalidate_node(node);
        }
    }

    #[expect(dead_code, reason = "XXX TODO")]
    fn invalidate_file(&mut self, path: &RootAssetPath<'static>) {
        // XXX TODO: `invalidate_node` repeats this lookup when setting the
        // state to `Unknown`. Could avoid the double lookup, but would that add
        // too much complication?
        if let Some(node) = self.file_to_state.get(path).and_then(|state| match state {
            AssetState::Known(state) => Some(state.node_index),
            _ => None,
        }) {
            self.invalidate_node(node);
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
    content_cache: ContentCache,
    // XXX TODO: We should have loader versions here for calculating dependency keys?
}

// Partially implements the `core::hash::Hasher` interface by wrapping
// `blake3::Hasher`. Does *not* support `Hasher::finish`, since that returns
// a `u64` when we want the full 32-byte hash. Instead, call `Blake3Wrapper::finish`.
//
// XXX TODO: Reconsider name? Maybe should be just `BassetHasher`?
pub(crate) struct Blake3Wrapper(pub(crate) blake3::Hasher);

impl Blake3Wrapper {
    pub(crate) fn new() -> Self {
        Self(blake3::Hasher::new())
    }

    pub(crate) fn finish(self) -> BassetHash {
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
    pub(crate) fn new(sources: Arc<AssetSources>) -> Self {
        Self {
            graph: Default::default(),
            content_cache: ContentCache::new(sources),
        }
    }

    fn graph(&self) -> MutexGuard<'_, InternalGraph> {
        self.graph.lock().unwrap_or_else(PoisonError::into_inner)
    }

    // XXX TODO: Avoid duplication. This was cut and pasted from `DevelopmentActionSource`.
    fn action_function<'a>(
        action: &ErasedBassetAction,
        action_type_path_to_action_function: &'a HashMap<
            &'static str,
            Box<dyn ErasedBassetActionFunction>,
        >,
    ) -> Result<&'a dyn ErasedBassetActionFunction, MissingActionFunctionError> {
        let type_path = action.type_path();

        action_type_path_to_action_function
            .get(type_path)
            .map(AsRef::as_ref)
            .ok_or(MissingActionFunctionError::NotFound(type_path))
    }

    fn equivalent(l: &[LoaderDependency], r: &[LoaderDependency]) -> bool {
        l.iter().all(|li| r.contains(li)) && r.iter().all(|ri| l.contains(ri))
    }

    async fn action_value_dependees(
        action: &RootAssetRef,
        env: &FullEnvironment,
        asset_server: &AssetServer,
        action_type_path_to_action_function: &HashMap<
            &'static str,
            Box<dyn ErasedBassetActionFunction>,
        >,
    ) -> Vec<LoaderDependency> {
        // XXX TODO: Expose this as a global setting.
        let verify = false;

        if !verify && let ActionDependencies::Known(known) = action.action().dependencies() {
            return known;
        }

        let filtered_env = env
            .filter(action.action())
            .map_err(|err| AssetLoadError::TodoError(Arc::new(format!("{err:?}").into())))
            .expect("XXX TODO");

        let apply_context = ApplyContext::new(
            action.clone(),
            asset_server,
            DependencyLoading::No,
            filtered_env,
            None,
        );

        let action_function =
            Self::action_function(action.action(), action_type_path_to_action_function)
                .expect("XXX TODO");

        let output = action_function
            .apply(apply_context, action)
            .await
            .expect("XXX TODO");

        let value_dependees = output.value_dependees();

        if verify
            && let ActionDependencies::Known(known) = action.action().dependencies()
            && !Self::equivalent(&known, &value_dependees)
        {
            // XXX TODO: Proper error handling.
            panic!("Found dependencies differed from declared dependencies.\nAction: {action:?}\nFound: {:?}\nDeclared: {known:?}", value_dependees);
        }

        value_dependees
    }

    pub(crate) async fn action_key(
        &self,
        root_action: &RootAssetRef,
        env: &FullEnvironment,
        asset_server: &AssetServer,
        // XXX TODO: Try and avoid passing this in, or wrap in it something
        // friendlier.
        action_type_path_to_action_function: &HashMap<
            &'static str,
            Box<dyn ErasedBassetActionFunction>,
        >,
    ) -> Option<(ActionCacheKey, Vec<LoaderDependency>)> {
        // Early out if possible.
        if let Some(existing) = self.graph().get_action(root_action) {
            return Some(existing);
        };

        let mut stack = Vec::<LoaderDependency>::new();

        // XXX TODO: Document that `IndexMap` is for consistent ordering.
        let mut pending_actions = IndexMap::<RootAssetRef, Vec<LoaderDependency>>::new();

        // XXX TODO: Document that `IndexSet` is for consistent ordering.
        let mut pending_files = IndexMap::<RootAssetPath<'static>, ContentHash>::new();

        stack.push(LoaderDependency::Action(root_action.clone()));

        while let Some(action) = stack.pop() {
            // Skip if we've already hit this dependency.
            if match &action {
                LoaderDependency::Action(action) => pending_actions.contains_key(action),
                LoaderDependency::File(file) => pending_files.contains_key(file),
            } {
                continue;
            }

            // Skip if we already have a node for this dependency.
            if self.graph().contains(&action) {
                continue;
            };

            match action {
                LoaderDependency::Action(action) => {
                    let value_dependees = Self::action_value_dependees(
                        &action,
                        env,
                        asset_server,
                        action_type_path_to_action_function,
                    )
                    .await;

                    stack.extend(value_dependees.clone());

                    // XXX TODO: Review. This invariant is held by a check earlier
                    // in the loop, but maybe it would be better to do it here?
                    assert!(!pending_actions.contains_key(&action));

                    pending_actions.insert(action, value_dependees);
                }
                LoaderDependency::File(file) => {
                    // XXX TODO: Review. This invariant is held by a check earlier
                    // in the loop, but maybe it would be better to do it here?
                    assert!(!pending_files.contains_key(&file));

                    // XXX TODO: Race condition. We don't know if the file read
                    // by the content cache matches the file read by the loader
                    // that registered this dependency. Probably needs the loader
                    // to give us the hash. Note that `LoadContext::read_asset_bytes`
                    // already does something similar for regular publishing - see
                    // also `LoadedAsset::loader_dependencies`.
                    let content_hash = self.content_cache.get(&file).await.expect("XXX TODO");

                    pending_files.insert(file, content_hash);
                }
            }
        }

        if !pending_actions.is_empty() || !pending_files.is_empty() {
            let mut graph = self.graph();

            // XXX TODO: Document reverse order reasoning.
            for (file, content_hash) in pending_files.into_iter().rev() {
                graph.set_file(file, content_hash);
            }

            // XXX TODO: Document reverse order reasoning.
            for (action, value_dependees) in pending_actions.into_iter().rev() {
                graph.set_action(action, env, value_dependees);
            }
        }

        // XXX TODO: This lookup could be optimized away since we know we did
        // the `set_action` for this action somewhere above. But is that worth
        // the complication?
        self.graph().get_action(root_action)
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
