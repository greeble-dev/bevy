//! Basset proof of concept.

use crate::{
    basset::{
        cache::{
            ActionCacheKey, BassetHash, ContentCache, DependencyCacheKey, DependencyCacheValue,
            MemoryAndFileCache,
        },
        dependency_graph::DependencyGraph,
        publisher::{
            write_pack_file, ManifestPath, PublishInput, ReadablePackFile, StagedAsset,
            WritablePackFile,
        },
        standalone::{read_standalone_asset, write_standalone_asset},
    },
    io::{AssetReaderError, AssetSourceId, AssetSources},
    meta::{AssetActionMinimal, AssetMetaMinimal},
    Asset, AssetApp, AssetDependency, AssetServer, DeserializeMetaError,
};
use alloc::{borrow::ToOwned, boxed::Box, string::ToString, sync::Arc, vec::Vec};
use atomicow::CowArc;
use bevy_app::{App, Plugin};
use bevy_asset::{
    io::Reader,
    meta::{AssetAction, AssetHash, AssetMeta, AssetMetaDyn, Settings},
    saver::{AssetSaver, ErasedAssetSaver},
    AssetAction2, AssetLoader, AssetPath, AssetRef, ErasedAssetLoader, ErasedLoadedAsset,
    LoadContext, LoadedAsset,
};
use bevy_ecs::error::BevyError;
use bevy_platform::collections::{HashMap, HashSet};
use bevy_reflect::TypePath;
use bevy_tasks::{BoxedFuture, ConditionalSendFuture};
use core::{
    any::type_name,
    fmt::{Debug, Display},
    ops::Deref,
    result::Result,
};
use downcast_rs::{impl_downcast, Downcast};
use serde::{Deserialize, Serialize};
use std::{
    format,
    path::{Path, PathBuf},
    time::Instant,
};
use tracing::{debug, info, warn};

mod blob;
mod cache;
mod dependency_graph;
pub mod publisher;
mod standalone;

// XXX TODO: Is this necessary any more? Maybe just fold into `AssetPlugin`.
pub struct BassetPlugin;

impl Plugin for BassetPlugin {
    fn build(&self, app: &mut App) {
        app.register_erased_asset_loader(Box::new(BassetLoader));
    }
}

/// An `AssetPath` without a label.
//
// XXX TODO: Maybe think more about the name. "root asset" does match other parts
// of the asset system, but it's a bit ambiguous. E.g. publishing wants a list
// of "root assets", but they're the roots of the publishing tree.
#[derive(Eq, PartialEq, Ord, PartialOrd, Hash, Clone, Debug)]
pub struct RootAssetPath<'a> {
    source: AssetSourceId<'a>,
    path: CowArc<'a, Path>,
}

impl<'a> RootAssetPath<'a> {
    pub fn without_label(value: AssetPath<'a>) -> RootAssetPath<'a> {
        Self {
            // XXX TODO: Avoid clones?
            source: value.source().clone_owned(),
            path: CowArc::from(PathBuf::from(value.path())),
        }
    }

    pub fn source(&self) -> &AssetSourceId<'a> {
        &self.source
    }

    pub fn path(&self) -> &Path {
        self.path.deref()
    }
}

impl Display for RootAssetPath<'_> {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        // XXX TODO: Avoid conversion?
        Display::fmt(&AssetPath::from(self.clone()), f)
    }
}

impl<'a> Serialize for RootAssetPath<'a> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        // XXX TODO: Implement without using AssetPath?
        AssetPath::from(self.clone()).serialize(serializer)
    }
}

impl<'a, 'de> Deserialize<'de> for RootAssetPath<'a> {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        // XXX TODO: Implement without using AssetPath?
        Ok(RootAssetPath::try_from(AssetPath::deserialize(deserializer)?).expect("XXX TODO?"))
    }
}

impl From<RootAssetPath<'_>> for AssetPath<'static> {
    fn from(value: RootAssetPath) -> Self {
        // XXX TODO: Odd, no way to pass the path as a CowArc?
        Self::from_path_buf(PathBuf::from(&*value.path.into_owned()))
            .with_source(value.source.clone_owned())
    }
}

impl TryFrom<AssetPath<'_>> for RootAssetPath<'static> {
    type Error = (); // XXX TODO?

    fn try_from(value: AssetPath<'_>) -> Result<Self, Self::Error> {
        if value.label().is_some() {
            Err(())
        } else {
            Ok(Self::without_label(value.clone_owned())) // XXX TODO: Avoid clone?
        }
    }
}

/// An `AssetAction2` without a label.
#[derive(Eq, PartialEq, Ord, PartialOrd, Hash, Clone, Debug, Serialize, Deserialize)]
pub struct RootAssetAction2 {
    name: Box<str>,
    params: Box<ron::value::RawValue>,
}

impl RootAssetAction2 {
    pub fn without_label(value: AssetAction2<'_>) -> RootAssetAction2 {
        Self {
            name: value.name().into(),
            params: value.params().to_owned(),
        }
    }

    fn name(&self) -> &str {
        &self.name
    }

    fn params(&self) -> &ron::value::RawValue {
        &self.params
    }
}

impl Display for RootAssetAction2 {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        write!(f, "(name: {}, params: {})", self.name, self.params)
    }
}

impl From<RootAssetAction2> for AssetAction2<'static> {
    fn from(value: RootAssetAction2) -> Self {
        Self::new(value.name, value.params, None)
    }
}

impl TryFrom<AssetAction2<'_>> for RootAssetAction2 {
    type Error = (); // XXX TODO?

    fn try_from(value: AssetAction2<'_>) -> Result<Self, Self::Error> {
        if value.label().is_some() {
            Err(())
        } else {
            Ok(Self::without_label(value))
        }
    }
}

/// An `AssetRef` without a label.
#[derive(Eq, PartialEq, Ord, PartialOrd, Hash, Clone, Debug, Serialize, Deserialize)]
pub enum RootAssetRef<'a> {
    Path(RootAssetPath<'a>),
    Action(RootAssetAction2),
}

impl<'a> RootAssetRef<'a> {
    // XXX TODO: Consider a clone_without_label version - would avoid redundantly
    // cloning the label for the common case of `RootAssetRef::without_label(path.clone())`.
    pub fn without_label(value: AssetRef<'a>) -> RootAssetRef<'a> {
        match value {
            AssetRef::Path(path) => Self::Path(RootAssetPath::without_label(path)),
            AssetRef::Action(action) => Self::Action(RootAssetAction2::without_label(action)),
        }
    }

    pub fn path(&self) -> Option<&RootAssetPath<'a>> {
        if let Self::Path(path) = self {
            Some(path)
        } else {
            None
        }
    }

    pub fn action(&self) -> Option<&RootAssetAction2> {
        if let Self::Action(action) = self {
            Some(action)
        } else {
            None
        }
    }
}

impl Display for RootAssetRef<'_> {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        // XXX TODO: Should the string distinguish paths and actions?
        match self {
            Self::Path(path) => Display::fmt(path, f),
            Self::Action(action) => Display::fmt(action, f),
        }
    }
}

impl From<RootAssetRef<'_>> for AssetRef<'static> {
    fn from(value: RootAssetRef) -> Self {
        match value {
            RootAssetRef::Path(path) => Self::Path(path.into()),
            RootAssetRef::Action(action) => Self::Action(action.into()),
        }
    }
}

impl TryFrom<AssetRef<'_>> for RootAssetRef<'static> {
    type Error = (); // XXX TODO?

    fn try_from(value: AssetRef<'_>) -> Result<Self, Self::Error> {
        if value.label().is_some() {
            Err(())
        } else {
            match value {
                AssetRef::Path(path) => Ok(Self::from(RootAssetPath::try_from(path)?)),
                AssetRef::Action(action) => Ok(Self::from(RootAssetAction2::try_from(action)?)),
            }
        }
    }
}

impl From<RootAssetAction2> for RootAssetRef<'static> {
    fn from(value: RootAssetAction2) -> Self {
        Self::Action(value)
    }
}

impl<'a> From<RootAssetPath<'a>> for RootAssetRef<'a> {
    fn from(value: RootAssetPath<'a>) -> Self {
        Self::Path(value)
    }
}

/// XXX TODO: Document. Review where this overlaps with `LoadContext` and check
/// if they can be combined.
pub struct ApplyContext<'a> {
    asset_server: &'a AssetServer,

    // Equivalent to `ErasedLoadedAsset::loader_dependencies.
    loader_dependencies: HashMap<AssetRef<'static>, AssetHash>,
}

impl<'a> ApplyContext<'a> {
    pub fn new(asset_server: &'a AssetServer) -> Self {
        Self {
            asset_server,
            loader_dependencies: Default::default(),
        }
    }
}

impl ApplyContext<'_> {
    pub async fn erased_load_dependee_path(
        &mut self,
        path: &AssetPath<'static>,
        settings: &Option<Box<ron::value::RawValue>>,
    ) -> Result<ErasedLoadedAsset, BevyError> {
        // XXX TODO: Avoid clone?

        let (asset, _) = load_path(
            self.asset_server,
            &RootAssetPath::without_label(path.clone()),
            settings,
        )
        .await?;

        // XXX TODO: Remainder duplicates `erased_load_dependee`. Refactor?

        // XXX TODO: Decide what to do here. The hash only appears to be used
        // for asset processing, so maybe can ignore for now.
        let hash = [0u8; 32];

        self.loader_dependencies.insert(path.clone().into(), hash);

        asset
            .take_labeled(path.label_cow())
            .map_err(|_| format!("Couldn't find labeled asset \"{path:?}\".").into())
    }

    pub async fn erased_load_dependee(
        &mut self,
        path: &AssetRef<'static>,
    ) -> Result<ErasedLoadedAsset, BevyError> {
        // XXX TODO: Avoid clone?
        let asset = load_ref(
            self.asset_server,
            &RootAssetRef::without_label(path.clone()),
        )
        .await?;

        // XXX TODO: Decide what to do here. The hash only appears to be used
        // for asset processing, so maybe can ignore for now.
        let hash = [0u8; 32];

        self.loader_dependencies.insert(path.clone(), hash);

        asset
            .take_labeled(path.label_cow())
            .map_err(|_| format!("Couldn't find labeled asset \"{path:?}\".").into())
    }

    pub async fn load_dependee<T: Asset>(
        &mut self,
        path: &AssetRef<'static>,
    ) -> Result<T, BevyError> {
        match self.erased_load_dependee(path).await?.value.downcast::<T>() {
            Ok(result) => Ok(*result),
            Err(original) => panic!(
                "Should have made type {}, actually made type {}. Path: {path:?}",
                type_name::<T>(),
                original.asset_type_name(),
            ),
        }
    }

    pub async fn finish<A: Asset>(self, asset: A) -> ErasedLoadedAsset {
        let mut loaded_asset = LoadedAsset::new_with_dependencies(asset);

        loaded_asset.loader_dependencies = self.loader_dependencies;

        loaded_asset.into()
    }
}

/// XXX TODO: Review this. Duplicated from `bevy_asset::meta::Settings`.
pub trait BassetActionParams: Downcast + Send + Sync + 'static {}

// XXX TODO: Review this. Duplicated from `bevy_asset::meta::Settings`.
impl_downcast!(BassetActionParams);

// XXX TODO: Review this. Duplicated from `bevy_asset::meta::Settings`.
impl<T: 'static> BassetActionParams for T where T: Send + Sync {}

/// XXX TODO: Document.
///
/// An action that takes a parameter struct of a known type and returns an
/// `ErasedLoadedAsset`.
pub trait BassetAction: Send + Sync + 'static {
    type Params: BassetActionParams + Serialize + for<'a> Deserialize<'a>;

    /// XXX TODO: Document.
    type Error: Into<BevyError>;

    /// XXX TODO: Document.
    /// XXX TODO: Reconsider returning `ErasedLoadedAsset`. Currently the return
    /// value is required to go through `ApplyContext::finish`, but that can be
    /// sidestepped by constructing the `ErasedLoadedAsset` directly. Or maybe
    /// `ApplyContext::finish` should be reconsidered?
    fn apply(
        &self,
        context: ApplyContext<'_>,
        params: &Self::Params,
    ) -> impl ConditionalSendFuture<Output = Result<ErasedLoadedAsset, Self::Error>>;
}

pub trait ErasedBassetAction: Send + Sync + 'static {
    fn apply<'a>(
        &'a self,
        context: ApplyContext<'a>,
        params: &'a ron::value::RawValue,
    ) -> BoxedFuture<'a, Result<ErasedLoadedAsset, BevyError>>;
}

impl<T> ErasedBassetAction for T
where
    T: BassetAction + Send + Sync,
{
    fn apply<'a>(
        &'a self,
        context: ApplyContext<'a>,
        params: &'a ron::value::RawValue,
    ) -> BoxedFuture<'a, Result<ErasedLoadedAsset, BevyError>> {
        Box::pin(async move {
            let params = params.into_rust::<T::Params>().expect("TODO");

            T::apply(self, context, &params).await.map_err(Into::into)
        })
    }
}

pub struct DevelopmentActionSourceSettings {
    file_cache_path: Option<PathBuf>,
    validate_dependency_cache: bool,
    validate_action_cache: bool,
    action_type_name_to_action: HashMap<&'static str, Box<dyn ErasedBassetAction>>,
    asset_type_name_to_saver: HashMap<&'static str, (Box<dyn ErasedAssetSaver>, Box<dyn Settings>)>,
}

impl Default for DevelopmentActionSourceSettings {
    fn default() -> Self {
        let mut action_type_name_to_action =
            HashMap::<&'static str, Box<dyn ErasedBassetAction>>::new();

        // XXX TODO: Review if we should be doing this. Maybe should be added
        // afterwards so it's not exposed to the user. Maybe shouldn't even go
        // in here since it could be special cases?
        action_type_name_to_action
            .insert(type_name::<action::LoadPath>(), Box::new(action::LoadPath));

        Self {
            file_cache_path: Default::default(),
            validate_dependency_cache: Default::default(),
            validate_action_cache: Default::default(),
            action_type_name_to_action,
            asset_type_name_to_saver: Default::default(),
        }
    }
}

impl DevelopmentActionSourceSettings {
    pub fn with_file_cache_path(mut self, path: PathBuf) -> Self {
        self.file_cache_path = Some(path);
        self
    }

    pub fn with_validate_dependency_cache(mut self, value: bool) -> Self {
        self.validate_dependency_cache = value;
        self
    }

    pub fn with_validate_action_cache(mut self, value: bool) -> Self {
        self.validate_action_cache = value;
        self
    }

    pub fn with_action<T: BassetAction>(mut self, action: T) -> Self {
        self.action_type_name_to_action
            .insert(type_name::<T>(), Box::new(action));

        self
    }

    pub fn with_saver<T: AssetSaver>(mut self, saver: T) -> Self {
        // XXX TODO: Do we need anything more than default?
        let settings = T::Settings::default();

        self.asset_type_name_to_saver.insert(
            type_name::<T::Asset>(),
            (Box::new(saver), Box::new(settings)),
        );

        self
    }
}

// XXX TODO: Review if all this stuff actually needs to be shared.
#[derive(Default)]
pub struct DevelopmentActionSourceBuilder {
    settings: Arc<DevelopmentActionSourceSettings>,
}

impl DevelopmentActionSourceBuilder {
    pub fn new(settings: DevelopmentActionSourceSettings) -> Self {
        Self {
            settings: Arc::new(settings),
        }
    }
}

impl ActionSourceBuilder for DevelopmentActionSourceBuilder {
    fn build(&self, sources: Arc<AssetSources>) -> Arc<dyn ActionSource> {
        Arc::new(DevelopmentActionSource::new(self.settings.clone(), sources))
    }
}

// XXX TODO: Review if all this stuff actually needs to be shared.
pub(crate) struct DevelopmentActionSource {
    // XXX TODO: Keeping settings is kinda annoying, but it's the only way I can
    // see to avoid cloning things like `BassetSettings::asset_type_name_to_action'.
    // See also comment on `AssetPlugin::basset_settings`.
    settings: Arc<DevelopmentActionSourceSettings>,
    content_cache: ContentCache,
    dependency_graph: Option<DependencyGraph>,
    action_cache: Option<MemoryAndFileCache<ActionCacheKey, Arc<[u8]>>>,
}

impl DevelopmentActionSource {
    pub(crate) fn new(
        settings: Arc<DevelopmentActionSourceSettings>,
        sources: Arc<AssetSources>,
    ) -> Self {
        let dependency_graph = Some(DependencyGraph::new(
            settings
                .file_cache_path
                .as_ref()
                .map(|p| p.join("dependency")),
            settings.validate_dependency_cache,
        ));

        let action_cache = Some(MemoryAndFileCache::new(
            "action_cache",
            settings.file_cache_path.as_ref().map(|p| p.join("action")),
            settings.validate_action_cache,
        ));

        Self {
            settings,
            content_cache: ContentCache::new(sources),
            dependency_graph,
            action_cache,
        }
    }

    pub(crate) fn action<'a>(&'a self, type_name: &str) -> Option<&'a dyn ErasedBassetAction> {
        self.settings
            .action_type_name_to_action
            .get(type_name)
            .map(move |a| &**a)
    }

    pub(crate) fn saver<'a>(
        &'a self,
        type_name: &str,
    ) -> Option<(&'a dyn ErasedAssetSaver, &'a dyn Settings)> {
        self.settings
            .asset_type_name_to_saver
            .get(type_name)
            .map(move |s| (&*s.0, &*s.1))
    }

    pub(crate) async fn dependency_key(
        &self,
        path: &RootAssetRef<'static>,
        _settings: Option<&dyn Settings>,
    ) -> DependencyCacheKey {
        let mut hasher = blake3::Hasher::new();
        // XXX TODO: Double check that we're using the correct hash for actions.
        hasher.update(path.to_string().as_bytes());

        // XXX TODO: We should be including the settings in the dependency key. But
        // it requires some extra plumbing - `ErasedLoadedAsset` needs to keep
        // track of the settings of each item in `ErasedLoadedAsset::loader_dependencies`.
        /*
        if let Some(meta) = meta {
            hasher.update(&meta.serialize());
        }
        */

        if let Some(really_a_path) = path.path() {
            let content_hash = self
                .content_cache
                .get(really_a_path)
                .await
                .expect("XXX TODO");

            hasher.update(&content_hash.as_bytes());
        }

        // XXX TODO: Should include loader versions.

        DependencyCacheKey(BassetHash::new(*hasher.finalize().as_bytes()))
    }
}

impl ActionSource for DevelopmentActionSource {
    fn apply<'a>(
        &'a self,
        action: &'a RootAssetAction2,
        asset_server: &'a AssetServer,
    ) -> BoxedFuture<'a, Result<ErasedLoadedAsset, BevyError>> {
        Box::pin(async move {
            // XXX TODO: Avoid converting to a ref? There's a bunch of things that expect
            // an `AssetRef` but maybe could be specialized for an action.
            let path = RootAssetRef::from(action.clone());

            let apply_context = ApplyContext::new(asset_server);

            if let Some(action_cache) = &self.action_cache
                && let Some(dependency_graph) = &self.dependency_graph
            {
                // XXX TODO: Maybe early out here if there's no saver? Depends if we end
                // up in a situation where the saver has been compiled out but we still
                // want to read from the cache.

                let action_key = dependency_graph.action_key(&path, self).await;

                if let Some(action_key) = action_key
                    && let Some(cached_standalone_asset) =
                        action_cache.get(&action_key, &path).await
                {
                    return read_standalone_asset(&cached_standalone_asset, &apply_context).await;
                }
            }

            let asset = self
                .action(action.name())
                .ok_or_else(|| {
                    BevyError::from(format!("Couldn't find action \"{}\")", action.name()))
                })?
                .apply(apply_context, action.params())
                .await?;

            // XXX TODO: Support action outputs with sub-assets. Could be troublesome
            // as there's two potential cases:
            //
            // 1. The asset saver for the root asset expects to be given the sub-assets.
            // 2. The root asset and sub-assets should be saved by separate savers.
            assert!(
                asset.labeled_assets.is_empty(),
                "XXX TODO. Not supported yet. {action:?}",
            );

            // XXX TODO: Review logging. Bit spammy right now.
            /*
            if let Some(keys) = asset.keys.as_ref()
                && !keys.immediate_dependee_action_keys.is_empty()
            {
                info!(
                    "{:?}: Dependencies = {:?}",
                    path, keys.immediate_dependee_action_keys,
                );
            }
            */

            // XXX TODO: Is settings parameter correct?
            let action_key = if let Some(future) = self.register_dependencies(&path, None, &asset) {
                future.await
            } else {
                None
            };

            if let Some(action_key) = action_key {
                if let Some(action_cache) = &self.action_cache {
                    if let Some((saver, settings)) = self.saver(asset.asset_type_name()) {
                        // XXX TODO: Verify loader matches saver? Need a way to get
                        // `AssetSaver::OutputLoader` out of `ErasedAssetSaver`.
                        let loader = asset_server
                            .get_asset_loader_with_type_name(saver.loader_type_name())
                            .await
                            .expect("XXX TODO");

                        let blob =
                            write_standalone_asset(&asset, &*loader, saver, settings).await?;

                        action_cache.put(action_key, blob.into(), &path);
                    } else {
                        let type_name = asset.asset_type_name();
                        debug!(?type_name, ?path, "Cache ineligible, no saver for type.");
                    }
                }
            } else {
                warn!(?path, "Register dependencies did not return an action key.");
            }

            Ok(asset)
        })
    }

    // XXX TODO: Settings?
    fn register_dependencies<'a>(
        &'a self,
        path: &'a RootAssetRef<'static>,
        // XXX TODO: Settings?
        _settings: Option<&'a dyn Settings>,
        asset: &'a ErasedLoadedAsset,
    ) -> Option<BoxedFuture<'a, Option<ActionCacheKey>>> {
        Some(Box::pin(async move {
            if let Some(dependency_graph) = &self.dependency_graph {
                // XXX TODO: Recalculating the dependency key is a race condition since
                // the content cache will be looking at the file as it is now, not what was loaded.
                // Do we need to put the dependency key into `ErasedLoadedAsset`?
                let dependency_key = self.dependency_key(path, None).await;

                let mut unresolved_loader_dependees = asset
                    .loader_dependencies
                    .clone()
                    .into_iter()
                    .map(|(path, _)| RootAssetRef::without_label(path))
                    .collect::<Vec<_>>();

                // XXX TODO: We're duplicating logic in `DependencyCacheValue::new`.
                // But maybe this goes away if something external gives us the
                // dependency keys, so this function just takes a `DependencyCacheValue`.
                unresolved_loader_dependees.sort();
                unresolved_loader_dependees.dedup();

                // XXX TODO Duplicated in `load_action`.
                let mut loader_dependees =
                    Vec::<(RootAssetRef<'static>, DependencyCacheKey)>::new();

                for dependee_path in unresolved_loader_dependees {
                    // XXX TODO: Recalculating the dependency key is a race condition since
                    // the content cache will be looking at the file as it is now, not what was loaded.
                    // Do we need to put the dependency key into `ErasedLoadedAsset`?
                    let dependee_key = self.dependency_key(&dependee_path, None).await;
                    // XXX TODO: Review passing `None` for meta parameter.
                    loader_dependees.push((dependee_path, dependee_key));
                }

                let mut external_dependees = HashSet::<RootAssetRef>::new();

                asset.visit_dependencies(&mut |dependency| {
                    if let Some(path) = match dependency {
                        AssetDependency::Id(_) => todo!(
                            "Decide if we disallow ids. Dependency tracking requires the path."
                        ),
                        AssetDependency::Handle(handle) => handle.path().cloned(),
                        AssetDependency::Path(path) => Some(path.clone()),
                    } {
                        external_dependees.insert(RootAssetRef::without_label(path));
                    }
                });

                if !path.to_string().contains("embedded://") {
                    std::dbg!(&path, &external_dependees); // XXX DO NOT CHECK IN

                    if path.to_string() == "BoxTextured\\CesiumLogoFlat.png" {
                        panic!();
                    }
                }

                // XXX TODO: Are we account for sub-asset dependencies?

                let dependency_value = DependencyCacheValue::new(
                    loader_dependees.into_iter(),
                    external_dependees.into_iter(),
                );

                dependency_graph.register_dependencies(path, dependency_key, dependency_value)
            } else {
                None
            }
        }))
    }

    fn register_bytes_dependency<'a>(
        &'a self,
        path: &'a RootAssetRef<'static>,
    ) -> Option<BoxedFuture<'a, Option<ActionCacheKey>>> {
        Some(Box::pin(async move {
            if let Some(dependency_graph) = &self.dependency_graph {
                // XXX TODO: Recalculating the dependency key is a race condition since
                // the content cache will be looking at the file as it is now, not what was loaded.
                // Do we need to put the dependency key into `ErasedLoadedAsset`?
                let dependency_key = self.dependency_key(path, None).await;

                let dependency_value = DependencyCacheValue::empty();

                dependency_graph.register_dependencies(path, dependency_key, dependency_value)
            } else {
                None
            }
        }))
    }

    // XXX TODO: Less hacky debugging.
    fn dump_dependency_graph(&self) {
        self.dependency_graph
            .as_ref()
            .inspect(|g| info!("GRAPH DUMP\n{:?}", g));
    }

    fn publish<'a>(
        &'a self,
        input: PublishInput,
        asset_server: &'a AssetServer,
        pack_path: &'a Path,
    ) -> Option<BoxedFuture<'a, ()>> {
        Some(Box::pin(async move {
            let begin_time = Instant::now();

            std::dbg!(&input);

            let mut pack = WritablePackFile::default();

            // Assets that have already been published to `pack`.
            //
            // XXX TODO: Consider removing this and checking `pack` directly?
            // Kinda depends on multithread approaches.
            let mut done = HashSet::<RootAssetRef>::new();

            let mut input_stack = input
                .paths
                .iter()
                .map(|p| RootAssetRef::without_label(p.clone()))
                .collect::<Vec<_>>();

            while let Some(input_asset) = input_stack.pop() {
                // XXX TODO: Clone is annoying, but not sure it's possible to avoid
                // without doing a separate `contains` then `insert`?
                //
                // XXX TODO: Should we be checking done here or where we add stuff to
                // the input stack? Note that the latter would mean we have to de-dupe
                // `input`.
                if !done.insert(input_asset.clone()) {
                    continue;
                }

                match &input_asset {
                    RootAssetRef::Path(path) => {
                        // XXX TODO: Consider how we can avoid this load - it's only
                        // needed for discovering dependencies and getting the meta.
                        // Can we try and reuse the dependency graph?

                        // XXX TODO: Settings parameter?
                        let (loaded, loader) = load_path(asset_server, path, &None)
                            .await
                            .expect("XXX TODO");

                        // XXX TODO: Duplicated in `RootAssetRef::Action` case?
                        loaded.visit_dependencies(&mut |dependency| {
                            if let Some(path) = match dependency {
                                AssetDependency::Id(_) => todo!("Decide if we disallow ids. Dependency tracking requires the path."),
                                AssetDependency::Handle(handle) => handle.path().cloned(),
                                AssetDependency::Path(path) => Some(path.clone()),
                            } {
                                input_stack.push(RootAssetRef::without_label(path));
                            };
                        });

                        input_stack.extend(
                            loaded
                                .loader_dependencies
                                .keys()
                                .cloned()
                                .map(RootAssetRef::without_label),
                        );

                        let asset_bytes = {
                            let mut reader = asset_server
                                .get_source(path.source())
                                .expect("XXX TODO")
                                .reader()
                                .read(path.path())
                                .await
                                .expect("XXX TODO");
                            let mut bytes = Vec::new();
                            reader.read_to_end(&mut bytes).await.expect("XXX TODO");
                            bytes.into()
                        };

                        // XXX TODO: Sort out loader settings? See above where we call `load_path`.
                        let meta_bytes = loader.default_meta().serialize().into_boxed_slice();

                        // XXX TODO: The action case somewhat duplicates this. Refactor?

                        pack.paths.insert(
                            ManifestPath::from(path),
                            StagedAsset {
                                asset_bytes,
                                meta_bytes,
                            },
                        );
                    }
                    RootAssetRef::Action(action) => {
                        // XXX TODO: Can this handle actions that can't be saved?

                        // XXX TODO: Can this read directly out of the action cache? We're
                        // mostly replicating what that's already done. Maybe the standalone
                        // files can be organized in such a way that we can grab `meta_bytes`
                        // and `action_bytes` directly. Or maybe there's better options for
                        // copying the cache.

                        let loaded = asset_server
                            .basset_action_source()
                            .apply(action, asset_server)
                            .await
                            .expect("XXX TODO");

                        // XXX TODO: Decide if we try to support the original path.
                        let fake_path =
                            AssetPath::parse("ERROR - Standalone assets shouldn't use their path");

                        // XXX TODO: Duplicates where `load_action` writes to the cache.
                        let (saver, saver_settings) =
                            self.saver(loaded.asset_type_name()).expect("XXX TODO");

                        let loader = asset_server
                            .get_asset_loader_with_type_name(saver.loader_type_name())
                            .await
                            .expect("XXX TODO");

                        let meta_bytes = loader.default_meta().serialize();

                        let mut asset_bytes = Vec::<u8>::new();
                        {
                            saver
                                .save(&mut asset_bytes, &loaded, saver_settings, fake_path)
                                .await
                                .expect("XXX TODO");
                        }

                        pack.actions.insert(
                            Box::<str>::from(action.to_string()),
                            StagedAsset {
                                asset_bytes: asset_bytes.into(),
                                meta_bytes: meta_bytes.into(),
                            },
                        );

                        loaded.visit_dependencies(&mut |dependency| {
                            if let Some(path) = match dependency {
                                AssetDependency::Id(_) => todo!("Decide if we disallow ids. Dependency tracking requires the path."),
                                AssetDependency::Handle(handle) => handle.path().cloned(),
                                AssetDependency::Path(path) => Some(path.clone()),
                            } {
                                input_stack.push(RootAssetRef::without_label(path));
                            };
                        });

                        // XXX TODO: We're not accounting for the standalone asset having
                        // loader dependencies. Not sure if we can work them out unless
                        // we do a fake load?
                    }
                }

                info!(%input_asset, "Publishing");
            }

            info!("Writing pack file {pack_path:?}");

            write_pack_file(pack, pack_path).await;

            info!(
                "Publishing finished in {:.2}s",
                begin_time.elapsed().as_secs_f32()
            );
        }))
    }
}

#[derive(Serialize, Deserialize, Debug, Clone)]
struct BassetFileSerializable {
    // XXX TODO: Consider renaming, could get confused with "root asset" versus "sub-asset".
    root: AssetRef<'static>,
    // TODO: Versioning?
}

#[derive(TypePath)]
struct BassetLoader;

impl ErasedAssetLoader for BassetLoader {
    fn load<'a>(
        &'a self,
        reader: &'a mut dyn Reader,
        _settings: &'a dyn Settings,
        load_context: LoadContext<'a>,
    ) -> BoxedFuture<'a, Result<ErasedLoadedAsset, BevyError>> {
        assert!(
            load_context.path().label().is_none(),
            "Paranoia. {:?}",
            load_context.path()
        );

        Box::pin(async move {
            let mut bytes = Vec::new();
            reader.read_to_end(&mut bytes).await?;

            let basset = ron::de::from_bytes::<BassetFileSerializable>(&bytes)?;

            let root_without_label = RootAssetRef::without_label(basset.root.clone());

            let asset = load_ref(load_context.asset_server(), &root_without_label)
                .await
                .map(|mut asset| {
                    // XXX TODO: See other cases of ignoring the hash.
                    let hash = [0u8; 32];

                    // XXX TODO: Justify this dependency replacement.
                    asset.loader_dependencies.clear();
                    asset.loader_dependencies.insert(basset.root.clone(), hash);

                    asset
                })?;

            asset
                .take_labeled(basset.root.label_cow())
                .map_err(|_| format!("Couldn't find labeled asset \"{:?}\".", &basset.root).into())
        })
    }

    fn extensions(&self) -> &[&str] {
        &["basset"]
    }

    fn deserialize_meta(&self, meta: &[u8]) -> Result<Box<dyn AssetMetaDyn>, DeserializeMetaError> {
        // XXX TODO: Review. Is this going to be problematic since we don't know the loader type?
        let meta = AssetMeta::<FakeAssetLoader, ()>::deserialize(meta)?;
        Ok(Box::new(meta))
    }

    fn default_meta(&self) -> Box<dyn AssetMetaDyn> {
        // XXX TODO: Review. Is this going to be problematic since we don't know the loader type?
        Box::new(AssetMeta::<FakeAssetLoader, ()>::new(AssetAction::Load {
            loader: self.type_path().to_string(),
            settings: <FakeAssetLoader as AssetLoader>::Settings::default(),
        }))
    }

    fn type_path(&self) -> &'static str {
        <Self as TypePath>::type_path()
    }

    fn type_id(&self) -> core::any::TypeId {
        core::any::TypeId::of::<Self>()
    }

    fn asset_type_name(&self) -> Option<&'static str> {
        None
    }

    fn asset_type_id(&self) -> Option<core::any::TypeId> {
        None
    }

    fn meta_from_settings(
        &self,
        settings: &[u8],
    ) -> Result<Box<dyn AssetMetaDyn>, DeserializeMetaError> {
        // XXX TODO: Review. Is this going to be problematic since we don't know the loader type?
        Ok(Box::new(AssetMeta::<FakeAssetLoader, ()>::new(
            AssetAction::Load {
                loader: self.type_path().to_string(),
                settings: ron::de::from_bytes(settings)?,
            },
        )))
    }
}

async fn load_ref(
    asset_server: &AssetServer,
    path: &RootAssetRef<'static>,
) -> Result<ErasedLoadedAsset, BevyError> {
    match path {
        RootAssetRef::Path(path) => load_path(asset_server, path, &None).await.map(|(l, _)| l), // XXX TODO: Settings?
        RootAssetRef::Action(action) => {
            asset_server
                .basset_action_source()
                .apply(action, asset_server)
                .await
        }
    }
}

// XXX TODO: Review how we're returning the loader. Only used by publishing.
pub(crate) async fn load_path(
    asset_server: &AssetServer,
    path: &RootAssetPath<'static>,
    settings: &Option<Box<ron::value::RawValue>>,
) -> Result<(ErasedLoadedAsset, Arc<dyn ErasedAssetLoader>), BevyError> {
    // XXX TODO: Try to avoid?
    let full_path = AssetPath::from(path.clone());

    let (mut meta, loader, mut reader) = asset_server
        .get_meta_loader_and_reader(&full_path, None)
        .await
        .map_err(Into::<BevyError>::into)?;

    if let Some(settings) = settings {
        meta = loader.meta_from_settings(settings.get_ron().as_bytes())?;
    }

    // Roughly the same as LoadContext::load_direct_internal.

    let load_dependencies = false;
    let populate_hashes = false;
    let update_dependency_cache = true;

    let asset = asset_server
        .load_with_settings_loader_and_reader(
            // XXX TODO: Avoid clone?
            &AssetPath::from(path.clone()),
            meta.loader_settings().expect("meta is set to Load"),
            &*loader,
            &mut *reader,
            load_dependencies,
            populate_hashes,
            update_dependency_cache,
        )
        .await
        .map_err(Into::<BevyError>::into)?;

    Ok((asset, loader.clone()))
}

// TODO: Review this. Awkward hack so that `BassetLoader::default_meta` and
// `BassetLoader::deserialize_meta` can return a meta even if it's wrong.
#[derive(TypePath)]
struct FakeAssetLoader;

impl AssetLoader for FakeAssetLoader {
    type Asset = ();
    type Settings = ();
    type Error = std::io::Error;

    async fn load(
        &self,
        _reader: &mut dyn Reader,
        _settings: &Self::Settings,
        _load_context: &mut LoadContext<'_>,
    ) -> Result<Self::Asset, Self::Error> {
        Ok(())
    }
}

// XXX TODO: Review name? Does have some similarities to `AssetSource` but not
// that much. `ActionHandler`?
//
// XXX TODO: Should be `pub`?
pub trait ActionSource: Send + Sync + 'static {
    fn apply<'a>(
        &'a self,
        action: &'a RootAssetAction2,
        // XXX TODO: Review and see if we can use something narrower than the
        // entire asset server.
        asset_server: &'a AssetServer,
    ) -> BoxedFuture<'a, Result<ErasedLoadedAsset, BevyError>>;

    // XXX TODO: Try and avoid exposing this. It's currently only for development
    // mode to fill out the dependency graph from `AssetServer::load_with_settings_loader_and_reader`.
    //
    // XXX TODO: Review return type. Returning an option is a faff, but avoids
    // redundantly created a boxed future it's a noop.
    fn register_dependencies<'a>(
        &'a self,
        _path: &'a RootAssetRef<'static>,
        _settings: Option<&'a dyn Settings>,
        _asset: &'a ErasedLoadedAsset,
    ) -> Option<BoxedFuture<'a, Option<ActionCacheKey>>> {
        None
    }

    // Registers a dependency on the given file, which is only loaded as raw
    // bytes - not through an `AssetLoader`. This means the file itself cannot
    // have dependencies.
    //
    // XXX TODO: Try to avoid exposing this, same as `register_dependencies`.
    //
    // XXX TODO: Review return type. Returning an option is a faff, but avoids
    // redundantly created a boxed future it's a noop.
    fn register_bytes_dependency<'a>(
        &'a self,
        _path: &'a RootAssetRef<'static>,
    ) -> Option<BoxedFuture<'a, Option<ActionCacheKey>>> {
        None
    }

    // XXX TODO: Less hacky debugging.
    fn dump_dependency_graph(&self) {
        // XXX TODO?
        tracing::error!("dump_dependency_graph not implemented");
    }

    // XXX TODO: Should this be here?
    fn publish<'a>(
        &'a self,
        _input: PublishInput,
        _asset_server: &'a AssetServer,
        _pack_path: &'a Path,
    ) -> Option<BoxedFuture<'a, ()>> {
        todo!("XXX TODO: What should this do by default?")
    }
}

pub trait ActionSourceBuilder: Send + Sync + 'static {
    fn build(&self, sources: Arc<AssetSources>) -> Arc<dyn ActionSource>;
}

// XXX TODO: Still needed?
#[expect(unused, reason = "XXX TODO")]
pub(crate) struct NullActionSourceBuilder;

impl ActionSourceBuilder for NullActionSourceBuilder {
    fn build(&self, _sources: Arc<AssetSources>) -> Arc<dyn ActionSource> {
        Arc::new(NullActionSource)
    }
}

pub(crate) struct NullActionSource;

impl ActionSource for NullActionSource {
    fn apply<'a>(
        &'a self,
        _action: &RootAssetAction2,
        // XXX TODO: Review and see if we can use something narrower than the
        // entire asset server.
        _asset_server: &AssetServer,
    ) -> BoxedFuture<'a, Result<ErasedLoadedAsset, BevyError>> {
        Box::pin(async move { todo!("XXX TODO? Return error?") })
    }
}

pub struct PublishedActionSourceBuilder {
    pub pack_file: Arc<ReadablePackFile>,
}

impl PublishedActionSourceBuilder {
    pub fn new(pack_file: Arc<ReadablePackFile>) -> Self {
        Self { pack_file }
    }
}

impl ActionSourceBuilder for PublishedActionSourceBuilder {
    fn build(&self, _sources: Arc<AssetSources>) -> Arc<dyn ActionSource> {
        Arc::new(PublishedActionSource {
            pack_file: self.pack_file.clone(),
        })
    }
}

struct PublishedActionSource {
    pack_file: Arc<ReadablePackFile>,
}

impl ActionSource for PublishedActionSource {
    fn apply<'a>(
        &'a self,
        action: &'a RootAssetAction2,
        asset_server: &'a AssetServer,
    ) -> BoxedFuture<'a, Result<ErasedLoadedAsset, BevyError>> {
        Box::pin(async move {
            // XXX TODO: Review how we should be referencing assets. Maybe want
            // a distinct type for safety?
            let action_string = action.to_string();

            // XXX TODO: The below duplicates a fair amount of `read_standalone_asset`.
            // Refactor?

            let Ok(mut readers) = self.pack_file.action(&action_string) else {
                // XXX TODO: Review. We need to support at least some level of
                // fallback to applying actions, since we have embedded assets
                // that use `load_with_settings` (e.g. SmaaPlugin). Not sure if
                // this should be special cased or generalized.
                return if &*action.name == type_name::<action::LoadPath>() {
                    let load_path = action::LoadPath;

                    let apply_context = ApplyContext::new(asset_server);

                    ErasedBassetAction::apply(&load_path, apply_context, &action.params).await
                } else {
                    // XXX TODO?
                    Err(AssetReaderError::NotFound(action.to_string().into()).into())
                };
            };

            let mut meta_bytes = Vec::<u8>::new();
            readers.meta.read_to_end(&mut meta_bytes).await?;

            let minimal_meta =
                ron::de::from_bytes::<AssetMetaMinimal>(&meta_bytes).expect("XXX TODO");

            let loader_name = match &minimal_meta.asset {
                AssetActionMinimal::Load { loader } => loader.as_str(),
                _ => todo!("XXX TODO"),
            };

            let loader = asset_server
                .get_asset_loader_with_type_name(loader_name)
                .await
                .expect("XXX TODO");

            let meta = loader.deserialize_meta(&meta_bytes).expect("XXX TODO");

            let load_dependencies = true;
            let populate_hashes = false;

            // We're in published mode, so no need to update the dependency cache.
            //
            // XXX TODO: Review. We're making a fragile assumption. Should this value
            // even matter if the dependency cache isn't available?
            let update_dependency_cache = false;

            // XXX TODO: Ew? Need to decide if we try to support the original path.
            let fake_path = AssetPath::parse("ERROR - Standalone assets shouldn't use their path");

            Ok(asset_server
                .load_with_settings_loader_and_reader(
                    &fake_path,
                    meta.loader_settings().expect("meta is set to Load"),
                    &*loader,
                    &mut readers.asset,
                    load_dependencies,
                    populate_hashes,
                    update_dependency_cache,
                )
                .await?)
        })
    }
}

pub mod action {
    use super::*;
    use alloc::string::String;

    pub struct LoadPath;

    #[derive(Serialize, Deserialize, Default)]
    pub struct LoadPathParams {
        // XXX TODO: Should be RootAssetPath? Avoiding for now to simplify lifetimes and defaults.
        pub path: String,
        #[serde(default)]
        pub loader_settings: Option<Box<ron::value::RawValue>>,
        // XXX TODO?
        //loader_name: Option<String>,
    }

    impl BassetAction for LoadPath {
        type Params = LoadPathParams;
        type Error = BevyError;

        async fn apply(
            &self,
            mut context: ApplyContext<'_>,
            params: &Self::Params,
        ) -> Result<ErasedLoadedAsset, Self::Error> {
            // XXX TODO: Try to avoid clones? But will mean changing lifetimes
            // of `BassetAction::apply`.
            let path = AssetPath::parse(&params.path).into_owned();

            // XXX TODO: Selecting a subasset should be done by the `AssetAction2::label`,
            // not here. How do we make this more robust?
            assert!(path.label().is_none());

            let settings = params.loader_settings.clone();

            // XXX TODO: Need to think through dependencies - I don't think this is
            // right.
            //
            // First, inside `erased_load_dependee_path` there's a call to `load_path`,
            // which will call `load_with_settings_loader_and_reader`, which will register
            // the file in the dependency tree _without_ settings. Should it even do this?
            //
            // Second, `erased_load_dependee_path` then registers the file (again without settings)
            // as a loader dependency in `ApplyContext`, but we don't call `ApplyContext::finish`,
            // so it's never used. This means the file the action is loading is not actually
            // registered as a loader dependency of this action. That seems wrong - it means
            // this action has no dependencies in the loader dependency graph.
            //
            // Suspect that we need to distinguish between bytes-only dependencies and
            // those that go through a loader. So technically the path we're loading
            // here is a bytes-only dependency, and any dependencies is has when actually
            // loaded become this action's loader dependencies.
            context.erased_load_dependee_path(&path, &settings).await

            // XXX TODO: Note that we're not calling `context.finish()`. Probably
            // correct but double check.
        }
    }
}
