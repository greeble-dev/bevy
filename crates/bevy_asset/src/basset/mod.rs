//! Basset proof of concept.

use crate::{
    basset::{
        cache::{
            ActionCacheKey, BassetHash, ContentCache, DependencyCacheKey, DependencyCacheValue,
            MemoryAndFileCache,
        },
        dependency_graph::DependencyGraph,
        standalone::{read_standalone_asset, write_standalone_asset},
    },
    Asset, AssetApp, AssetContainer, AssetServer, DeserializeMetaError, LabeledAsset,
    UntypedAssetId,
};
use alloc::{
    // XXX TODO: Review if `std` imports should be `alloc`/`core`.
    boxed::Box,
    string::ToString,
    sync::Arc,
    vec::Vec,
};
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
use bevy_platform::collections::HashMap;
use bevy_reflect::TypePath;
use bevy_tasks::{BoxedFuture, ConditionalSendFuture};
use core::{fmt::Debug, result::Result};
use downcast_rs::{impl_downcast, Downcast};
use serde::{Deserialize, Serialize};
use std::{format, path::PathBuf};
use tracing::{debug, warn};

mod blob;
mod cache;
mod dependency_graph;
mod standalone;

pub struct BassetPlugin;

impl Plugin for BassetPlugin {
    fn build(&self, app: &mut App) {
        app.register_erased_asset_loader(Box::new(BassetLoader))
            .register_erased_asset_loader(Box::new(ActionLoader));
    }
}

/// XXX TODO: Document. Review where this overlaps with `LoadContext` and check
/// if they can be combined.
pub struct ApplyContext<'a> {
    asset_server: &'a AssetServer,

    // Equivalent to `ErasedLoadedAsset::loader_dependencies.
    loader_dependencies: HashMap<AssetRef<'static>, AssetHash>,
}

// XXX TODO: Oof, that name.
pub struct PartialErasedLoadedLabeledAsset {
    pub(crate) value: Box<dyn AssetContainer>,
    #[expect(unused, reason = "XXX TODO?")]
    pub(crate) id: UntypedAssetId,
}

// XXX TODO: Duplicates a lot of `PartialErasedLoadedAsset` and `ErasedLoadedAsset`.
impl PartialErasedLoadedLabeledAsset {
    pub fn get<A: Asset>(&self) -> Option<&A> {
        self.value.downcast_ref::<A>()
    }

    pub fn downcast<A: Asset>(mut self) -> Result<A, PartialErasedLoadedLabeledAsset> {
        match self.value.downcast::<A>() {
            Ok(value) => Ok(*value),
            Err(value) => {
                self.value = value;
                Err(self)
            }
        }
    }
}

impl From<LabeledAsset> for PartialErasedLoadedLabeledAsset {
    fn from(value: LabeledAsset) -> Self {
        Self {
            value: value.asset.value,
            id: value.handle.id(),
        }
    }
}

// Similar to `ErasedLoadedAsset`, but stores sub-assets as a distinct `PartialErasedLoadedLabeledAsset`
// type. This avoids the awkwardly recursive nature of `ErasedLoadedAsset` storing
// sub-assets as `ErasedLoadedAsset`.
pub struct PartialErasedLoadedAsset {
    pub(crate) value: Box<dyn AssetContainer>,
    pub(crate) labeled_assets: Vec<PartialErasedLoadedLabeledAsset>,
    #[expect(unused, reason = "XXX TODO?")]
    pub(crate) label_to_asset_index: HashMap<CowArc<'static, str>, usize>,
    pub(crate) asset_id_to_asset_index: HashMap<UntypedAssetId, usize>,
}

// XXX TODO: Duplicates a lot of `ErasedLoadedAsset`.
impl PartialErasedLoadedAsset {
    pub fn get<A: Asset>(&self) -> Option<&A> {
        self.value.downcast_ref::<A>()
    }

    pub fn downcast<A: Asset>(mut self) -> Result<A, PartialErasedLoadedAsset> {
        match self.value.downcast::<A>() {
            Ok(value) => Ok(*value),
            Err(value) => {
                self.value = value;
                Err(self)
            }
        }
    }

    pub fn get_labeled_by_id(
        &self,
        id: UntypedAssetId,
    ) -> Option<&PartialErasedLoadedLabeledAsset> {
        self.asset_id_to_asset_index
            .get(&id)
            .map(|&index| &self.labeled_assets[index])
    }
}

impl From<ErasedLoadedAsset> for PartialErasedLoadedAsset {
    fn from(value: ErasedLoadedAsset) -> Self {
        let labeled_assets = value
            .labeled_assets
            .into_iter()
            .map(PartialErasedLoadedLabeledAsset::from)
            .collect();

        Self {
            value: value.value,
            labeled_assets,
            label_to_asset_index: value.label_to_asset_index,
            asset_id_to_asset_index: value.asset_id_to_asset_index,
        }
    }
}

impl ApplyContext<'_> {
    pub async fn erased_load_dependee(
        &mut self,
        path: &AssetRef<'static>,
    ) -> Result<PartialErasedLoadedAsset, BevyError> {
        let asset = load_ref(self.asset_server, &path.without_label().into_owned()).await?;

        // XXX TODO: Decide what to do here. The hash only appears to be used
        // for asset processing, so maybe can ignore for now.
        let hash = [0u8; 32];

        self.loader_dependencies.insert(path.clone(), hash);

        if let Some(label) = path.label_cow() {
            match asset.take_labeled(label.clone()) {
                Ok(labeled_asset) => Ok(labeled_asset.into()),
                // XXX TODO
                Err(_) => panic!("Couldn't find label \"{}\" in \"{}\".", label, path),
            }
        } else {
            Ok(asset.into())
        }
    }

    // XXX TODO: Review where this duplicates `erased_load_dependee`.
    pub async fn erased_load_dependee_path(
        &mut self,
        path: &AssetPath<'static>,
        settings: &Option<Box<ron::value::RawValue>>,
    ) -> Result<PartialErasedLoadedAsset, BevyError> {
        let asset = load_path(self.asset_server, path, settings).await?;

        // XXX TODO: Decide what to do here. The hash only appears to be used
        // for asset processing, so maybe can ignore for now.
        let hash = [0u8; 32];

        // XXX TODO: Duplicates earlier `path.clone().into()`.
        self.loader_dependencies.insert(path.clone().into(), hash);

        if let Some(label) = path.label_cow() {
            match asset.take_labeled(label.clone()) {
                Ok(labeled_asset) => Ok(labeled_asset.into()),
                // XXX TODO
                Err(_) => panic!("Couldn't find label \"{}\" in \"{}\".", label, path),
            }
        } else {
            Ok(asset.into())
        }
    }

    pub async fn load_dependee<T: Asset>(
        &mut self,
        path: &AssetRef<'static>,
    ) -> Result<T, BevyError> {
        match self.erased_load_dependee(path).await?.downcast::<T>() {
            Ok(result) => Ok(result),
            Err(original) => panic!(
                "Should have made type {}, actually made type {}. Path: {:?}",
                core::any::type_name::<T>(),
                original.value.asset_type_name(),
                path,
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
/// XXX TODO: Why doesn't this implement serialize/deserialize?
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
    /// XXX TODO: Document. Review if serialize traits should go into `BassetActionParams`.
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

// XXX TODO: Review if this actually needs to be shared between `BassetLoader`
// and `ActionLoader`.
#[derive(Default)]
pub struct BassetShared {
    action_type_name_to_action: HashMap<&'static str, Box<dyn ErasedBassetAction>>,
    asset_type_name_to_saver: HashMap<&'static str, (Box<dyn ErasedAssetSaver>, Box<dyn Settings>)>,
    content_cache: ContentCache,
    dependency_graph: Option<DependencyGraph>,
    action_cache: Option<MemoryAndFileCache<ActionCacheKey, Arc<[u8]>>>,
}

impl BassetShared {
    pub fn new(
        file_cache_path: Option<PathBuf>,
        validate_dependency_cache: bool,
        validate_action_cache: bool,
    ) -> Self {
        Self {
            action_type_name_to_action: Default::default(),
            asset_type_name_to_saver: Default::default(),
            content_cache: Default::default(),
            dependency_graph: Some(DependencyGraph::new(
                file_cache_path.as_ref().map(|p| p.join("dependency")),
                validate_dependency_cache,
            )),
            action_cache: Some(MemoryAndFileCache::new(
                "action_cache",
                file_cache_path.as_ref().map(|p| p.join("action")),
                validate_action_cache,
            )),
        }
    }

    pub fn with_action<T: BassetAction>(mut self, action: T) -> Self {
        self.action_type_name_to_action
            .insert(core::any::type_name::<T>(), Box::new(action));

        self
    }

    pub fn with_saver<T: AssetSaver>(mut self, saver: T) -> Self {
        let settings = T::Settings::default();

        self.asset_type_name_to_saver.insert(
            core::any::type_name::<T::Asset>(),
            (Box::new(saver), Box::new(settings)),
        );

        self
    }

    pub(crate) fn action<'a>(&'a self, type_name: &str) -> Option<&'a dyn ErasedBassetAction> {
        self.action_type_name_to_action
            .get(type_name)
            .map(move |a| &**a)
    }

    pub(crate) fn saver<'a>(
        &'a self,
        type_name: &str,
    ) -> Option<(&'a dyn ErasedAssetSaver, &'a dyn Settings)> {
        self.asset_type_name_to_saver
            .get(type_name)
            .map(move |s| (&*s.0, &*s.1))
    }

    pub(crate) async fn dependency_key(
        &self,
        path: &AssetRef<'static>,
        _settings: Option<&dyn Settings>,
        asset_server: &AssetServer,
    ) -> DependencyCacheKey {
        let mut hasher = blake3::Hasher::new();

        // XXX TODO: Is this wrong for actions and paths with a label? Yes if the cache
        // holds the full asset - the label selects after getting from the cache. No if
        // the cache holds the sub-asset. Or should this function just disallow
        // paths with labels?
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
                .get(really_a_path, asset_server)
                .await
                .expect("XXX TODO");

            hasher.update(&content_hash.as_bytes());
        }

        // XXX TODO: Loader version.

        DependencyCacheKey(BassetHash::new(*hasher.finalize().as_bytes()))
    }

    // XXX TODO: Check if this should take `AssetRef` or `AssetPath`.
    // XXX TODO: Settings?
    pub(crate) async fn register_dependencies(
        &self,
        path: &AssetRef<'static>,
        _settings: Option<&dyn Settings>,
        dependees: impl Iterator<Item = AssetRef<'static>>,
        asset_server: &AssetServer,
    ) {
        if let Some(dependency_graph) = &self.dependency_graph {
            let shared = asset_server.basset_shared();

            // XXX TODO: Recalculating the dependency key is a race condition since
            // the content cache will be looking at the file as it is now, not what was loaded.
            // Do we need to put the dependency key into `ErasedLoadedAsset`?
            let dependency_key = shared.dependency_key(path, None, asset_server).await;

            // XXX TODO Duplicated in `load_action`.
            let mut dependencies = Vec::<(AssetRef<'static>, DependencyCacheKey)>::new();

            for dependency_path in dependees {
                // XXX TODO: Review passing `None` for meta parameter.
                dependencies.push((
                    dependency_path.clone(),
                    // XXX TODO: Recalculating the dependency key is a race condition since
                    // the content cache will be looking at the file as it is now, not what was loaded.
                    // Do we need to put the dependency key into `ErasedLoadedAsset`?
                    shared
                        .dependency_key(&dependency_path, None, asset_server)
                        .await,
                ));
            }

            let dependency_value = DependencyCacheValue::new(dependencies.into_iter());

            dependency_graph.register_dependencies(path, dependency_key, dependency_value);
        }
    }
}

#[derive(Serialize, Deserialize, Debug, Clone)]
struct BassetFileSerializable {
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

            load_ref(load_context.asset_server(), &basset.root)
                .await
                .map(|mut asset| {
                    // XXX TODO: See other cases of ignoring the hash.
                    let hash = [0u8; 32];

                    // XXX TODO: Justify this dependency replacement.
                    asset.loader_dependencies.clear();
                    asset.loader_dependencies.insert(basset.root.clone(), hash);

                    asset
                })
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
}

#[derive(TypePath)]
struct ActionLoader;

impl ErasedAssetLoader for ActionLoader {
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
            assert_eq!(
                reader.read_to_end(&mut Vec::new()).await?,
                0,
                "Reader should have been empty, all data is in the path."
            );

            let action = load_context.path().action().expect(
                "ActionLoader should have been given an AssetRef::Action, not an AssetRef::Path.",
            );

            load_action(load_context.asset_server(), action).await
        })
    }

    fn extensions(&self) -> &[&str] {
        &[]
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
}

fn apply_settings(settings: Option<&mut dyn Settings>, ron: &Option<Box<ron::value::RawValue>>) {
    let Some(_settings) = settings else {
        return;
    };

    let Some(_ron) = ron else {
        return;
    };

    // XXX TODO: Need some way to apply a dyn Settings without knowing the type.
    todo!();
    /*
    if let Some(settings) = settings.downcast_mut::<demo::StringAssetSettings>() {
        *settings = ron
            .clone()
            .into_rust::<demo::StringAssetSettings>()
            .expect("TODO");
    }
    */
}

async fn load_ref(
    asset_server: &AssetServer,
    path: &AssetRef<'static>,
) -> Result<ErasedLoadedAsset, BevyError> {
    match path {
        AssetRef::Path(path) => load_path(asset_server, path, &None).await, // XXX TODO: Settings?
        AssetRef::Action(action) => load_action(asset_server, action).await,
    }
}

async fn load_path(
    asset_server: &AssetServer,
    path: &AssetPath<'static>,
    settings: &Option<Box<ron::value::RawValue>>,
) -> Result<ErasedLoadedAsset, BevyError> {
    assert!(
        path.label().is_none(),
        "Labels should not be handled here. {path:?}",
    );

    let (mut meta, loader, mut reader) = asset_server
        // XXX TODO `for_path` or `for_action`?
        .get_meta_loader_and_reader_for_path(path, None)
        .await
        .map_err(Into::<BevyError>::into)?;

    apply_settings(meta.loader_settings_mut(), settings);

    // Roughly the same as LoadContext::load_direct_internal.

    let load_dependencies = false;
    let populate_hashes = false;
    let update_dependency_cache = true;

    let asset = asset_server
        .load_with_settings_loader_and_reader(
            &AssetRef::from(path),
            meta.loader_settings().expect("meta is set to Load"),
            &*loader,
            &mut *reader,
            load_dependencies,
            populate_hashes,
            update_dependency_cache,
        )
        .await
        .map_err(Into::<BevyError>::into)?;

    Ok(asset)
}

async fn load_action(
    asset_server: &AssetServer,
    action: &AssetAction2<'static>,
) -> Result<ErasedLoadedAsset, BevyError> {
    assert!(
        action.label().is_none(),
        "Labels should not be handled here. {action:?}",
    );

    // XXX TODO: Avoid? There's a bunch of things that expect an `AssetRef` but
    // maybe could be specialized for an action.
    let path = AssetRef::from(action.clone());

    let shared = asset_server.basset_shared().clone();

    let apply_context = ApplyContext {
        asset_server,
        loader_dependencies: HashMap::default(),
    };

    if let Some(action_cache) = &shared.action_cache
        && let Some(dependency_graph) = &shared.dependency_graph
    {
        // XXX TODO: Maybe early out here if there's no saver? Depends if we end
        // up in a situation where the saver has been compiled out but we still
        // want to read from the cache.

        let action_key = dependency_graph
            .action_key(&path, &shared, asset_server)
            .await;

        if let Some(action_key) = action_key
            && let Some(cached_standalone_asset) = action_cache.get(&action_key, &path).await
        {
            return read_standalone_asset(&path, &cached_standalone_asset, &apply_context).await;
        }
    }

    let asset = shared
        .action(action.name())
        .ok_or_else(|| BevyError::from(format!("Couldn't find action \"{}\")", action.name())))?
        .apply(apply_context, action.params())
        .await?;

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

    if let Some(action_cache) = &shared.action_cache
        && let Some(dependency_graph) = &shared.dependency_graph
        && let Some((saver, settings)) = shared.saver(asset.asset_type_name())
    {
        // XXX TODO: Review duplication. We already tried the dependency key earlier.
        // XXX TODO: Review passing `None` for meta parameter.
        // XXX TODO: Recalculating the dependency key is a race condition since
        // the content cache will be looking at the file as it is now, not what was loaded.
        let dependency_key = shared.dependency_key(&path, None, asset_server).await;

        // XXX TODO: Sub-asset dependencies?
        // XXX TODO: Generally awkward.
        let mut dependencies = Vec::<(AssetRef<'static>, DependencyCacheKey)>::new();

        for (dependency_path, _) in &asset.loader_dependencies {
            // XXX TODO: Review passing `None` for meta parameter.
            dependencies.push((
                dependency_path.clone(),
                // XXX TODO: Recalculating the dependency key is a race condition since
                // the content cache will be looking at the file as it is now, not what was loaded.
                // Do we need to put the dependency key into `ErasedLoadedAsset`?
                shared
                    .dependency_key(dependency_path, None, asset_server)
                    .await,
            ));
        }

        let dependency_value = DependencyCacheValue::new(dependencies.into_iter());

        let action_key = dependency_graph.register_dependencies(
            &path, // XXX TODO: Avoid clone?
            dependency_key,
            dependency_value,
        );

        if let Some(action_key) = action_key {
            // XXX TODO: Verify loader matches saver? Need a way to get
            // `AssetSaver::OutputLoader` out of `ErasedAssetSaver`.
            let loader = asset_server
                .get_asset_loader_with_type_name(saver.loader_type_name())
                .await
                .expect("TODO");

            let blob = write_standalone_asset(&asset, &*loader, saver, settings).await?;

            action_cache.put(action_key, blob.into(), &path);
        } else {
            warn!(
                ?path,
                %dependency_key,
                "Register dependencies did not return an action key."
            );
        }
    } else {
        let type_name = asset.asset_type_name();
        debug!(?type_name, ?path, "Cache ineligible, no saver for type.");
    }

    Ok(asset)
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
