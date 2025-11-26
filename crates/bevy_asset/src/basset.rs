//! Basset proof of concept.

#![expect(dead_code, reason = "TODO")]

use bevy_app::{App, Plugin};
use bevy_asset::{
    io::Reader,
    io::SliceReader,
    meta::{AssetAction, AssetHash, AssetMeta, AssetMetaDyn, Settings},
    saver::{AssetSaver, ErasedAssetSaver},
    AssetAction2, AssetLoader, AssetPath, AssetRef, AsyncWriteExt, DeserializeMetaError,
    ErasedAssetLoader, ErasedLoadedAsset, LoadContext, LoadedAsset,
};
use bevy_ecs::error::BevyError;
use bevy_platform::{
    collections::HashMap,
    sync::{PoisonError, RwLock},
};
use bevy_tasks::{BoxedFuture, ConditionalSendFuture, IoTaskPool};

use async_fs::File;
use atomicow::CowArc;
use core::{
    fmt::{Debug, Display},
    result::Result,
};
use downcast_rs::{impl_downcast, Downcast};
use serde::{Deserialize, Serialize};
use tracing::{debug, warn};

// XXX TODO: Review if `std` imports should be `alloc`/`core`.
use alloc::{
    boxed::Box,
    string::{String, ToString},
    sync::Arc,
    vec,
    vec::Vec,
};
use core::{hash::Hash, marker::PhantomData};
use std::{format, io::Write, path::PathBuf};

use crate::{Asset, AssetApp, AssetContainer, AssetServer, LabeledAsset, UntypedAssetId};

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

    shared: &'a BassetShared,
}

// XXX TODO: Oof, that name.
pub struct PartialErasedLoadedLabeledAsset {
    pub(crate) value: Box<dyn AssetContainer>,
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

pub struct PartialErasedLoadedAsset {
    pub(crate) value: Box<dyn AssetContainer>,
    pub(crate) labeled_assets: HashMap<CowArc<'static, str>, PartialErasedLoadedLabeledAsset>,
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
        self.labeled_assets.values().find(|a| a.id == id)
    }
}

impl From<ErasedLoadedAsset> for PartialErasedLoadedAsset {
    fn from(value: ErasedLoadedAsset) -> Self {
        let mut labeled_assets = HashMap::new();

        for (label, value) in value.labeled_assets.into_iter() {
            labeled_assets.insert(label, Into::<PartialErasedLoadedLabeledAsset>::into(value));
        }

        Self {
            value: value.value,
            labeled_assets,
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
    dependency_cache: Option<MemoryAndFileCache<DependencyCacheKey, Arc<DependencyCacheValue>>>,
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
            dependency_cache: Some(MemoryAndFileCache::new(
                "dependency_cache",
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

    // XXX TODO: The meta should be required, not optional. Or we need another
    // way to make sure the key incorporates the loader settings.
    pub(crate) async fn dependency_key(
        &self,
        path: &AssetRef<'static>,
        _meta: Option<&dyn AssetMetaDyn>,
        asset_server: &AssetServer,
    ) -> DependencyCacheKey {
        let mut hasher = blake3::Hasher::new();

        // XXX TODO: Is this wrong for actions and paths with a label? Yes if the cache
        // holds the full asset - the label selects after getting from the cache. No if
        // the cache holds the sub-asset.
        hasher.update(path.to_string().as_bytes());

        // XXX TODO: We should be including the meta in the dependency key. But
        // it requires some extra plumbing - `ErasedLoadedAsset` needs to keep
        // track of the meta of each item in `ErasedLoadedAsset::loader_dependencies`.
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
                .expect("TODO");

            hasher.update(&content_hash.as_bytes());
        }

        // XXX TODO: Loader version?

        DependencyCacheKey(BassetHash::new(*hasher.finalize().as_bytes()))
    }

    pub(crate) fn action_key(
        dependency_key: &DependencyCacheKey,
        immediate_dependee_action_keys: &HashMap<AssetRef<'static>, ActionCacheKey>,
    ) -> ActionCacheKey {
        let mut hasher = blake3::Hasher::new();

        // XXX TODO: Action key also needs loader version.
        // XXX TODO: Anything else?

        hasher.update(&dependency_key.as_hash().as_bytes());

        // XXX TODO: How are we stabilising the order of dependencies?

        for immediate_dependee_action_key in immediate_dependee_action_keys.values() {
            hasher.update(&immediate_dependee_action_key.as_hash().as_bytes());
        }

        let result = ActionCacheKey(BassetHash::new(*hasher.finalize().as_bytes()));

        std::dbg!(&result, &dependency_key, &immediate_dependee_action_keys);

        result
    }

    // XXX TODO: Should take the dependency key directly. Right now we're looking up
    // the content key inside this function, but we don't know if that's the same
    // content that the caller used.
    pub(crate) async fn register_dependees(
        &self,
        path: &AssetRef<'static>,
        meta: Option<&dyn AssetMetaDyn>,
        dependees: impl Iterator<Item = AssetRef<'static>>,
        asset_server: &AssetServer,
    ) {
        if let Some(dependency_cache) = &self.dependency_cache {
            let key = self.dependency_key(path, meta, asset_server).await;

            let value = DependencyCacheValue::new(dependees);

            if should_log(path) {
                debug!(%key, ?path, ?value, "Register dependees.");
            }

            dependency_cache.put(key, Arc::new(value), path).await;
        }
    }

    /*
    pub(crate) async fn loaded_asset_keys(
        &self,
        path: &AssetRef<'static>,
        immediate_dependee_action_keys: &HashMap<AssetRef<'static>, ActionCacheKey>,
        asset_server: &AssetServer,
    ) -> LoadedAssetKeys {
        let dependency_key = self.dependency_key(path, asset_server).await;

        let action_key = BassetShared::action_key(&dependency_key, immediate_dependee_action_keys);

        // XXX TODO: Check if cloning and collecting can be reduced.
        let dependency_list = DependencyList {
            list: immediate_dependee_action_keys.keys().cloned().collect(),
        };

        // XXX TODO: Is this the right place for the cache put?
        if let Some(dependency_cache) = &self.dependency_cache {
            dependency_cache.put(dependency_key, Arc::new(dependency_list), path);
        }

        LoadedAssetKeys {
            dependency_key,
            action_key,
            // XXX TODO: Parameter should be by value to avoid clone here?
            immediate_dependee_action_keys: immediate_dependee_action_keys.clone(),
        }
    }
    */
}

#[derive(Serialize, Deserialize, Debug, Clone)]
struct BassetFileSerializable {
    root: AssetRef<'static>,
    // TODO: Versioning?
}

struct BassetLoader;

impl ErasedAssetLoader for BassetLoader {
    fn load<'a>(
        &'a self,
        reader: &'a mut dyn Reader,
        _meta: &'a dyn AssetMetaDyn,
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
            loader: self.type_name().to_string(),
            settings: <FakeAssetLoader as AssetLoader>::Settings::default(),
        }))
    }

    fn type_name(&self) -> &'static str {
        core::any::type_name::<Self>()
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

struct ActionLoader;

impl ErasedAssetLoader for ActionLoader {
    fn load<'a>(
        &'a self,
        reader: &'a mut dyn Reader,
        _meta: &'a dyn AssetMetaDyn,
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
            loader: self.type_name().to_string(),
            settings: <FakeAssetLoader as AssetLoader>::Settings::default(),
        }))
    }

    fn type_name(&self) -> &'static str {
        core::any::type_name::<Self>()
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

// XXX TODO: Remove this at some point? It's a recursive version that doesn't
// work (due to async + recursion = trouble), but might be useful reference.
/*
async fn action_key_from_dependency_cache(
    path: &AssetRef<'static>,
    asset_dependency_key: &DependencyCacheKey,
    shared: &BassetShared,
    asset_server: &AssetServer,
) -> Option<ActionCacheKey> {
    let dependency_list = shared
        .dependency_cache
        .get(asset_dependency_key, path)
        .await?;

    let mut hasher = blake3::Hasher::new();

    // XXX TODO: Action key also needs loader version.
    // XXX TODO: Anything else?

    hasher.update(&asset_dependency_key.0);

    // XXX TODO: How are we stabilising the order of dependencies?

    for dependee in &dependency_list.list {
        let dependee_dependency_key = dependency_key(dependee, shared, asset_server).await;

        // XXX TODO: Avoid cycles.
        // XXX TODO: Maybe avoid recursion?

        let dependee_action_key = action_key_from_dependency_cache(
            dependee,
            &dependee_dependency_key,
            shared,
            asset_server,
        )
        .await?;

        hasher.update(&dependee_action_key.0);
    }

    Some(ActionCacheKey(*hasher.finalize().as_bytes()))
}
*/

async fn action_key_from_dependency_cache(
    path: &AssetRef<'static>,
    asset_dependency_key: &DependencyCacheKey,
    asset_server: &AssetServer,
) -> Option<StandaloneAssetInfo> {
    // XXX TODO: How are we handling duplicates and cycles? Maybe need a local path -> action cache.
    // XXX TODO: Rewrite this to use a couple of arrays instead of needing an
    // array per `Entry`. One array of `Option<ActionCacheKey` that gets filled
    // out, and another array of `(AssetRef, usize, Range)` where the usize is
    // the destination key for that `AssetRef`, and the `Range` points to the
    // source dependee keys.

    struct Entry {
        path: AssetRef<'static>,
        dependency_key: DependencyCacheKey,
        pending_dependees: Vec<AssetRef<'static>>,
        completed_dependees: HashMap<AssetRef<'static>, ActionCacheKey>,
    }

    let shared = asset_server.basset_shared().clone();

    let dependency_cache = shared.dependency_cache.as_ref()?;

    let initial_dependee_list = dependency_cache.get(asset_dependency_key, path).await?;

    let mut stack = vec![Entry {
        path: path.clone(),
        dependency_key: *asset_dependency_key,
        pending_dependees: initial_dependee_list.list.clone(),
        completed_dependees: HashMap::new(),
    }];

    loop {
        let mut top = stack.pop().expect("Should never be empty at this point");

        if let Some(next_dependee) = top.pending_dependees.pop() {
            let dependee_dependency_key = shared
                .dependency_key(&next_dependee, None, asset_server)
                .await;

            let dependee_dependee_list = dependency_cache
                .get(&dependee_dependency_key, &next_dependee)
                .await?;

            stack.push(Entry {
                path: next_dependee,
                dependency_key: dependee_dependency_key,
                pending_dependees: dependee_dependee_list.list.clone(),
                completed_dependees: HashMap::new(),
            });

            continue;
        }

        let action_key = BassetShared::action_key(&top.dependency_key, &top.completed_dependees);

        if let Some(mut parent) = stack.pop() {
            // XXX TODO: If already inserted, verify the action key matches.
            parent.completed_dependees.insert(top.path, action_key);

            stack.push(parent);

            continue;
        }

        let immediate_dependee_action_keys = ImmediateDependeeActionKeys::new(
            top.completed_dependees.iter().map(|(p, k)| (p.clone(), *k)),
        );

        return Some(StandaloneAssetInfo {
            action_key,
            dependency_key: *asset_dependency_key,
            immediate_dependee_action_keys,
        });
    }
}

const STANDALONE_MAGIC: &[u8] = b"BEVY_STANDALONE_ASSET\n";
const STANDALONE_VERSION: u16 = 1;

#[derive(Default, Clone, Eq, PartialEq, Debug, Serialize, Deserialize)]
struct ImmediateDependeeActionKeys {
    // XXX TODO: Could store some debug info on the input?
    list: Vec<(AssetRef<'static>, ActionCacheKey)>,
}

impl ImmediateDependeeActionKeys {
    fn new(list: impl Iterator<Item = (AssetRef<'static>, ActionCacheKey)>) -> Self {
        let mut list = list.collect::<Vec<_>>();
        list.sort();

        Self { list }
    }
}

#[derive(Clone, PartialEq, Eq, Debug, Serialize, Deserialize)]
struct StandaloneAssetInfo {
    action_key: ActionCacheKey,
    dependency_key: DependencyCacheKey,
    immediate_dependee_action_keys: ImmediateDependeeActionKeys,
}

async fn read_standalone_asset(
    original_path: &AssetRef<'static>,
    blob: &[u8],
    context: &ApplyContext<'_>,
    expected_info: &StandaloneAssetInfo,
) -> Result<ErasedLoadedAsset, BevyError> {
    let mut blob = BlobReader::new(blob);

    let magic = blob.bytes(STANDALONE_MAGIC.len()).expect("TODO");

    if magic != STANDALONE_MAGIC {
        return Err("TODO".into());
    }

    let version = blob.u16().expect("TODO");

    if version != STANDALONE_VERSION {
        return Err("TODO".into());
    }

    // XXX TODO: Some awkward duplication here. We get the loader name so we can
    // deserialize the meta, but that meta already contains the loader name.
    // Don't see an obvious solution.

    let loader_name = blob.string().expect("TODO");
    let meta_bytes = blob.bytes_sized().expect("TODO");
    let info_bytes = blob.bytes_sized().expect("TODO");
    let asset_bytes = blob.bytes_sized().expect("TODO");

    let loader = context
        .asset_server
        .get_asset_loader_with_type_name(loader_name)
        .await
        .expect("TODO");

    let meta = loader.deserialize_meta(meta_bytes).expect("TODO");

    let info = ron::de::from_bytes::<StandaloneAssetInfo>(info_bytes).expect("TODO");

    assert_eq!(&info, expected_info);

    let mut reader = SliceReader::new(asset_bytes);

    let load_dependencies = false;
    let populate_hashes = false;

    // Don't update the dependency cache - this asset loader loader doesn't
    // know about the dependencies of the action that produced this asset.
    //
    // XXX TODO: Maybe better if we sidestepped `load_with_meta_loader_and_reader`
    // for clarity? Or generally rethink how the dependency cache gets filled out.
    let update_dependency_cache = false;

    let mut asset = context
        .asset_server
        .load_with_meta_loader_and_reader(
            original_path,
            &*meta,
            &*loader,
            &mut reader,
            load_dependencies,
            populate_hashes,
            update_dependency_cache,
        )
        .await
        .map_err(Into::<BevyError>::into)?;

    // Apply the correct dependencies.

    let mut loader_dependencies = HashMap::<AssetRef<'static>, AssetHash>::new();

    for (dependee, _) in &info.immediate_dependee_action_keys.list {
        // XXX TODO: Fake hash for now. Not sure if we need this long-term as
        // the hash is only used for processing.
        let hash = [0u8; 32];

        loader_dependencies.insert(dependee.clone(), hash);
    }

    // Check that the recorded dependencies are correct - they should be a
    // superset of what the loader used. If this fails then either we didn't
    // record the dependency correctly, or the loader is unexpectedly reading
    // different dependencies.

    for dependee in asset.loader_dependencies.keys() {
        assert!(loader_dependencies.contains_key(dependee), "{dependee}");
    }

    asset.loader_dependencies = loader_dependencies;

    Ok(asset)
}

async fn write_standalone_asset(
    asset: &ErasedLoadedAsset,
    loader: &dyn ErasedAssetLoader,
    saver: &dyn ErasedAssetSaver,
    saver_settings: &dyn Settings,
    info: &StandaloneAssetInfo,
) -> Result<Box<[u8]>, BevyError> {
    let mut asset_bytes = Vec::<u8>::new();

    saver
        .save(&mut asset_bytes, asset, saver_settings)
        .await
        .map_err(Into::<BevyError>::into)?;

    // XXX TODO: Think through loader settings. Firstly, if the asset was loaded
    // with certain settings then we should preserve them here? There might also
    // be situations where a saver/loader pair are expecting certain settings?
    // Could get messy.

    let meta_bytes = loader.default_meta().serialize();
    let info_bytes = ron::ser::to_string(&info).expect("TODO").into_bytes();

    let mut writer = Vec::<u8>::new();

    let mut blob = BlobWriter::new(&mut writer);

    blob.bytes(STANDALONE_MAGIC);
    blob.u16(STANDALONE_VERSION);
    blob.string(loader.type_name());
    blob.bytes_sized(&meta_bytes);
    blob.bytes_sized(&info_bytes);
    blob.bytes_sized(&asset_bytes);

    Ok(writer.into())
}

#[derive(Hash, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Serialize, Deserialize)]
pub struct BassetHash([u8; 32]);

impl BassetHash {
    // XXX TODO: Should take bytes by value or ref?
    fn new(bytes: [u8; 32]) -> Self {
        Self(bytes)
    }

    // XXX TODO: Naming convention suggests `as_bytes` should return a reference?
    // So this should be `to_bytes`?
    fn as_bytes(&self) -> [u8; 32] {
        self.0
    }
}

impl Debug for BassetHash {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        // XXX TODO: Review. Duplicated elsewhere. Could be optimized. Avoid `std`?
        let hex = String::from_iter(self.0.iter().map(|b| std::format!("{:x}", b)));

        Debug::fmt(&hex, f)
    }
}

impl Display for BassetHash {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        // Display only the first 8 characters.
        //
        // XXX TODO: Review. Duplicated elsewhere. Could be optimized. Avoid `std`?
        let hex = String::from_iter(self.0.iter().take(4).map(|b| std::format!("{:x}", b)));

        Display::fmt(&hex, f)
    }
}

trait CacheKey: Copy + Clone + Eq + Hash + Debug + Send + Sync {
    fn as_hash(&self) -> BassetHash;
}

trait MemoryCacheValue: Send + Sync + Clone + Eq + Debug {}

impl<T: Send + Sync + Clone + Eq + Debug> MemoryCacheValue for T {}

struct MemoryCache<K: CacheKey, V: MemoryCacheValue> {
    name: &'static str,
    key_to_value: HashMap<K, V>,
    validate: bool,
}

fn should_log(path: &AssetRef<'static>) -> bool {
    // Skip embedded sources for now as they're too spammy.
    if let Some(path) = path.path()
        && path.source().as_str() == Some("embedded")
    {
        false
    } else {
        true
    }
}

fn log(name: &'static str, path: &AssetRef<'static>, key: BassetHash, string: &'static str) {
    if should_log(path) {
        debug!(%key, ?path, "{name}: {string}");
    }
}

impl<K: CacheKey, V: MemoryCacheValue> MemoryCache<K, V> {
    fn new(name: &'static str, validate: bool) -> Self {
        Self {
            name,
            key_to_value: Default::default(),
            validate,
        }
    }

    // XXX TODO: `asset_path` is only for debugging. Maybe make it more opaque?
    fn get(&self, key: &K, asset_path: &AssetRef<'static>) -> Option<V> {
        if self.validate {
            return None;
        }

        let result = self.key_to_value.get(key).cloned();

        if result.is_some() {
            log(self.name, asset_path, key.as_hash(), "Memory cache hit.");
        } else {
            log(self.name, asset_path, key.as_hash(), "Memory cache miss.");
        }

        result
    }

    // XXX TODO: `asset_path` is only for debugging. Maybe make it more opaque?
    fn put(&mut self, key: K, value: V, asset_path: &AssetRef<'static>) {
        log(self.name, asset_path, key.as_hash(), "Memory cache put.");

        if self.validate
            && let Some(existing) = self.key_to_value.get(&key)
        {
            assert_eq!(&value, existing, "{key:?}");
        }

        self.key_to_value.insert(key, value);
    }
}

// XXX TODO: Why do we need this `static` bound to make the async happy?
trait FileCacheValue: Send + Sync + Clone + Eq + Debug + 'static {
    fn write(&self, file: &mut File) -> impl ConditionalSendFuture<Output = Result<(), BevyError>>;

    fn read(bytes: Box<[u8]>) -> Self;
}

impl FileCacheValue for Arc<[u8]> {
    async fn write(&self, file: &mut File) -> Result<(), BevyError> {
        file.write_all(self).await.map_err(BevyError::from)
    }

    fn read(bytes: Box<[u8]>) -> Self {
        bytes.into()
    }
}

#[derive(Default)]
struct FileCache<K: CacheKey, V: FileCacheValue> {
    name: &'static str,
    base_path: PathBuf,
    validate: bool,
    phantom: PhantomData<(K, V)>,
}

impl<K: CacheKey, V: FileCacheValue> FileCache<K, V> {
    fn new(name: &'static str, base_path: PathBuf, validate: bool) -> Self {
        Self {
            name,
            base_path,
            validate,
            phantom: PhantomData::<(K, V)>,
        }
    }

    fn value_path(&self, key: &K) -> PathBuf {
        // XXX TODO: Duplicates `BassetHash` debug?
        // XXX TODO: Check if this is worth optimising.
        let hex = String::from_iter(key.as_hash().0.iter().map(|b| format!("{:x}", b)));

        // Spread files across multiple folders by using the first few digits of
        // the hash. This is a hedge against filesystems that don't like
        // thousands of files in a single folder.
        //
        // XXX TODO: Could be refined? Review the probabilities.
        // XXX TODO: Can be optimised if the file separator is a single character?
        let relative_path = [&hex[0..2], &hex[2..4], &hex[..]]
            .iter()
            .collect::<PathBuf>();

        self.base_path.join(relative_path)
    }

    // XXX TODO: `asset_path` is only for debugging. Maybe make it more opaque?
    async fn get(&self, key: &K, asset_path: &AssetRef<'static>) -> Option<V> {
        if self.validate {
            return None;
        }

        match self.unvalidated_get(key).await {
            Some(value) => {
                log(self.name, asset_path, key.as_hash(), "File cache hit.");
                Some(value)
            }
            None => {
                log(self.name, asset_path, key.as_hash(), "File cache miss.");
                None
            }
        }
    }

    // XXX TODO: `asset_path` is only for debugging. Maybe make it more opaque?
    async fn unvalidated_get(&self, key: &K) -> Option<V> {
        let value_path = self.value_path(key);

        if let Ok(value) = async_fs::read(value_path).await {
            Some(<V as FileCacheValue>::read(value.into()))
        } else {
            None
        }
    }

    // XXX TODO: `asset_path` is only for debugging. Maybe make it more opaque?
    async fn put(&self, key: K, value: V, asset_path: &AssetRef<'static>) {
        let value_path = self.value_path(&key);

        // XXX TODO: Review faff that avoids lifetime issues.
        let value = value.clone();

        log(self.name, asset_path, key.as_hash(), "File cache put.");

        if self.validate
            && let Some(existing) = self.unvalidated_get(&key).await
        {
            assert_eq!(&value, &existing, "{key:?}");
        }

        IoTaskPool::get()
            .spawn(async move {
                // XXX TODO: Early out if file already exists?

                async_fs::create_dir_all(value_path.parent().expect("TODO"))
                    .await
                    .expect("TODO");

                let temp_path = value_path.with_extension("tmp");

                let Ok(mut temp_file) = async_fs::OpenOptions::new()
                    .write(true)
                    .create_new(true)
                    .open(&temp_path)
                    .await
                else {
                    // TODO: Temp files could be left behind if interrupted
                    // during write or rename. Should we try to clean them up?
                    return;
                };

                <V as FileCacheValue>::write(&value, &mut temp_file)
                    .await
                    .expect("TODO");

                async_fs::rename(temp_path, value_path)
                    .await
                    .expect("TODO, this is ok to fail?");
            })
            .detach();
    }
}

struct MemoryAndFileCache<K: CacheKey, V: FileCacheValue + MemoryCacheValue> {
    name: &'static str,
    memory: Arc<RwLock<MemoryCache<K, V>>>,
    file: Option<FileCache<K, V>>,
}

impl<K: CacheKey, V: FileCacheValue> MemoryAndFileCache<K, V> {
    fn new(name: &'static str, file_cache_path: Option<PathBuf>, validate: bool) -> Self {
        MemoryAndFileCache {
            name,
            memory: Arc::new(RwLock::new(MemoryCache::new(name, validate))),
            file: file_cache_path.map(|p| FileCache::new(name, p, validate)),
        }
    }

    // XXX TODO: `asset_path` is only for debugging. Maybe make it more opaque?
    async fn get(&self, key: &K, asset_path: &AssetRef<'static>) -> Option<V> {
        if let Some(from_memory) = self
            .memory
            .read()
            .unwrap_or_else(PoisonError::into_inner)
            .get(key, asset_path)
        {
            return Some(from_memory);
        }

        if let Some(file) = &self.file {
            let from_file = file.get(key, asset_path).await?;

            self.memory
                .write()
                .unwrap_or_else(PoisonError::into_inner)
                .put(*key, from_file.clone(), asset_path);

            Some(from_file)
        } else {
            None
        }
    }

    // XXX TODO: `asset_path` is only for debugging. Maybe make it more opaque?
    async fn put(&self, key: K, value: V, asset_path: &AssetRef<'static>) {
        // XXX TODO: Avoid `blob.clone()` if there's no file cache?
        self.memory
            .write()
            .unwrap_or_else(PoisonError::into_inner)
            .put(key, value.clone(), asset_path);

        if let Some(file) = &self.file {
            file.put(key, value, asset_path).await;
        }
    }
}

#[derive(Copy, Clone, Hash, Eq, PartialEq)]
struct ContentHash(BassetHash);

#[derive(Default)]
struct ContentCache {
    path_to_hash: RwLock<HashMap<AssetPath<'static>, ContentHash>>,
}

impl ContentHash {
    fn as_bytes(&self) -> [u8; 32] {
        self.0.as_bytes()
    }
}

impl ContentCache {
    // XXX TODO: Can we avoid `AssetPath` being `<'static>`?
    // XXX TODO: Returning BassetHash by value is probabably the practical choice? But check.
    async fn get(
        &self,
        path: &AssetPath<'static>,
        asset_server: &AssetServer,
    ) -> Result<ContentHash, BevyError> {
        if let Some(hash) = self
            .path_to_hash
            .read()
            .unwrap_or_else(PoisonError::into_inner)
            .get(path)
        {
            // XXX TODO: Review. Too spammy for now.
            //info!(?path, "Content cache hit.");

            return Ok(*hash);
        }

        // XXX TODO: Review. Too spammy for now.
        //info!(?path, "Content cache miss.");

        let source = asset_server
            .get_source(path.source())
            .map_err(BevyError::from)?;

        let mut reader = source
            .reader()
            .read(path.path())
            .await
            .map_err(BevyError::from)?;

        let mut bytes = Vec::<u8>::new();
        reader
            .read_to_end(&mut bytes)
            .await
            .map_err(BevyError::from)?;

        let hash = content_hash(&bytes);

        self.path_to_hash
            .write()
            .unwrap_or_else(PoisonError::into_inner)
            .insert(path.clone(), hash);

        Ok(hash)
    }
}

#[derive(Hash, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Debug, Serialize, Deserialize)]
pub struct DependencyCacheKey(BassetHash);

impl DependencyCacheKey {
    fn as_bytes(&self) -> [u8; 32] {
        self.0.as_bytes()
    }
}

impl Display for DependencyCacheKey {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        Display::fmt(&self.0, f)
    }
}

impl CacheKey for DependencyCacheKey {
    fn as_hash(&self) -> BassetHash {
        self.0
    }
}

#[derive(Default, Debug, Eq, PartialEq, Serialize, Deserialize)]
struct DependencyCacheValue {
    // XXX TODO: Could store some debug info on the input?
    list: Vec<AssetRef<'static>>,
}

impl DependencyCacheValue {
    fn new(list: impl Iterator<Item = AssetRef<'static>>) -> Self {
        let mut list = list.collect::<Vec<_>>();
        list.sort();

        Self { list }
    }
}

impl FileCacheValue for Arc<DependencyCacheValue> {
    async fn write(&self, file: &mut File) -> Result<(), BevyError> {
        // XXX TODO: Should have a serialized struct that includes a version number and an opaque value.
        let string = ron::ser::to_string(self.as_ref()).map_err(BevyError::from)?;

        file.write_all(string.as_bytes())
            .await
            .map_err(BevyError::from)
    }

    fn read(bytes: Box<[u8]>) -> Self {
        Arc::new(ron::de::from_bytes::<DependencyCacheValue>(&bytes).expect("TODO"))
    }
}

fn content_hash(bytes: &[u8]) -> ContentHash {
    let mut hasher = blake3::Hasher::new();
    hasher.update(bytes);
    ContentHash(BassetHash::new(*hasher.finalize().as_bytes()))
}

#[derive(Hash, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Debug, Serialize, Deserialize)]
pub struct ActionCacheKey(pub BassetHash);

impl ActionCacheKey {
    fn as_bytes(&self) -> [u8; 32] {
        self.0.as_bytes()
    }
}

impl Display for ActionCacheKey {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        Display::fmt(&self.0, f)
    }
}

impl CacheKey for ActionCacheKey {
    fn as_hash(&self) -> BassetHash {
        self.0
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

    let path = AssetRef::from(path);

    let (mut meta, loader, mut reader) = asset_server
        // XXX TODO `for_path` or `for_action`?
        .get_meta_loader_and_reader_for_ref(&path, None)
        .await
        .map_err(Into::<BevyError>::into)?;

    apply_settings(meta.loader_settings_mut(), settings);

    // Roughly the same as LoadContext::load_direct_internal.

    let load_dependencies = false;
    let populate_hashes = false;
    let update_dependency_cache = true;

    let asset = asset_server
        .load_with_meta_loader_and_reader(
            &path,
            &*meta,
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
        shared: &shared,
    };

    if let Some(action_cache) = &shared.action_cache {
        // XXX TODO: Maybe early out here if there's no saver? Depends if we end
        // up in a situation where the saver has been compiled out but we still
        // want to read from the cache.

        // XXX TODO: Review passing `None` for `meta`. Is that right?
        let dependency_key = shared.dependency_key(&path, None, asset_server).await;

        let info = action_key_from_dependency_cache(&path, &dependency_key, asset_server).await;

        if let Some(info) = info
            && let Some(cached_standalone_asset) = action_cache.get(&info.action_key, &path).await
        {
            return read_standalone_asset(&path, &cached_standalone_asset, &apply_context, &info)
                .await;
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
        && let Some((saver, settings)) = shared.saver(asset.asset_type_name())
    {
        // XXX TODO: Review duplication and awkwardness. We call `register_dependees`,
        // and then immediately look them up inside `action_key_from_dependency_cache`.

        asset_server
            .basset_shared()
            .register_dependees(
                &path, // XXX TODO: Avoid clone?
                None,
                asset.loader_dependencies.keys().cloned(),
                asset_server,
            )
            .await;

        let dependency_key = shared.dependency_key(&path, None, asset_server).await;

        let info = action_key_from_dependency_cache(&path, &dependency_key, asset_server).await;

        if let Some(info) = info {
            // XXX TODO: Verify loader matches saver? Need a way to get
            // `AssetSaver::OutputLoader` out of `ErasedAssetSaver`.
            let loader = asset_server
                .get_asset_loader_with_type_name(saver.loader_type_name())
                .await
                .expect("TODO");

            let blob = write_standalone_asset(&asset, &*loader, saver, settings, &info).await?;

            action_cache.put(info.action_key, blob.into(), &path).await;
        } else {
            warn!(
                ?path,
                %dependency_key,
                "Failed to get info from dependency cache after loading."
            );
        }
    } else {
        let type_name = asset.asset_type_name();
        debug!(?type_name, ?path, "Cache ineligible, no saver for type.");
    }

    Ok(asset)
}

// Helper for reading things out of a byte slice.
//
// XXX TODO: Surely this exists somewhere as a library? Or are there better
// approaches? Contrast with `MeshletMeshLoader` - fairly similar, but uses
// `Reader` directly plus a few helpers. Also see `byteorder` crate
// (https://docs.rs/byteorder/latest/byteorder/trait.ReadBytesExt.html)
struct BlobReader<'a> {
    data: &'a [u8],
    cursor: usize,
}

impl<'a> BlobReader<'a> {
    fn new(data: &'a [u8]) -> Self {
        Self { data, cursor: 0 }
    }

    fn bytes(&mut self, num: usize) -> Option<&'a [u8]> {
        let range = self.cursor..(self.cursor + num);
        self.cursor = range.end;
        self.data.get(range)
    }

    fn bytes_const<const NUM: usize>(&mut self) -> Option<&'a [u8; NUM]> {
        <&[u8; NUM]>::try_from(self.bytes(NUM)?).ok()
    }

    fn bytes_sized(&mut self) -> Option<&'a [u8]> {
        let len = self.u64()? as usize;
        self.bytes(len)
    }

    fn u64(&mut self) -> Option<u64> {
        let bytes = *self.bytes_const::<{ size_of::<u64>() }>()?;
        Some(u64::from_le_bytes(bytes))
    }

    fn u16(&mut self) -> Option<u16> {
        let bytes = *self.bytes_const::<{ size_of::<u16>() }>()?;
        Some(u16::from_le_bytes(bytes))
    }

    fn string(&mut self) -> Option<&'a str> {
        let len = self.u64()? as usize;
        str::from_utf8(self.bytes(len)?).ok()
    }
}

struct BlobWriter<'a> {
    writer: &'a mut dyn Write,
}

impl<'a> BlobWriter<'a> {
    fn new(writer: &'a mut dyn Write) -> Self {
        Self { writer }
    }

    fn bytes(&mut self, bytes: &[u8]) {
        self.writer.write_all(bytes).expect("TODO");
    }

    fn bytes_sized(&mut self, bytes: &[u8]) {
        self.u64(bytes.len() as u64);
        self.bytes(bytes);
    }

    fn u64(&mut self, value: u64) {
        self.writer.write_all(&value.to_le_bytes()).expect("TODO");
    }

    fn u16(&mut self, value: u16) {
        self.writer.write_all(&value.to_le_bytes()).expect("TODO");
    }

    fn string(&mut self, string: &str) {
        self.bytes_sized(string.as_bytes());
    }
}

// TODO: Review this. Awkward hack so that `BassetLoader::default_meta` and
// `BassetLoader::deserialize_meta` can return a meta even if it's wrong.
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
