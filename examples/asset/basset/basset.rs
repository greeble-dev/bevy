//! Basset proof of concept.
#![expect(dead_code, reason = "TODO")]

#[path = "../../helpers/camera_controller.rs"]
mod camera_controller;

use async_fs::File;
use bevy::{
    asset::{
        io::Reader,
        meta::{AssetAction, AssetHash, AssetMeta, AssetMetaDyn, Settings},
        saver::{AssetSaver, ErasedAssetSaver},
        AssetLoader, AssetPath, DeserializeMetaError, ErasedAssetLoader, ErasedLoadedAsset,
        LoadContext, LoadedAsset,
    },
    ecs::error::BevyError,
    light::CascadeShadowConfigBuilder,
    pbr::experimental::meshlet::{
        MeshletMesh, MeshletPlugin, MESHLET_DEFAULT_VERTEX_POSITION_QUANTIZATION_FACTOR,
    },
    platform::{
        collections::HashMap,
        sync::{PoisonError, RwLock},
    },
    prelude::*,
    reflect::TypePath,
    render::render_resource::AsBindGroup,
    tasks::{BoxedFuture, ConditionalSendFuture, IoTaskPool},
    time::common_conditions::on_timer,
};
use bevy_asset::{
    io::SliceReader, ActionCacheKey, AssetAction2, AssetRef, AsyncWriteExt, LoadedAssetKeys,
};
use camera_controller::{CameraController, CameraControllerPlugin};
use core::result::Result;
use downcast_rs::{impl_downcast, Downcast};
use serde::{Deserialize, Serialize};
use std::{
    boxed::Box, hash::Hash, io::Write, marker::PhantomData, path::PathBuf, sync::Arc,
    time::Duration,
};

struct BassetPlugin;

impl Plugin for BassetPlugin {
    fn build(&self, app: &mut App) {
        let shared = Arc::new(
            BassetShared::new(Some("examples/asset/basset/cache".into()))
                .with_action(action::LoadPath)
                .with_action(action::JoinStrings)
                .with_action(action::UppercaseString)
                .with_action(action::AcmeSceneFromGltf)
                .with_action(action::MeshletFromMesh)
                .with_action(action::ConvertAcmeSceneMeshesToMeshlets)
                .with_saver(demo::StringAssetSaver)
                .with_saver(demo::IntAssetSaver),
        );

        app.init_asset::<demo::StringAsset>()
            .init_asset::<demo::IntAsset>()
            .init_asset::<acme::AcmeScene>()
            .register_asset_loader(demo::StringAssetLoader)
            .register_asset_loader(demo::IntAssetLoader)
            .register_erased_asset_loader(Box::new(BassetLoader::new(shared.clone())))
            .register_erased_asset_loader(Box::new(ActionLoader::new(shared)))
            .add_systems(Update, acme::tick_scene_spawners);
    }
}

// XXX TODO: Document. Reconsider name.
fn resolve_keys(
    keys: &HashMap<AssetRef<'static>, Option<ActionCacheKey>>,
) -> Option<HashMap<AssetRef<'static>, ActionCacheKey>> {
    let mut out = HashMap::<AssetRef<'static>, ActionCacheKey>::new();

    for (key, optional_value) in keys {
        if let Some(value) = optional_value {
            out.insert(key.clone(), *value);
        } else {
            return None;
        }
    }

    Some(out)
}

/// XXX TODO: Document.
pub struct ApplyContext<'a> {
    // XXX TODO: Can this be a reference? Also review `'static` lifetime.
    path: AssetRef<'static>,
    asset_server: &'a AssetServer,
    immediate_dependee_action_keys: HashMap<AssetRef<'static>, Option<ActionCacheKey>>,

    // Equivalent to `ErasedLoadedAsset::loader_dependencies.
    // XXX TODO: Review this. Partially duplicates immediate_dependee_action_keys.
    loader_dependencies: HashMap<AssetRef<'static>, AssetHash>,

    dependency_key: DependencyCacheKey,
    shared: &'a BassetShared,
}

impl ApplyContext<'_> {
    async fn erased_load_dependee(
        &mut self,
        path: &AssetRef<'static>,
    ) -> Result<ErasedLoadedAsset, BevyError> {
        let asset = load_ref(self.asset_server, path, self.shared).await?;

        // XXX TODO: What if the dependee doesn't have an action key?
        //
        // If it's an action, we can run the action directly here? If it's a path
        // then we load and work out the dependency key

        self.immediate_dependee_action_keys.insert(
            path.clone(),
            asset.keys.as_ref().map(|keys| keys.action_key),
        );

        // XXX TODO: Decide what to do here. The hash only appears to be used
        // for asset processing, so maybe can ignore for now.
        let hash = [0u8; 32];

        self.loader_dependencies.insert(path.clone(), hash);

        Ok(asset)
    }

    // XXX TODO: Kinda duplicates erased_load_dependee.
    async fn erased_load_dependee_path(
        &mut self,
        path: &AssetPath<'static>,
        settings: &Option<Box<ron::value::RawValue>>,
    ) -> Result<ErasedLoadedAsset, BevyError> {
        let asset = load_path(self.asset_server, path, settings, self.shared).await?;

        // XXX TODO: What if the dependee doesn't have an action key?
        //
        // If it's an action, we can run the action directly here? If it's a path
        // then we load and work out the dependency key

        self.immediate_dependee_action_keys.insert(
            path.clone().into(),
            asset.keys.as_ref().map(|keys| keys.action_key),
        );

        // XXX TODO: Decide what to do here. The hash only appears to be used
        // for asset processing, so maybe can ignore for now.
        let hash = [0u8; 32];

        // XXX TODO: Duplicates earlier `path.clone().into()`.
        self.loader_dependencies.insert(path.clone().into(), hash);

        Ok(asset)
    }

    async fn load_dependee<T: Asset>(&mut self, path: &AssetRef<'static>) -> Result<T, BevyError> {
        match self.erased_load_dependee(path).await?.downcast::<T>() {
            Ok(result) => Ok(result.take()),
            Err(original) => panic!(
                "Should have made type {}, actually made type {}. Path: {:?}",
                core::any::type_name::<T>(),
                original.asset_type_name(),
                path,
            ),
        }
    }

    async fn finish<A: Asset>(self, asset: A) -> ErasedLoadedAsset {
        if let Some(immediate_dependee_action_keys) =
            resolve_keys(&self.immediate_dependee_action_keys)
        {
            // XXX TODO: Check if cloning and collecting can be reduced.
            let dependency_list = DependencyList {
                list: immediate_dependee_action_keys.keys().cloned().collect(),
            };

            // XXX TODO: Move into function?
            // XXX TODO: Check if cloning and collecting can be reduced.
            let loaded_asset_keys = LoadedAssetKeys {
                action_key: action_key_from_loaded_dependencies(
                    &self.dependency_key,
                    &immediate_dependee_action_keys,
                ),
                immediate_dependee_action_keys,
            };

            self.shared.dependency_cache.put(
                self.dependency_key,
                Arc::new(dependency_list),
                &self.path.clone(), // XXX TODO: Avoid clone?
            );

            LoadedAsset::new_with_keys(
                asset,
                Some(loaded_asset_keys),
                Some(self.loader_dependencies),
            )
            .into()
        } else {
            LoadedAsset::new_with_keys(asset, None, Some(self.loader_dependencies)).into()
        }
    }
}

/// XXX TODO: Review this. Duplicated from `bevy_asset::meta::Settings`.
pub trait BassetActionParams: Downcast + Send + Sync + 'static {}

// XXX TODO: Review this. Duplicated from `bevy_asset::meta::Settings`.
impl_downcast!(BassetActionParams);

// XXX TODO: Review this. Duplicated from `bevy_asset::meta::Settings`.
impl<T: 'static> BassetActionParams for T where T: Send + Sync {}

/// XXX TODO: Document.
pub trait BassetAction: Send + Sync + 'static {
    /// XXX TODO: Document. Review if serialize traits should go into `BassetActionParams`.
    type Params: BassetActionParams + Serialize + for<'a> Deserialize<'a>;

    /// XXX TODO: Document.
    type Error: Into<BevyError>;

    /// XXX TODO: Document.
    fn apply(
        &self,
        context: &mut ApplyContext<'_>,
        params: &Self::Params,
    ) -> impl ConditionalSendFuture<Output = Result<ErasedLoadedAsset, Self::Error>>;
}

trait ErasedBassetAction: Send + Sync + 'static {
    fn apply<'a>(
        &'a self,
        context: &'a mut ApplyContext,
        params: &'a ron::value::RawValue,
    ) -> BoxedFuture<'a, Result<ErasedLoadedAsset, BevyError>>;
}

impl<T> ErasedBassetAction for T
where
    T: BassetAction + Send + Sync,
{
    fn apply<'a>(
        &'a self,
        context: &'a mut ApplyContext,
        params: &'a ron::value::RawValue,
    ) -> BoxedFuture<'a, Result<ErasedLoadedAsset, BevyError>> {
        Box::pin(async move {
            let params = params.into_rust::<T::Params>().expect("TODO");

            T::apply(self, context, &params).await.map_err(Into::into)
        })
    }
}

#[derive(Serialize, Deserialize, Debug, Clone)]
struct BassetFileSerializable {
    root: AssetRef<'static>,
    // TODO: Versioning?
}

// XXX TODO: Review if this actually needs to be shared between `BassetLoader`
// and `ActionLoader`.
struct BassetShared {
    action_type_name_to_action: HashMap<&'static str, Box<dyn ErasedBassetAction>>,
    asset_type_name_to_saver: HashMap<&'static str, (Box<dyn ErasedAssetSaver>, Box<dyn Settings>)>,
    content_cache: ContentCache,
    dependency_cache: MemoryAndFileCache<DependencyCacheKey, Arc<DependencyList>>,
    action_cache: MemoryAndFileCache<ActionCacheKey, Arc<[u8]>>,
}

impl BassetShared {
    fn new(file_cache_path: Option<PathBuf>) -> Self {
        Self {
            action_type_name_to_action: Default::default(),
            asset_type_name_to_saver: Default::default(),
            content_cache: Default::default(),
            dependency_cache: MemoryAndFileCache::new(
                file_cache_path.as_ref().map(|p| p.join("dependency")),
            ),
            action_cache: MemoryAndFileCache::new(
                file_cache_path.as_ref().map(|p| p.join("action")),
            ),
        }
    }

    fn with_action<T: BassetAction>(mut self, action: T) -> Self {
        self.action_type_name_to_action
            .insert(core::any::type_name::<T>(), Box::new(action));

        self
    }

    fn with_saver<T: AssetSaver>(mut self, saver: T) -> Self {
        let settings = T::Settings::default();

        self.asset_type_name_to_saver.insert(
            core::any::type_name::<T::Asset>(),
            (Box::new(saver), Box::new(settings)),
        );

        self
    }

    fn action<'a>(&'a self, type_name: &str) -> Option<&'a dyn ErasedBassetAction> {
        self.action_type_name_to_action
            .get(type_name)
            .map(move |a| &**a)
    }

    fn saver<'a>(
        &'a self,
        type_name: &str,
    ) -> Option<(&'a dyn ErasedAssetSaver, &'a dyn Settings)> {
        self.asset_type_name_to_saver
            .get(type_name)
            .map(move |s| (&*s.0, &*s.1))
    }
}

struct BassetLoader {
    shared: Arc<BassetShared>,
}

impl BassetLoader {
    fn new(shared: Arc<BassetShared>) -> Self {
        Self { shared }
    }
}

impl ErasedAssetLoader for BassetLoader {
    fn load<'a>(
        &'a self,
        reader: &'a mut dyn Reader,
        _meta: &'a dyn AssetMetaDyn,
        load_context: LoadContext<'a>,
    ) -> BoxedFuture<'a, Result<ErasedLoadedAsset, BevyError>> {
        Box::pin(async move {
            let mut bytes = Vec::new();
            reader.read_to_end(&mut bytes).await?;

            let basset = ron::de::from_bytes::<BassetFileSerializable>(&bytes)?;

            // XXX TODO: Review use of `load_ref`. Is this getting the
            // dependencies and everything else right?

            load_ref(load_context.asset_server(), &basset.root, &self.shared).await
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

    fn type_id(&self) -> std::any::TypeId {
        core::any::TypeId::of::<Self>()
    }

    fn asset_type_name(&self) -> Option<&'static str> {
        None
    }

    fn asset_type_id(&self) -> Option<std::any::TypeId> {
        None
    }
}

struct ActionLoader {
    shared: Arc<BassetShared>,
}

impl ActionLoader {
    fn new(shared: Arc<BassetShared>) -> Self {
        Self { shared }
    }
}

impl ErasedAssetLoader for ActionLoader {
    fn load<'a>(
        &'a self,
        reader: &'a mut dyn Reader,
        _meta: &'a dyn AssetMetaDyn,
        load_context: LoadContext<'a>,
    ) -> BoxedFuture<'a, Result<ErasedLoadedAsset, BevyError>> {
        Box::pin(async move {
            assert_eq!(
                reader.read_to_end(&mut Vec::new()).await?,
                0,
                "Reader should have been empty, all data is in the path."
            );

            let action = load_context.asset_path().action().expect(
                "ActionLoader should have been given an AssetRef::Action, not an AssetRef::Path.",
            );

            load_action(load_context.asset_server(), action, &self.shared).await
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

    fn type_id(&self) -> std::any::TypeId {
        core::any::TypeId::of::<Self>()
    }

    fn asset_type_name(&self) -> Option<&'static str> {
        None
    }

    fn asset_type_id(&self) -> Option<std::any::TypeId> {
        None
    }
}

async fn dependency_key(
    path: &AssetRef<'static>,
    shared: &BassetShared,
    asset_server: &AssetServer,
) -> DependencyCacheKey {
    let mut hasher = blake3::Hasher::new();

    // XXX TODO: Is this wrong for actions and paths with a label? Yes if the cache
    // holds the full asset - the label selects after getting from the cache. No if
    // the cache holds the sub-asset.
    hasher.update(path.to_string().as_bytes());

    if let Some(really_a_path) = path.path() {
        let content_hash = shared
            .content_cache
            .get(really_a_path, asset_server)
            .await
            .expect("TODO");

        hasher.update(&content_hash.0);
    }

    // XXX TODO: Loader version?

    DependencyCacheKey(*hasher.finalize().as_bytes())
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
    shared: &BassetShared,
    asset_server: &AssetServer,
) -> Option<ActionCacheKey> {
    // XXX TODO: How are we handling duplicates? Maybe need a local path -> action cache.
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

    let initial_dependency_list = shared
        .dependency_cache
        .get(asset_dependency_key, path)
        .await?;

    let mut stack = vec![Entry {
        path: path.clone(),
        dependency_key: *asset_dependency_key,
        pending_dependees: initial_dependency_list.list.clone(),
        completed_dependees: HashMap::new(),
    }];

    loop {
        let mut top = stack.pop().expect("Should never be empty at this point");

        if let Some(next_dependee) = top.pending_dependees.pop() {
            let dependee_dependency_key =
                dependency_key(&next_dependee, shared, asset_server).await;

            let dependee_dependency_list = shared
                .dependency_cache
                .get(&dependee_dependency_key, &next_dependee)
                .await?;

            stack.push(Entry {
                path: next_dependee,
                dependency_key: dependee_dependency_key,
                pending_dependees: dependee_dependency_list.list.clone(),
                completed_dependees: HashMap::new(),
            });

            continue;
        }

        let action_key =
            action_key_from_loaded_dependencies(&top.dependency_key, &top.completed_dependees);

        if let Some(mut parent) = stack.pop() {
            parent.completed_dependees.insert(top.path, action_key);

            stack.push(parent);

            continue;
        }

        return Some(action_key);
    }
}

fn action_key_from_loaded_dependencies(
    dependency_key: &DependencyCacheKey,
    immediate_dependee_action_keys: &HashMap<AssetRef<'static>, ActionCacheKey>,
) -> ActionCacheKey {
    let mut hasher = blake3::Hasher::new();

    // XXX TODO: Action key also needs loader version.
    // XXX TODO: Anything else?

    hasher.update(&dependency_key.0);

    // XXX TODO: How are we stabilising the order of dependencies?

    for immediate_dependee_action_key in immediate_dependee_action_keys.values() {
        hasher.update(&immediate_dependee_action_key.0);
    }

    ActionCacheKey(*hasher.finalize().as_bytes())
}

fn action_key_from_loaded_asset(asset: &ErasedLoadedAsset) -> Option<ActionCacheKey> {
    asset.keys.as_ref().map(|keys| keys.action_key)
}

const STANDALONE_MAGIC: &[u8] = b"BEVY_STANDALONE_ASSET\n";
const STANDALONE_VERSION: u16 = 1;

async fn read_standalone_asset(
    blob: &[u8],
    context: &ApplyContext<'_>,
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
    let asset_bytes = blob.bytes_sized().expect("TODO");

    let loader = context
        .asset_server
        .get_asset_loader_with_type_name(loader_name)
        .await
        .expect("TODO");

    let meta = loader.deserialize_meta(meta_bytes).expect("TODO");

    // XXX TODO: Argh? Need to work out how meaningful this is to the loader.
    let fake_path = AssetRef::from(AssetPath::parse("fake_path_from_basset_read_binary_asset"));

    let mut reader = SliceReader::new(asset_bytes);

    let load_dependencies = false;
    let populate_hashes = false;

    context
        .asset_server
        .load_with_meta_loader_and_reader(
            &fake_path,
            &*meta,
            &*loader,
            &mut reader,
            load_dependencies,
            populate_hashes,
        )
        .await
        .map_err(Into::<BevyError>::into)
}

async fn write_standalone_asset(
    asset: &ErasedLoadedAsset,
    loader: &dyn ErasedAssetLoader,
    saver: &dyn ErasedAssetSaver,
    saver_settings: &dyn Settings,
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

    let mut writer = Vec::<u8>::new();

    let mut blob = BlobWriter::new(&mut writer);

    blob.bytes(STANDALONE_MAGIC);
    blob.u16(STANDALONE_VERSION);
    blob.string(loader.type_name());
    blob.bytes_sized(&meta_bytes);
    blob.bytes_sized(&asset_bytes);

    Ok(writer.into())
}

trait CacheKey: Clone + Eq + Hash {
    fn as_hash(&self) -> AssetHash;
}

trait MemoryCacheValue: Send + Sync + Clone {}

impl<T: Send + Sync + Clone> MemoryCacheValue for T {}

struct MemoryCache<K: CacheKey, V: MemoryCacheValue> {
    key_to_value: HashMap<K, V>,
}

// XXX TODO: Review why we need this manual `Default` implementation instead of
// deriving it. Causes a compile error in `MemoryAndFileCache`.
impl<K: CacheKey, V: MemoryCacheValue> Default for MemoryCache<K, V> {
    fn default() -> Self {
        Self {
            key_to_value: Default::default(),
        }
    }
}

impl<K: CacheKey, V: MemoryCacheValue> MemoryCache<K, V> {
    fn get(&self, key: &K) -> Option<V> {
        self.key_to_value.get(key).cloned()
    }

    fn put(&mut self, key: K, value: V) {
        self.key_to_value.insert(key, value);
    }
}

// XXX TODO: Why do we need this `static` bound to make the async happy?
trait FileCacheValue: Send + Sync + Clone + 'static {
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

struct FileCache<K: CacheKey, V: FileCacheValue> {
    base_path: PathBuf,
    phantom: PhantomData<(K, V)>,
}

impl<K: CacheKey, V: FileCacheValue> FileCache<K, V> {
    fn new(base_path: PathBuf) -> Self {
        Self {
            base_path,
            phantom: PhantomData::<(K, V)>,
        }
    }

    fn value_path(&self, key: &K) -> PathBuf {
        // XXX TODO: Check if this is worth optimising.
        let hex = String::from_iter(key.as_hash().iter().map(|b| format!("{:x}", b)));

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
        let value_path = self.value_path(key);

        let Ok(value) = async_fs::read(value_path).await else {
            info!("{asset_path:?}: File cache miss.");

            return None;
        };

        info!("{asset_path:?}: File cache hit.");

        Some(<V as FileCacheValue>::read(value.into()))
    }

    // XXX TODO: `asset_path` is only for debugging. Maybe make it more opaque?
    fn put(&self, key: K, value: V, asset_path: &AssetRef<'static>) {
        let value_path = self.value_path(&key);
        let asset_path = asset_path.to_owned(); // XXX TODO: Annoying clone when it's only for debugging.
        let value = value.clone(); // XXX TODO: ???

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

                info!("{asset_path:?}: File cache put.");
            })
            .detach();
    }
}

struct MemoryAndFileCache<K: CacheKey, V: FileCacheValue + MemoryCacheValue> {
    memory: Arc<RwLock<MemoryCache<K, V>>>,
    file: Option<FileCache<K, V>>,
}

impl<K: CacheKey, V: FileCacheValue> MemoryAndFileCache<K, V> {
    fn new(file_cache_path: Option<PathBuf>) -> Self {
        MemoryAndFileCache {
            memory: Default::default(),
            file: file_cache_path.map(FileCache::new),
        }
    }

    // XXX TODO: `asset_path` is only for debugging. Maybe make it more opaque?
    async fn get(&self, key: &K, asset_path: &AssetRef<'static>) -> Option<V> {
        if let Some(from_memory) = self
            .memory
            .read()
            .unwrap_or_else(PoisonError::into_inner)
            .get(key)
        {
            info!("{asset_path:?}: Memory cache hit.");

            return Some(from_memory);
        }

        info!("{asset_path:?}: Memory cache miss.");

        if let Some(file) = &self.file {
            let from_file = file.get(key, asset_path).await?;

            self.memory
                .write()
                .unwrap_or_else(PoisonError::into_inner)
                .put(key.clone(), from_file.clone());

            Some(from_file)
        } else {
            None
        }
    }

    // XXX TODO: `asset_path` is only for debugging. Maybe make it more opaque?
    fn put(&self, key: K, value: V, asset_path: &AssetRef<'static>) {
        // XXX TODO: Avoid `blob.clone()` if there's no file cache?
        self.memory
            .write()
            .unwrap_or_else(PoisonError::into_inner)
            .put(key.clone(), value.clone());

        info!("{asset_path:?}: Memory cache put.");

        if let Some(file) = &self.file {
            file.put(key, value, asset_path);
        }
    }
}

#[derive(Hash, Copy, Clone)]
struct ContentHash(AssetHash);

#[derive(Default)]
struct ContentCache {
    path_to_hash: RwLock<HashMap<AssetPath<'static>, ContentHash>>,
}

impl ContentCache {
    // XXX TODO: Can we avoid `AssetPath` being `<'static>`?
    // XXX TODO: Returning AssetHash by value is probabably the practical choice? But check.
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
            info!("{path:?}: Content cache hit.");

            return Ok(*hash);
        }

        info!("{path:?}: Content cache miss.");

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

#[derive(Hash, Copy, Clone, Eq, PartialEq)]
struct DependencyCacheKey(AssetHash);

impl CacheKey for DependencyCacheKey {
    fn as_hash(&self) -> AssetHash {
        self.0
    }
}

#[derive(Default, Serialize, Deserialize)]
struct DependencyList {
    // XXX TODO: Could store some debug info on the input?
    list: Vec<AssetRef<'static>>,
}

impl FileCacheValue for Arc<DependencyList> {
    async fn write(&self, file: &mut File) -> Result<(), BevyError> {
        let string = ron::ser::to_string(self.as_ref()).map_err(BevyError::from)?;

        file.write_all(string.as_bytes())
            .await
            .map_err(BevyError::from)
    }

    fn read(bytes: Box<[u8]>) -> Self {
        Arc::new(ron::de::from_bytes::<DependencyList>(&bytes).expect("TODO"))
    }
}

fn content_hash(bytes: &[u8]) -> ContentHash {
    let mut hasher = blake3::Hasher::new();
    hasher.update(bytes);
    ContentHash(*hasher.finalize().as_bytes())
}

impl CacheKey for ActionCacheKey {
    fn as_hash(&self) -> AssetHash {
        self.0
    }
}

fn apply_settings(settings: Option<&mut dyn Settings>, ron: &Option<Box<ron::value::RawValue>>) {
    let Some(settings) = settings else {
        return;
    };

    let Some(ron) = ron else {
        return;
    };

    // XXX TODO: Need some way to apply a dyn Settings without knowing the type.
    if let Some(settings) = settings.downcast_mut::<demo::StringAssetSettings>() {
        *settings = ron
            .clone()
            .into_rust::<demo::StringAssetSettings>()
            .expect("TODO");
    }
}

async fn load_ref(
    asset_server: &AssetServer,
    path: &AssetRef<'static>,
    shared: &BassetShared,
) -> Result<ErasedLoadedAsset, BevyError> {
    match path {
        AssetRef::Path(path) => load_path(asset_server, path, &None, shared).await, // XXX TODO: Settings?
        AssetRef::Action(action) => load_action(asset_server, action, shared).await,
    }
}

async fn load_path(
    asset_server: &AssetServer,
    path: &AssetPath<'static>,
    settings: &Option<Box<ron::value::RawValue>>,
    shared: &BassetShared,
) -> Result<ErasedLoadedAsset, BevyError> {
    // XXX TODO: Can this be optimised for the case of path being an AssetAction?
    // So we know that it's just going to call ActionLoader.

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

    let asset = asset_server
        .load_with_meta_loader_and_reader(
            &path,
            &*meta,
            &*loader,
            &mut *reader,
            load_dependencies,
            populate_hashes,
        )
        .await
        .map_err(Into::<BevyError>::into)?;

    let dependency_key = dependency_key(&path, shared, asset_server).await;

    //todo!("This should get the action key from asset.keys");

    let immediate_dependee_action_keys = HashMap::default();

    let action_key =
        action_key_from_loaded_dependencies(&dependency_key, &immediate_dependee_action_keys);

    let keys = LoadedAssetKeys {
        action_key,
        immediate_dependee_action_keys,
    };

    let asset = asset.with_keys(Some(keys));

    if let Some(label) = path.label_cow() {
        match asset.take_labeled(label.clone()) {
            Ok(labeled_asset) => Ok(labeled_asset),
            Err(_) => panic!("Couldn't find label \"{}\" in \"{}\".", label, path),
        }
    } else {
        Ok(asset)
    }
}

async fn load_action(
    asset_server: &AssetServer,
    action: &AssetAction2<'static>,
    shared: &BassetShared,
) -> Result<ErasedLoadedAsset, BevyError> {
    // XXX TODO: Avoid? There's a bunch of things that expect an `AssetRef` but
    // maybe could be specialized for an action.
    let path = AssetRef::from(action.clone());

    // XXX TODO: `dependency_key` will be looking up the content hash even though we have the bytes right here. Avoid?
    let dependency_key = dependency_key(&path, shared, asset_server).await;

    let mut apply_context = ApplyContext {
        path: path.clone(), // XXX TODO: Avoid `clone`?
        asset_server,
        immediate_dependee_action_keys: HashMap::default(),
        loader_dependencies: HashMap::default(),
        dependency_key,
        shared,
    };

    // XXX TODO: Proper cache key including dependencies etc.
    let action_key =
        action_key_from_dependency_cache(&path, &dependency_key, shared, asset_server).await;

    if let Some(action_key) = action_key
        && let Some(cached_standalone_asset) = shared.action_cache.get(&action_key, &path).await
    {
        // XXX TODO: Are we correctly updating the loader dependencies?
        // compare against `load_direct` and `BassetActionContext::erased_load`.
        return read_standalone_asset(&cached_standalone_asset, &apply_context).await;
    }

    let asset = shared
        .action(action.name())
        .ok_or_else(|| BevyError::from(format!("Couldn't find action \"{}\")", action.name())))?
        .apply(&mut apply_context, action.params())
        .await?;

    // XXX TODO: At this point we should replace `asset.loader_dependencies`
    // with our own `context.loader_dependencies`.

    if let Some(keys) = asset.keys.as_ref()
        && !keys.immediate_dependee_action_keys.is_empty()
    {
        info!(
            "{:?}: Dependencies = {:?}",
            path, keys.immediate_dependee_action_keys,
        );
    }

    // XXX TODO: This is where we need to work out the real action key
    // if it wasn't found previously.
    if let Some(action_key) = action_key
        && let Some((saver, settings)) = shared.saver(asset.asset_type_name())
    {
        info!("{:?}: Cache put.", &action);

        // XXX TODO: Verify loader matches saver? Need a way to get
        // `AssetSaver::OutputLoader` out of `ErasedAssetSaver`.
        let loader = asset_server
            .get_asset_loader_with_type_name(saver.loader_type_name())
            .await
            .expect("TODO");

        let blob = write_standalone_asset(&asset, &*loader, saver, settings).await?;

        shared.action_cache.put(action_key, blob.into(), &path);
    } else {
        info!(
            "{:?}: Cache ineligible, no saver for \"{}\".",
            path,
            asset.asset_type_name()
        );
    }

    // XXX TODO: Should select sub-asset with `action.label`? But need to work
    // out if the cache holds the full asset or sub-asset (see also `dependency_key`).

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

mod action {
    use super::*;

    pub struct LoadPath;

    #[derive(Serialize, Deserialize, Default)]
    pub struct LoadPathParams {
        pub path: String, // TODO: Should be AssetPath. Avoiding for now to simplify lifetimes.
        #[serde(default)]
        pub loader_settings: Option<Box<ron::value::RawValue>>,
        // TODO
        //loader_name: Option<String>,
    }

    impl BassetAction for LoadPath {
        type Params = LoadPathParams;
        type Error = BevyError;

        async fn apply(
            &self,
            context: &mut ApplyContext<'_>,
            params: &Self::Params,
        ) -> Result<ErasedLoadedAsset, Self::Error> {
            // XXX TODO: Review if treating the asset as a dependee is correct.
            context
                .erased_load_dependee_path(
                    // XXX TODO: Avoid `into_owned`?
                    &AssetPath::parse(&params.path).into_owned(),
                    &params.loader_settings,
                )
                .await
        }
    }

    pub struct JoinStrings;

    #[derive(Serialize, Deserialize, Default)]
    pub struct JoinStringsParams {
        separator: String,
        strings: Vec<AssetRef<'static>>,
    }

    impl BassetAction for JoinStrings {
        type Params = JoinStringsParams;
        type Error = BevyError;

        async fn apply(
            &self,
            context: &mut ApplyContext<'_>,
            params: &Self::Params,
        ) -> Result<ErasedLoadedAsset, Self::Error> {
            let mut strings = Vec::new();

            for path in &params.strings {
                strings.push(context.load_dependee::<demo::StringAsset>(path).await?.0);
            }

            let joined = strings
                .into_iter()
                .reduce(|l, r| l + &params.separator + &r)
                .unwrap_or("".to_owned());

            Ok(LoadedAsset::new_with_dependencies(demo::StringAsset(joined)).into())
        }
    }

    pub struct UppercaseString;

    #[derive(Serialize, Deserialize, Default)]
    pub struct UppercaseStringParams {
        string: AssetRef<'static>,
    }

    impl BassetAction for UppercaseString {
        type Params = UppercaseStringParams;
        type Error = BevyError;

        async fn apply(
            &self,
            context: &mut ApplyContext<'_>,
            params: &Self::Params,
        ) -> Result<ErasedLoadedAsset, Self::Error> {
            let string = demo::StringAsset(
                context
                    .load_dependee::<demo::StringAsset>(&params.string)
                    .await?
                    .0
                    .to_uppercase(),
            );

            Ok(LoadedAsset::new_with_dependencies(string).into())
        }
    }

    /// Creates an `AcmeScene` from a `Gltf`. This does not respect the glTF's
    /// scenes list - it just takes every node.
    pub struct AcmeSceneFromGltf;

    #[derive(Serialize, Deserialize, Default)]
    pub struct AcmeSceneFromGltfParams {
        gltf: AssetRef<'static>,
    }

    impl BassetAction for AcmeSceneFromGltf {
        type Params = AcmeSceneFromGltfParams;
        type Error = BevyError;

        async fn apply(
            &self,
            context: &mut ApplyContext<'_>,
            params: &Self::Params,
        ) -> Result<ErasedLoadedAsset, Self::Error> {
            let gltf = context.erased_load_dependee(&params.gltf).await?;

            let scene = acme::from_gltf(&gltf);

            // XXX TODO: What about dependencies?

            Ok(LoadedAsset::new_with_dependencies(scene).into())
        }
    }

    pub struct MeshletFromMesh;

    #[derive(Serialize, Deserialize, Default)]
    pub struct MeshletFromMeshParams {
        mesh: AssetRef<'static>,
        #[serde(default)]
        vertex_position_quantization_factor: Option<u8>,
    }

    impl MeshletFromMeshParams {
        fn vertex_position_quantization_factor(&self) -> u8 {
            self.vertex_position_quantization_factor
                .unwrap_or(MESHLET_DEFAULT_VERTEX_POSITION_QUANTIZATION_FACTOR)
        }
    }

    impl BassetAction for MeshletFromMesh {
        type Params = MeshletFromMeshParams;
        type Error = BevyError;

        async fn apply(
            &self,
            context: &mut ApplyContext<'_>,
            params: &Self::Params,
        ) -> Result<ErasedLoadedAsset, Self::Error> {
            // TODO: Should we check if `MeshletPlugin` is registered so we can
            // return a sensible error?

            let mesh = context.load_dependee::<Mesh>(&params.mesh).await?;

            let meshlet =
                MeshletMesh::from_mesh(&mesh, params.vertex_position_quantization_factor())?;

            Ok(LoadedAsset::new_with_dependencies(meshlet).into())
        }
    }

    pub struct ConvertAcmeSceneMeshesToMeshlets;

    #[derive(Serialize, Deserialize, Default)]
    pub struct ConvertAcmeSceneMeshesToMeshletsParams {
        scene: AssetRef<'static>,
        #[serde(default)]
        vertex_position_quantization_factor: Option<u8>,
    }

    impl BassetAction for ConvertAcmeSceneMeshesToMeshlets {
        type Params = ConvertAcmeSceneMeshesToMeshletsParams;
        type Error = BevyError;

        async fn apply(
            &self,
            context: &mut ApplyContext<'_>,
            params: &Self::Params,
        ) -> Result<ErasedLoadedAsset, Self::Error> {
            // TODO: Should we check if `MeshletPlugin` is registered so we can
            // return a sensible error?

            let mut scene = context
                .load_dependee::<acme::AcmeScene>(&params.scene)
                .await?;

            for entity in &mut scene.entities {
                if let Some(mesh) = entity.mesh.take() {
                    entity.meshlet_mesh = Some(acme::AcmeMeshletMesh {
                        asset: AssetRef::Action(make_action::<MeshletFromMesh>(
                            &MeshletFromMeshParams {
                                mesh: mesh.asset,
                                vertex_position_quantization_factor: params
                                    .vertex_position_quantization_factor,
                            },
                        )),
                    });
                }
            }

            Ok(LoadedAsset::new_with_dependencies(scene).into())
        }
    }
}

mod demo {
    use bevy_asset::{io::Writer, saver::SavedAsset, AsyncWriteExt};

    use super::*;

    #[derive(Asset, TypePath, Debug)]
    pub struct StringAsset(pub String);

    // TODO: Delete? Originally used to confirm that loader settings work. Probably
    // redundant now?
    #[derive(Serialize, Deserialize, Default)]
    pub struct StringAssetSettings {
        uppercase: bool,
    }

    #[derive(Default)]
    pub struct StringAssetLoader;

    impl AssetLoader for StringAssetLoader {
        type Asset = StringAsset;
        type Settings = StringAssetSettings;
        type Error = std::io::Error;

        async fn load(
            &self,
            reader: &mut dyn Reader,
            settings: &Self::Settings,
            _load_context: &mut LoadContext<'_>,
        ) -> Result<StringAsset, Self::Error> {
            let mut bytes = Vec::new();
            reader.read_to_end(&mut bytes).await?;

            let mut string = String::from_utf8(bytes).expect("TODO");

            if settings.uppercase {
                string = string.to_uppercase();
            }

            Ok(StringAsset(string))
        }

        fn extensions(&self) -> &[&str] {
            &["string"]
        }
    }

    #[derive(Default)]
    pub struct StringAssetSaver;

    impl AssetSaver for StringAssetSaver {
        type Asset = StringAsset;
        type Settings = ();
        type OutputLoader = StringAssetLoader;
        type Error = std::io::Error;

        async fn save(
            &self,
            writer: &mut Writer,
            asset: SavedAsset<'_, Self::Asset>,
            _settings: &Self::Settings,
        ) -> Result<StringAssetSettings, Self::Error> {
            writer.write_all(asset.0.as_bytes()).await?;

            Ok(StringAssetSettings::default())
        }
    }

    #[derive(Asset, TypePath, Debug)]
    pub struct IntAsset(pub i64);

    #[derive(Default)]
    pub struct IntAssetLoader;

    impl AssetLoader for IntAssetLoader {
        type Asset = IntAsset;
        type Settings = ();
        type Error = std::io::Error;

        async fn load(
            &self,
            reader: &mut dyn Reader,
            _: &Self::Settings,
            _load_context: &mut LoadContext<'_>,
        ) -> Result<IntAsset, Self::Error> {
            let mut bytes = Vec::new();
            reader.read_to_end(&mut bytes).await?;
            // TODO: Error handling.
            Ok(IntAsset(
                String::from_utf8(bytes).unwrap().parse::<i64>().unwrap(),
            ))
        }

        fn extensions(&self) -> &[&str] {
            &["int"]
        }
    }

    #[derive(Default)]
    pub struct IntAssetSaver;

    impl AssetSaver for IntAssetSaver {
        type Asset = IntAsset;
        type Settings = ();
        type OutputLoader = IntAssetLoader;
        type Error = std::io::Error;

        async fn save(
            &self,
            writer: &mut Writer,
            asset: SavedAsset<'_, Self::Asset>,
            _settings: &Self::Settings,
        ) -> Result<(), Self::Error> {
            writer.write_all(asset.0.to_string().as_bytes()).await?;

            Ok(())
        }
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

mod acme {
    use super::*;
    use bevy::pbr::experimental::meshlet::MeshletMesh3d;

    #[derive(Serialize, Deserialize, Debug)]
    pub struct AcmeMesh {
        pub asset: AssetRef<'static>,
    }

    #[derive(Serialize, Deserialize, Debug)]
    pub struct AcmeMeshletMesh {
        pub asset: AssetRef<'static>,
    }

    #[derive(Serialize, Deserialize, Debug)]
    pub struct AcmeMaterial {
        pub base_color_texture: Option<AssetRef<'static>>,
    }

    #[derive(Serialize, Deserialize, Default, Debug)]
    pub struct AcmeEntity {
        #[serde(default)]
        pub transform: Transform,

        #[serde(default)]
        pub mesh: Option<AcmeMesh>,

        #[serde(default)]
        pub meshlet_mesh: Option<AcmeMeshletMesh>,

        #[serde(default)]
        pub material: Option<AcmeMaterial>,
    }

    #[derive(Asset, TypePath, Serialize, Deserialize, Default, Debug)]
    pub struct AcmeScene {
        pub entities: Vec<AcmeEntity>,
    }

    fn get_sub_asset<'a, T: Asset>(
        asset: &'a ErasedLoadedAsset,
        sub_asset_handle: &Handle<T>,
    ) -> &'a T {
        asset
            .get_labeled_by_id(sub_asset_handle.into())
            .expect("TODO")
            .get::<T>()
            .expect("TODO")
    }

    pub fn from_gltf(asset: &ErasedLoadedAsset) -> AcmeScene {
        let mut entities = Vec::<AcmeEntity>::new();

        let gltf = asset.get::<Gltf>().expect("TODO");

        let mut stack = gltf
            .nodes
            .iter()
            .map(|node_handle| {
                let node = get_sub_asset(asset, node_handle);

                (node, node.transform)
            })
            .collect::<Vec<_>>();

        loop {
            let Some((node, transform)) = stack.pop() else {
                break;
            };

            if let Some(mesh) = node
                .mesh
                .as_ref()
                .map(|mesh_handle| get_sub_asset(asset, mesh_handle))
            {
                for primitive in mesh.primitives.iter() {
                    let mesh = Some(AcmeMesh {
                        asset: primitive.mesh.path().expect("TODO").clone(),
                    });

                    let standard_material =
                        get_sub_asset(asset, primitive.material.as_ref().expect("TODO"));

                    let material = Some(AcmeMaterial {
                        base_color_texture: standard_material
                            .base_color_texture
                            .clone()
                            .map(|p| p.path().expect("TODO").clone()),
                    });

                    entities.push(AcmeEntity {
                        transform,
                        mesh,
                        material,
                        ..Default::default()
                    });
                }
            }

            for child in node
                .children
                .iter()
                .map(|child_handle| get_sub_asset(asset, child_handle))
            {
                stack.push((child, transform * child.transform));
            }
        }

        AcmeScene { entities }
    }

    pub fn spawn(
        commands: &mut Commands,
        scene: &AcmeScene,
        parent_entity: Option<Entity>,
        asset_server: &AssetServer,
        standard_material_assets: &mut Assets<StandardMaterial>,
        meshlet_debug_material_assets: &mut Assets<MeshletDebugMaterial>,
    ) {
        for scene_entity in &scene.entities {
            let mut world_entity = commands.spawn(scene_entity.transform);

            // XXX TODO: Currently this forces the debug material for meshlets.
            // Should change that to be a scene conversion action. AcmeMaterial
            // will become an enum of standard/debug materials.

            if scene_entity.meshlet_mesh.is_some() {
                world_entity.insert(MeshMaterial3d(
                    meshlet_debug_material_assets.add(MeshletDebugMaterial::default()),
                ));
            } else if let Some(material) = &scene_entity.material {
                let standard_material = StandardMaterial {
                    base_color_texture: material
                        .base_color_texture
                        .as_ref()
                        .map(|p| asset_server.load::<Image>(p)),
                    ..Default::default()
                };

                world_entity.insert(MeshMaterial3d(
                    standard_material_assets.add(standard_material),
                ));
            }

            if let Some(mesh) = &scene_entity.mesh {
                world_entity.insert(Mesh3d(asset_server.load::<Mesh>(&mesh.asset)));
            }

            if let Some(meshlet_mesh) = &scene_entity.meshlet_mesh {
                world_entity.insert(MeshletMesh3d(
                    asset_server.load::<MeshletMesh>(&meshlet_mesh.asset),
                ));
            }

            if let Some(parent_entity) = parent_entity {
                world_entity.insert(ChildOf(parent_entity));
            }
        }
    }

    #[derive(Component)]
    pub struct AcmeSceneSpawner(pub Handle<AcmeScene>);

    // XXX TODO: This is currently used to keep the handle alive long enough that
    // the asset loaded events can be printed. Rethink?
    #[derive(Component)]
    #[expect(dead_code, reason = "TODO")]
    pub struct AcmeSceneInstance(pub Handle<AcmeScene>);

    pub fn tick_scene_spawners(
        mut commands: Commands,
        spawners: Query<(Entity, &AcmeSceneSpawner)>,
        scene_assets: Res<Assets<AcmeScene>>,
        asset_server: Res<AssetServer>,
        mut standard_material_assets: ResMut<Assets<StandardMaterial>>,
        mut meshlet_debug_material_assets: ResMut<Assets<MeshletDebugMaterial>>,
    ) {
        for (entity, spawner) in spawners {
            let Some(scene_asset) = scene_assets.get(&spawner.0) else {
                continue;
            };

            commands
                .entity(entity)
                .insert(AcmeSceneInstance(spawner.0.clone()));

            commands.entity(entity).remove::<AcmeSceneSpawner>();

            spawn(
                &mut commands,
                scene_asset,
                Some(entity),
                &asset_server,
                &mut standard_material_assets,
                &mut meshlet_debug_material_assets,
            );
        }
    }
}

#[derive(Asset, TypePath, AsBindGroup, Clone, Default)]
struct MeshletDebugMaterial {
    _dummy: (),
}

impl Material for MeshletDebugMaterial {}

#[derive(Resource)]
struct Handles(Vec<UntypedHandle>);

fn setup(
    mut commands: Commands,
    asset_server: Res<AssetServer>,
    mut meshes: ResMut<Assets<Mesh>>,
    mut materials: ResMut<Assets<StandardMaterial>>,
) {
    // XXX TODO
    //let _inline_path = BassetPathSerializable::Path("1234.int".into()).to_asset_path();

    commands.insert_resource(Handles(vec![
        asset_server
            .load::<demo::StringAsset>("hello.string")
            .untyped(),
        asset_server
            .load::<demo::StringAsset>("world.string")
            .untyped(),
        asset_server.load::<demo::IntAsset>("1234.int").untyped(),
        asset_server.load::<demo::IntAsset>("int.basset").untyped(),
        asset_server
            .load::<demo::StringAsset>("string.basset")
            .untyped(),
        asset_server
            .load::<demo::StringAsset>("string_loader_uppercase.basset")
            .untyped(),
        asset_server
            .load::<demo::StringAsset>("join_strings.basset")
            .untyped(),
        //asset_server.load::<demo::IntAsset>(inline_path).untyped(),
    ]));

    commands.spawn((
        acme::AcmeSceneSpawner(asset_server.load::<acme::AcmeScene>("scene_from_gltf.basset")),
        Transform::from_xyz(-100.0, 0.0, 0.0)
            .looking_to(Dir3::new(vec3(1.0, 0.0, 2.0)).unwrap(), Vec3::Y),
    ));

    commands.spawn((
        acme::AcmeSceneSpawner(asset_server.load::<acme::AcmeScene>("meshlet_scene.basset")),
        Transform::from_xyz(100.0, 0.0, 0.0)
            .looking_to(Dir3::new(vec3(1.0, 0.0, 2.0)).unwrap(), Vec3::Y),
    ));

    commands.spawn((
        Camera3d::default(),
        Transform::from_xyz(-30.0, 200.0, 300.0).looking_at(Vec3::new(-30.0, 75.0, 0.0), Vec3::Y),
        CameraController {
            walk_speed: 200.0,
            ..Default::default()
        },
        // Meshlets are incompatible with MSAA.
        #[cfg(feature = "meshlet")]
        Msaa::Off,
    ));

    commands.spawn((
        Mesh3d(meshes.add(Plane3d::default().mesh().size(500000.0, 500000.0))),
        MeshMaterial3d(materials.add(Color::srgb(0.3, 0.5, 0.3))),
    ));

    commands.spawn((
        Transform::from_xyz(100.0, 200.0, 200.0).looking_at(Vec3::new(0.0, 0.0, 0.0), Vec3::Y),
        DirectionalLight {
            shadows_enabled: true,
            ..default()
        },
        CascadeShadowConfigBuilder {
            num_cascades: 1,
            maximum_distance: 600.0,
            ..default()
        }
        .build(),
    ));
}

fn print_events<T: Asset + std::fmt::Debug>(
    asset_server: &AssetServer,
    assets: &Assets<T>,
    events: &mut MessageReader<AssetEvent<T>>,
) {
    for event in events.read() {
        match *event {
            AssetEvent::Added { id } | AssetEvent::Modified { id } => {
                info!(
                    "{:?}: Value = {:?}",
                    asset_server.get_path(id).unwrap(),
                    assets.get(id).unwrap()
                );
            }
            _ => (),
        }
    }
}

fn print(
    asset_server: Res<AssetServer>,
    string_assets: Res<Assets<demo::StringAsset>>,
    int_assets: Res<Assets<demo::IntAsset>>,
    scene_assets: Res<Assets<acme::AcmeScene>>,
    mut string_events: MessageReader<AssetEvent<demo::StringAsset>>,
    mut int_events: MessageReader<AssetEvent<demo::IntAsset>>,
    mut scene_events: MessageReader<AssetEvent<acme::AcmeScene>>,
) {
    print_events(&asset_server, &string_assets, &mut string_events);
    print_events(&asset_server, &int_assets, &mut int_events);
    print_events(&asset_server, &scene_assets, &mut scene_events);
}

fn reload(asset_server: Res<AssetServer>, handles: Res<Handles>, mut done: Local<bool>) {
    if *done {
        return;
    }

    *done = true;

    info!("RELOADING");

    for handle in &handles.0 {
        asset_server.reload(handle.path().expect("TODO"));
    }
}

fn make_action<A: BassetAction>(params: &<A as BassetAction>::Params) -> AssetAction2<'static> {
    AssetAction2::new(
        core::any::type_name::<A>().into(),
        ron::value::RawValue::from_rust(params).expect("TODO"),
        None,
    )
}

fn main() {
    dbg!(ron::ser::to_string(&AssetRef::Path("asdf.txt".into())).expect("TODO"));
    dbg!(
        ron::ser::to_string(&AssetRef::Action(make_action::<action::LoadPath>(
            &action::LoadPathParams {
                path: "asdf.txt".into(),
                ..Default::default()
            }
        )))
        .expect("TODO")
    );
    dbg!(ron::de::from_str::<AssetRef>("Path(\"asdf.txt\")").expect("TODO"));
    dbg!(ron::de::from_str::<AssetRef>("Action((name: \"foo\", params: ()))").expect("TODO"));

    dbg!(serde_json::ser::to_string(&AssetRef::Path("asdf.txt".into())).expect("TODO"));
    dbg!(
        serde_json::ser::to_string(&AssetRef::Action(make_action::<action::LoadPath>(
            &action::LoadPathParams {
                path: "asdf.txt".into(),
                ..Default::default()
            }
        )))
        .expect("TODO")
    );

    App::new()
        .add_plugins((
            DefaultPlugins.set(AssetPlugin {
                file_path: "examples/asset/basset/assets".to_string(),
                ..default()
            }),
            BassetPlugin,
            MaterialPlugin::<MeshletDebugMaterial>::default(),
            CameraControllerPlugin,
            MeshletPlugin {
                cluster_buffer_slots: 1 << 14,
            },
        ))
        .add_systems(Startup, setup)
        .add_systems(Update, print)
        .add_systems(Update, reload.run_if(on_timer(Duration::from_secs(2))))
        .run();
}
