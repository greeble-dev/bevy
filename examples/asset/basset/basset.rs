//! Basset proof of concept.

#[path = "../../helpers/camera_controller.rs"]
mod camera_controller;

use bevy::{
    asset::{
        io::Reader,
        meta::{AssetAction, AssetHash, AssetMeta, AssetMetaDyn, Settings},
        saver::{AssetSaver, ErasedAssetSaver},
        AssetLoader, AssetPath, DeserializeMetaError, ErasedAssetLoader, ErasedLoadedAsset,
        InlineBasset, LoadContext, LoadedAsset,
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
use bevy_asset::{io::SliceReader, AsyncWriteExt};
use camera_controller::{CameraController, CameraControllerPlugin};
use core::result::Result;
use downcast_rs::{impl_downcast, Downcast};
use serde::{Deserialize, Serialize};
use std::{boxed::Box, io::Write, path::PathBuf, sync::Arc, time::Duration};

struct BassetPlugin;

impl Plugin for BassetPlugin {
    fn build(&self, app: &mut App) {
        app.init_asset::<demo::StringAsset>()
            .init_asset::<demo::IntAsset>()
            .init_asset::<acme::AcmeScene>()
            .register_asset_loader(demo::StringAssetLoader)
            .register_asset_loader(demo::IntAssetLoader)
            .register_erased_asset_loader(Box::new(
                BassetLoader::new(Some("examples/asset/basset/cache".into()))
                    .with_action(action::LoadPath)
                    .with_action(action::JoinStrings)
                    .with_action(action::UppercaseString)
                    .with_action(action::AcmeSceneFromGltf)
                    .with_action(action::MeshletFromMesh)
                    .with_action(action::ConvertAcmeSceneMeshesToMeshlets)
                    .with_saver(demo::StringAssetSaver)
                    .with_saver(demo::IntAssetSaver),
            ))
            .add_systems(Update, acme::tick_scene_spawners);
    }
}

/// XXX TODO: Document.
pub struct BassetActionContext<'a> {
    loader: &'a BassetLoader,
    asset_server: &'a AssetServer,
    loader_dependencies: HashMap<AssetPath<'static>, AssetHash>,
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
        context: &mut BassetActionContext<'_>,
        params: &Self::Params,
    ) -> impl ConditionalSendFuture<Output = Result<ErasedLoadedAsset, Self::Error>>;
}

impl BassetActionContext<'_> {
    async fn apply<T: Asset>(&mut self, action: &BassetPathSerializable) -> Result<T, BevyError> {
        match self.erased_apply(action).await?.downcast::<T>() {
            Ok(result) => Ok(result.take()),
            Err(original) => panic!(
                "Should have made type {}, actually made type {}. Action: {:?}",
                core::any::type_name::<T>(),
                original.asset_type_name(),
                action,
            ),
        }
    }

    async fn erased_apply(
        &mut self,
        action: &BassetPathSerializable,
    ) -> Result<ErasedLoadedAsset, BevyError> {
        match action {
            BassetPathSerializable::Path(path) => {
                self.erased_load(AssetPath::parse(path).into_owned(), &None)
                    .await
            }
            BassetPathSerializable::Action { name, params } => {
                self.loader
                    .action(name)
                    .ok_or_else(|| BevyError::from(format!("Couldn't find action \"{name}\")")))?
                    .apply(self, params)
                    .await
            }
        }
    }

    async fn erased_load(
        &mut self,
        path: AssetPath<'static>,
        settings: &Option<Box<ron::value::RawValue>>,
    ) -> Result<ErasedLoadedAsset, BevyError> {
        let (asset, hash) = load_direct(self.asset_server, &path, settings).await?;

        self.loader_dependencies.insert(path, hash);

        Ok(asset)
    }
}

trait ErasedBassetAction: Send + Sync + 'static {
    fn apply<'a>(
        &'a self,
        context: &'a mut BassetActionContext,
        params: &'a ron::value::RawValue,
    ) -> BoxedFuture<'a, Result<ErasedLoadedAsset, BevyError>>;
}

impl<T> ErasedBassetAction for T
where
    T: BassetAction + Send + Sync,
{
    fn apply<'a>(
        &'a self,
        context: &'a mut BassetActionContext,
        params: &'a ron::value::RawValue,
    ) -> BoxedFuture<'a, Result<ErasedLoadedAsset, BevyError>> {
        // TODO: Check that we're correctly using BoxedFuture and Box::pin.
        Box::pin(async move {
            let params = params.into_rust::<T::Params>().expect("TODO");

            T::apply(self, context, &params).await.map_err(Into::into)
        })
    }
}

/// XXX TODO: Document.
#[derive(Serialize, Deserialize, Debug, Clone)]
pub enum BassetPathSerializable {
    /// XXX TODO: Document.
    Path(String),

    /// XXX TODO: Document.
    Action {
        /// XXX TODO: Document.
        name: String,

        /// XXX TODO: Document.
        /// XXX TODO: If we split this into serializable and deserializable forms
        /// then the deserializable can use a reference instead of a box. See
        /// `serde_json::RawValue` example. But that's awkward for action params -
        /// don't want to make them split types too. Maybe can do an enum with
        /// a custom serialize?
        params: Box<ron::value::RawValue>,
    },
}

// TODO: Would prefer to avoid Default, but might be necessary for serialization?
impl Default for BassetPathSerializable {
    fn default() -> Self {
        Self::Path("".into())
    }
}

impl BassetPathSerializable {
    /// XXX TODO: Document.
    pub fn from_action<A: BassetAction>(params: &<A as BassetAction>::Params) -> Self {
        Self::Action {
            name: core::any::type_name::<A>().into(),
            params: ron::value::RawValue::from_rust(params).expect("TODO"),
        }
    }

    /// XXX TODO: Document.
    pub fn from_handle(handle: &UntypedHandle) -> Self {
        // TODO: Avoid the `into_owned`? Not sure why skipping that causes lifetime
        // issues for `asset_server`.
        let path = handle
            .path()
            .expect("TODO - there are valid cases where this can fail, e.g. uuid handles")
            .clone();

        Self::from_asset_path(&path)
    }

    /// XXX TODO: Document.
    pub fn from_asset_path(asset_path: &AssetPath<'static>) -> Self {
        if let Some(inline_basset) = asset_path.inline_basset() {
            // XXX TODO: We're losing information here if there's something other
            // than `basset::root`, e.g. versioning.

            let basset =
                ron::de::from_str::<BassetFileSerializable>(inline_basset.value()).expect("TODO");

            basset.root
        } else {
            Self::Path(asset_path.to_string())
        }
    }

    /// XXX TODO: Replace with From/To impl?
    pub fn to_asset_path(&self) -> AssetPath<'static> {
        // XXX TODO: Versioning? Need a more robust way to deal with the difference
        // between BassetFileSerializable and BassetPathSerializable.
        AssetPath::from_basset(InlineBasset::new(
            ron::ser::to_string(&BassetFileSerializable { root: self.clone() })
                .expect("TODO")
                .into(),
        ))
    }
}

#[derive(Serialize, Deserialize, Debug, Clone)]
struct BassetFileSerializable {
    root: BassetPathSerializable,
    // TODO: Versioning?
}

struct BassetLoader {
    action_type_name_to_action: HashMap<&'static str, Box<dyn ErasedBassetAction>>,
    asset_type_name_to_saver: HashMap<&'static str, (Box<dyn ErasedAssetSaver>, Box<dyn Settings>)>,
    cache: BassetActionCache,
}

impl BassetLoader {
    fn new(file_cache_path: Option<PathBuf>) -> Self {
        Self {
            cache: BassetActionCache::new(file_cache_path),
            action_type_name_to_action: Default::default(),
            asset_type_name_to_saver: Default::default(),
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

impl ErasedAssetLoader for BassetLoader {
    fn load<'a>(
        &'a self,
        reader: &'a mut dyn Reader,
        _meta: &'a dyn AssetMetaDyn,
        load_context: LoadContext<'a>,
    ) -> BoxedFuture<'a, Result<ErasedLoadedAsset, BevyError>> {
        // TODO: Check that we're correctly using BoxedFuture and Box::pin.
        Box::pin(async move {
            let mut bytes = Vec::new();
            reader.read_to_end(&mut bytes).await?;

            // XXX TODO: Should include all inputs, versions etc.
            let input_hash = hash_bytes(&bytes);

            let mut context = BassetActionContext {
                loader: self,
                asset_server: load_context.asset_server(),
                loader_dependencies: HashMap::default(),
            };

            if let Some(cached_standalone_asset) =
                self.cache.get(&input_hash, load_context.asset_path()).await
            {
                // XXX TODO: Are we correctly updating the loader dependencies?
                // compare against `load_direct` and `BassetActionContext::erased_load`.
                return read_standalone_asset(&cached_standalone_asset, &context).await;
            }

            let basset = ron::de::from_bytes::<BassetFileSerializable>(&bytes)?;

            let asset = context.erased_apply(&basset.root).await?;

            // XXX TODO: At this point we should replace `asset.loader_dependencies`
            // with our own `context.loader_dependencies`.

            if !context.loader_dependencies.is_empty() {
                info!(
                    "{:?}: Dependencies = [{}]",
                    load_context.asset_path(),
                    &context
                        .loader_dependencies
                        .keys()
                        .map(|p| p.to_string())
                        .reduce(|l, r| l + &", " + &r)
                        .unwrap()
                );
            }

            if let Some((saver, settings)) = self.saver(asset.asset_type_name()) {
                info!("{:?}: Cache put.", load_context.asset_path());

                // XXX TODO: Verify loader matches saver? Need a way to get
                // `AssetSaver::OutputLoader` out of `ErasedAssetSaver`.
                let loader = load_context
                    .asset_server()
                    .get_asset_loader_with_type_name(saver.loader_type_name())
                    .await
                    .expect("TODO");

                let blob = write_standalone_asset(&asset, &*loader, saver, settings).await?;

                self.cache
                    .put(&input_hash, blob.into(), load_context.asset_path());
            } else {
                info!(
                    "{:?}: Cache ineligible, no saver for \"{}\".",
                    load_context.asset_path(),
                    asset.asset_type_name()
                );
            }

            Ok(asset)
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
        core::any::type_name::<BassetLoader>()
    }

    fn type_id(&self) -> std::any::TypeId {
        core::any::TypeId::of::<BassetLoader>()
    }

    fn asset_type_name(&self) -> Option<&'static str> {
        None
    }

    fn asset_type_id(&self) -> Option<std::any::TypeId> {
        None
    }
}

const STANDALONE_MAGIC: &[u8] = b"BEVY_STANDALONE_ASSET\n";
const STANDALONE_VERSION: u16 = 1;

async fn read_standalone_asset(
    blob: &[u8],
    context: &BassetActionContext<'_>,
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
    let fake_path = AssetPath::parse("fake_path_from_basset_read_binary_asset");

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

#[derive(Default)]
struct BassetActionMemoryCache {
    hash_to_blob: HashMap<AssetHash, Arc<[u8]>>,
}

impl BassetActionMemoryCache {
    // XXX TODO: Is this lifetime necessary?
    fn get<'a>(&'a self, hash: &AssetHash) -> Option<Arc<[u8]>> {
        self.hash_to_blob.get(hash).map(Clone::clone)
    }

    fn put(&mut self, hash: &AssetHash, blob: Arc<[u8]>) {
        self.hash_to_blob.insert(*hash, blob);
    }
}

struct BassetActionFileCache {
    path: PathBuf,
}

impl BassetActionFileCache {
    fn hash_path(&self, hash: &AssetHash) -> PathBuf {
        // XXX TODO: Duplicates code in `AssetPath::from_basset`.
        let hex = String::from_iter(hash.iter().map(|b| format!("{:x}", b)));

        // Spread files across multiple folders in case we're on a filesystem
        // that doesn't like lots of files in a single folder.
        //
        // XXX TODO: Maybe unnecessary?
        let hash_path = [&hex[0..7], &hex[8..15], &hex[16..23], &hex[..]]
            .iter()
            .collect::<PathBuf>();

        self.path.join(hash_path)
    }
}

struct BassetActionCache {
    memory: Arc<RwLock<BassetActionMemoryCache>>,
    file: Option<BassetActionFileCache>,
}

impl BassetActionCache {
    fn new(file_cache_path: Option<PathBuf>) -> Self {
        BassetActionCache {
            memory: Default::default(),
            file: file_cache_path.map(|path| BassetActionFileCache { path }),
        }
    }

    // XXX TODO: `asset_path` is only for debugging. Maybe make it more opaque?
    async fn get(&self, hash: &AssetHash, asset_path: &AssetPath<'static>) -> Option<Arc<[u8]>> {
        if let Some(from_memory) = self
            .memory
            .read()
            .unwrap_or_else(PoisonError::into_inner)
            .get(hash)
        {
            info!("{asset_path:?}: Memory cache hit.");

            return Some(from_memory);
        }

        info!("{asset_path:?}: Memory cache miss.");

        let Some(file_cache) = &self.file else {
            return None;
        };

        let path = file_cache.hash_path(hash);

        let Ok(from_file) = async_fs::read(path).await else {
            info!("{asset_path:?}: File cache miss.");

            return None;
        };

        info!("{asset_path:?}: File cache hit.");

        let from_file = Into::<Arc<[u8]>>::into(from_file);

        self.memory
            .write()
            .unwrap_or_else(PoisonError::into_inner)
            .put(hash, from_file.clone());

        Some(from_file)
    }

    // XXX TODO: `asset_path` is only for debugging. Maybe make it more opaque?
    fn put(&self, hash: &AssetHash, blob: Arc<[u8]>, asset_path: &AssetPath<'static>) {
        self.memory
            .write()
            .unwrap_or_else(PoisonError::into_inner)
            .put(hash, blob.clone());

        info!("{asset_path:?}: Memory cache put.");

        let Some(file_cache) = &self.file else {
            return;
        };

        let path = file_cache.hash_path(hash);
        let asset_path = asset_path.to_owned(); // XXX TODO: Annoying clone when it's only for debugging.

        IoTaskPool::get()
            .spawn(async move {
                async_fs::create_dir_all(path.parent().expect("TODO"))
                    .await
                    .expect("TODO");

                let temp_path = path.with_extension("tmp");

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

                temp_file.write_all(&blob).await.expect("TODO");

                async_fs::rename(temp_path, path)
                    .await
                    .expect("TODO, this is ok to fail?");

                info!("{asset_path:?}: File cache put.");
            })
            .detach();
    }
}

fn hash_bytes(bytes: &[u8]) -> AssetHash {
    let mut hasher = blake3::Hasher::new();
    hasher.update(bytes);
    *hasher.finalize().as_bytes()
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

async fn load_direct(
    asset_server: &AssetServer,
    path: &AssetPath<'static>,
    settings: &Option<Box<ron::value::RawValue>>,
) -> Result<(ErasedLoadedAsset, AssetHash), BevyError> {
    let (mut meta, loader, mut reader) = asset_server
        .get_meta_loader_and_reader(path, None)
        .await
        .map_err(Into::<BevyError>::into)?;

    apply_settings(meta.loader_settings_mut(), settings);

    // Roughly the same as LoadContext::load_direct_internal.

    let load_dependencies = false;
    let populate_hashes = false;

    let asset = asset_server
        .load_with_meta_loader_and_reader(
            path,
            &*meta,
            &*loader,
            &mut *reader,
            load_dependencies,
            populate_hashes,
        )
        .await
        .map_err(Into::<BevyError>::into)?;

    // XXX TODO: Is this correct for labeled assets?
    let info = meta.processed_info().as_ref();
    let hash = info.map(|i| i.full_hash).unwrap_or_default();

    if let Some(label) = path.label_cow() {
        match asset.take_labeled(label.clone()) {
            Ok(labeled_asset) => Ok((labeled_asset, hash)),
            Err(_) => panic!("Couldn't find label \"{}\" in \"{}\".", label, path),
        }
    } else {
        Ok((asset, hash))
    }
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
            context: &mut BassetActionContext<'_>,
            params: &Self::Params,
        ) -> Result<ErasedLoadedAsset, Self::Error> {
            context
                .erased_load(
                    AssetPath::parse(&params.path).into_owned(),
                    &params.loader_settings,
                )
                .await
        }
    }

    pub struct JoinStrings;

    #[derive(Serialize, Deserialize, Default)]
    pub struct JoinStringsParams {
        separator: String,
        strings: Vec<BassetPathSerializable>,
    }

    impl BassetAction for JoinStrings {
        type Params = JoinStringsParams;
        type Error = BevyError;

        async fn apply(
            &self,
            context: &mut BassetActionContext<'_>,
            params: &Self::Params,
        ) -> Result<ErasedLoadedAsset, Self::Error> {
            let mut strings = Vec::new();

            for action in &params.strings {
                strings.push(context.apply::<demo::StringAsset>(action).await?.0);
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
        string: BassetPathSerializable,
    }

    impl BassetAction for UppercaseString {
        type Params = UppercaseStringParams;
        type Error = BevyError;

        async fn apply(
            &self,
            context: &mut BassetActionContext<'_>,
            params: &Self::Params,
        ) -> Result<ErasedLoadedAsset, Self::Error> {
            let string = demo::StringAsset(
                context
                    .apply::<demo::StringAsset>(&params.string)
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
        gltf: BassetPathSerializable,
    }

    impl BassetAction for AcmeSceneFromGltf {
        type Params = AcmeSceneFromGltfParams;
        type Error = BevyError;

        async fn apply(
            &self,
            context: &mut BassetActionContext<'_>,
            params: &Self::Params,
        ) -> Result<ErasedLoadedAsset, Self::Error> {
            let gltf = context.erased_apply(&params.gltf).await?;

            let scene = acme::from_gltf(&gltf, context.asset_server);

            // XXX TODO: What about dependencies?

            Ok(LoadedAsset::new_with_dependencies(scene).into())
        }
    }

    pub struct MeshletFromMesh;

    #[derive(Serialize, Deserialize, Default)]
    pub struct MeshletFromMeshParams {
        mesh: BassetPathSerializable,
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
            context: &mut BassetActionContext<'_>,
            params: &Self::Params,
        ) -> Result<ErasedLoadedAsset, Self::Error> {
            // TODO: Should we check if `MeshletPlugin` is registered so we can
            // return a sensible error?

            let mesh = context.apply::<Mesh>(&params.mesh).await?;

            let meshlet =
                MeshletMesh::from_mesh(&mesh, params.vertex_position_quantization_factor())?;

            Ok(LoadedAsset::new_with_dependencies(meshlet).into())
        }
    }

    pub struct ConvertAcmeSceneMeshesToMeshlets;

    #[derive(Serialize, Deserialize, Default)]
    pub struct ConvertAcmeSceneMeshesToMeshletsParams {
        scene: BassetPathSerializable,
        #[serde(default)]
        vertex_position_quantization_factor: Option<u8>,
    }

    impl BassetAction for ConvertAcmeSceneMeshesToMeshlets {
        type Params = ConvertAcmeSceneMeshesToMeshletsParams;
        type Error = BevyError;

        async fn apply(
            &self,
            context: &mut BassetActionContext<'_>,
            params: &Self::Params,
        ) -> Result<ErasedLoadedAsset, Self::Error> {
            // TODO: Should we check if `MeshletPlugin` is registered so we can
            // return a sensible error?

            let mut scene = context.apply::<acme::AcmeScene>(&params.scene).await?;

            for entity in &mut scene.entities {
                if let Some(mesh) = entity.mesh.take() {
                    entity.meshlet_mesh = Some(acme::AcmeMeshletMesh {
                        asset: BassetPathSerializable::from_action::<MeshletFromMesh>(
                            &MeshletFromMeshParams {
                                mesh: mesh.asset,
                                vertex_position_quantization_factor: params
                                    .vertex_position_quantization_factor,
                            },
                        ),
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
        pub asset: BassetPathSerializable,
    }

    #[derive(Serialize, Deserialize, Debug)]
    pub struct AcmeMeshletMesh {
        pub asset: BassetPathSerializable,
    }

    #[derive(Serialize, Deserialize, Debug)]
    pub struct AcmeMaterial {
        pub base_color_texture: Option<BassetPathSerializable>,
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
        asset_server: &'a AssetServer,
    ) -> &'a T {
        let label = asset_server
            .get_path(sub_asset_handle.id().untyped())
            .expect("TODO")
            .label_cow()
            .expect("TODO");

        asset
            .get_labeled(label.into_owned())
            .expect("TODO")
            .get::<T>()
            .expect("TODO")
    }

    pub fn from_gltf(asset: &ErasedLoadedAsset, asset_server: &AssetServer) -> AcmeScene {
        let mut entities = Vec::<AcmeEntity>::new();

        let gltf = asset.get::<Gltf>().expect("TODO");

        let mut stack = gltf
            .nodes
            .iter()
            .map(|node_handle| {
                let node = get_sub_asset(asset, node_handle, asset_server);

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
                .map(|mesh_handle| get_sub_asset(asset, mesh_handle, asset_server))
            {
                for primitive in mesh.primitives.iter() {
                    let mesh = Some(AcmeMesh {
                        asset: BassetPathSerializable::from_handle(
                            &primitive.mesh.clone().untyped(),
                        ),
                    });

                    let standard_material = get_sub_asset(
                        asset,
                        primitive.material.as_ref().expect("TODO"),
                        asset_server,
                    );

                    let material = Some(AcmeMaterial {
                        base_color_texture: standard_material
                            .base_color_texture
                            .clone()
                            .map(|p| BassetPathSerializable::from_handle(&p.untyped())),
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
                .map(|child_handle| get_sub_asset(asset, child_handle, asset_server))
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
                        .map(|p| asset_server.load::<Image>(p.to_asset_path())),
                    ..Default::default()
                };

                world_entity.insert(MeshMaterial3d(
                    standard_material_assets.add(standard_material),
                ));
            }

            if let Some(mesh) = &scene_entity.mesh {
                world_entity.insert(Mesh3d(
                    asset_server.load::<Mesh>(mesh.asset.to_asset_path()),
                ));
            }

            if let Some(meshlet_mesh) = &scene_entity.meshlet_mesh {
                world_entity.insert(MeshletMesh3d(
                    asset_server.load::<MeshletMesh>(meshlet_mesh.asset.to_asset_path()),
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
    let inline_path = BassetPathSerializable::Path("1234.int".into()).to_asset_path();

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
        asset_server.load::<demo::IntAsset>(inline_path).untyped(),
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

fn main() {
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
