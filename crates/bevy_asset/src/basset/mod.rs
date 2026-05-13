//! Basset proof of concept.

use crate::{
    basset::{
        action::{LoadPath, LoadPathFunction},
        cache::{
            ActionCacheKey, CacheLoaderDependency, DependencyCacheKey, DependencyCacheValue,
            MemoryAndFileCache,
        },
        dependency_graph::DependencyGraph,
        publisher::{
            write_pack_file, PublishDependency, PublishInput, ReadablePackFile, StagedAsset,
            WritablePackFile,
        },
        standalone::{
            load_standalone_asset, read_standalone_asset, save_standalone_asset,
            write_standalone_asset, StandaloneAssetData,
        },
    },
    io::{AssetReaderError, AssetSourceId, AssetSources},
    meta::{AssetActionMinimal, AssetHash, AssetMetaMinimal},
    saver::{ErasedPolyAssetSaver, ErasedUniAssetSaver, PolyAssetSaver},
    Asset, AssetApp, AssetDependency, AssetLoadError, AssetLoaderError, AssetPath, AssetServer,
    ErasedAssetLoader, Handle, LoaderDependency, PolyAssetLoader, ReadAssetBytesError,
};
use alloc::{
    boxed::Box,
    string::{String, ToString},
    sync::Arc,
    vec::Vec,
};
use atomicow::CowArc;
use bevy_app::{App, Plugin};
use bevy_asset::{
    io::Reader,
    meta::Settings,
    saver::{AssetSaver, ErasedAssetSaver},
    AssetRef, ErasedLoadedAsset, LoadContext, LoadedAsset,
};
use bevy_ecs::{error::BevyError, reflect::AppTypeRegistry, world::FromWorld};
use bevy_platform::collections::{HashMap, HashSet};
use bevy_reflect::{
    reflect_trait,
    serde::{
        DeserializeWithRegistry, ReflectDeserializeWithRegistry, ReflectDeserializer,
        ReflectSerializeWithRegistry, ReflectSerializer, SerializeWithRegistry,
        TypedReflectDeserializer,
    },
    PartialReflect, Reflect, ReflectDeserialize, ReflectFromReflect, ReflectSerialize, TypeInfo,
    TypePath, TypeRegistry, TypeRegistryArc,
};
use bevy_tasks::{BoxedFuture, ConditionalSendFuture};
use core::{
    any::{type_name, TypeId},
    fmt::{Debug, Display},
    hash::{Hash, Hasher},
    ops::Deref,
    panic::AssertUnwindSafe,
    result::Result,
};
use downcast_rs::{impl_downcast, Downcast};
use futures_lite::FutureExt;
use serde::{
    de::{DeserializeSeed, MapAccess, SeqAccess, Visitor},
    ser::SerializeStruct,
    Deserialize, Deserializer, Serialize, Serializer,
};
use std::{
    format,
    path::{Path, PathBuf},
    time::Instant,
};
use thiserror::Error;
use tracing::{debug, info, warn};

mod blob;
pub mod cache;
mod dependency_graph;
pub mod publisher;
pub mod standalone;

// XXX TODO: Is this necessary any more? Maybe just fold into `AssetPlugin`.
pub struct BassetPlugin;

impl Plugin for BassetPlugin {
    fn build(&self, app: &mut App) {
        app.init_poly_asset_loader::<BassetLoader>();
    }
}

/// An `AssetPath` without a label.
//
// XXX TODO: Maybe think more about the name. "root asset" does match other parts
// of the asset system, but it's a bit ambiguous. E.g. publishing wants a list
// of "root assets", but they're the roots of the publishing tree.
#[derive(Eq, PartialEq, Ord, PartialOrd, Hash, Clone, Reflect)]
#[reflect(opaque)]
#[reflect(Serialize, Deserialize)]
pub struct RootAssetPath<'a> {
    source: AssetSourceId<'a>,
    path: CowArc<'a, Path>,
}

impl<'a> RootAssetPath<'a> {
    pub fn new(source: AssetSourceId<'a>, path: CowArc<'a, Path>) -> Self {
        Self { source, path }
    }

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

impl<'a> Debug for RootAssetPath<'a> {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        Display::fmt(self, f)
    }
}

impl<'a> Display for RootAssetPath<'a> {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        if let AssetSourceId::Name(name) = self.source() {
            write!(f, "{name}://")?;
        }
        write!(f, "{}", self.path.display())?;
        Ok(())
    }
}

impl<'a> Serialize for RootAssetPath<'a> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        // XXX TODO: Implement without using AssetPath?
        AssetPath::from(self.clone()).serialize(serializer)
    }
}

impl<'a, 'de> Deserialize<'de> for RootAssetPath<'a> {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        // XXX TODO: Implement without using AssetPath?
        Ok(RootAssetPath::try_from(AssetPath::deserialize(deserializer)?).expect("XXX TODO?"))
    }
}

impl<'a> From<RootAssetPath<'a>> for AssetPath<'a> {
    fn from(value: RootAssetPath) -> Self {
        // XXX TODO: Odd, no way to create an `AssetPath` from a CowArc?
        Self::from_path_buf(PathBuf::from(&*value.path.into_owned()))
            .with_source(value.source.clone_owned())
    }
}

impl<'a> TryFrom<AssetPath<'a>> for RootAssetPath<'a> {
    type Error = (); // XXX TODO?

    fn try_from(value: AssetPath<'a>) -> Result<Self, Self::Error> {
        if value.label().is_some() {
            Err(())
        } else {
            Ok(Self::without_label(value))
        }
    }
}

impl From<String> for RootAssetPath<'static> {
    fn from(value: String) -> Self {
        // XXX TODO: Avoid going via `AssetPath`. Although that might mean
        // reimplementing `AssetPath::parse`, which is annoying.
        AssetPath::from(value).try_into().expect("XXX TODO")
    }
}

/// An `AssetRef` without a label.
#[derive(Eq, PartialEq, Hash, Clone, Debug, Reflect)]
#[reflect(opaque)]
#[reflect(SerializeWithRegistry, DeserializeWithRegistry)]
pub struct RootAssetRef {
    action: ErasedBassetAction,
}

impl RootAssetRef {
    pub fn new<P: BassetAction>(action: P) -> Self {
        Self {
            action: ErasedBassetAction::new(Arc::new(action)),
        }
    }

    pub fn without_label(value: AssetRef<'_>) -> Self {
        Self {
            action: value.action().clone(),
        }
    }

    fn action(&self) -> &ErasedBassetAction {
        &self.action
    }

    // XXX TODO: See notes on `AssetRef::try_temporary_path_workaround`.
    pub fn try_temporary_path_workaround(&self) -> Option<AssetPath<'static>> {
        if let Some(action) = self.action.0.downcast_ref::<LoadPath>()
            && action.loader_settings.is_none()
        {
            // XXX TODO: Should use `try_parse`?
            Some(AssetPath::parse(&action.path).clone_owned())
        } else {
            None
        }
    }
}

impl Display for RootAssetRef {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        write!(f, "{:?}", self.action)
    }
}

impl SerializeWithRegistry for RootAssetRef {
    fn serialize<S>(&self, serializer: S, registry: &TypeRegistry) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        // XXX TODO: Review. This makes the serialized data nicer, but relies
        // on `deserialize_any` - see notes on `deserialize_any` call in
        // `RootAssetRef` deserializer below.
        if let Some(path) = self.try_temporary_path_workaround() {
            path.serialize(serializer)
        } else {
            let mut s = serializer.serialize_struct("AssetRef", 3)?;

            s.serialize_field(
                "action",
                &ReflectSerializer::new((*self.action.0).as_partial_reflect(), registry),
            )?;

            s.end()
        }
    }
}

// XXX TODO: Duplicates a lot of the `AssetRef` equivalent. Awkward to refactor though?
impl<'de> DeserializeWithRegistry<'de> for RootAssetRef {
    fn deserialize<D>(deserializer: D, registry: &TypeRegistry) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        struct RootAssetRefVisitor<'a> {
            registry: &'a TypeRegistry,
        }

        impl<'a, 'de> Visitor<'de> for RootAssetRefVisitor<'a> {
            type Value = RootAssetRef;

            fn expecting(&self, formatter: &mut core::fmt::Formatter) -> core::fmt::Result {
                formatter.write_str("struct RootAssetRef or string")
            }

            fn visit_str<E>(self, v: &str) -> Result<RootAssetRef, E>
            where
                E: serde::de::Error,
            {
                Ok(RootAssetPath::from(v.to_string()).into())
            }

            fn visit_string<E>(self, v: String) -> Result<RootAssetRef, E>
            where
                E: serde::de::Error,
            {
                Ok(RootAssetPath::from(v).into())
            }

            fn visit_seq<V>(self, mut seq: V) -> Result<RootAssetRef, V::Error>
            where
                V: SeqAccess<'de>,
            {
                let action_dyn = seq
                    .next_element_seed(ReflectDeserializer::new(self.registry))?
                    .ok_or_else(|| serde::de::Error::invalid_length(0, &self))?;

                // XXX TODO: Do we need `action_dyn.get_represented_type_info()" if we already know the type?
                let action_registration = self
                    .registry
                    .get_with_type_path(
                        action_dyn
                            .get_represented_type_info()
                            .expect("XXX TODO?")
                            .type_path(),
                    )
                    .expect("XXX TODO?");

                let action_concrete = action_registration
                    .data::<ReflectFromReflect>()
                    .unwrap()
                    .from_reflect(&*action_dyn)
                    .unwrap();

                let action = action_registration
                    .data::<ReflectBassetAction>()
                    .expect("XXX TODO?")
                    .get_boxed(action_concrete)
                    .expect("XXX TODO?");

                Ok(RootAssetRef {
                    action: ErasedBassetAction(action.into()),
                })
            }

            fn visit_map<V>(self, mut map: V) -> Result<RootAssetRef, V::Error>
            where
                V: MapAccess<'de>,
            {
                #[derive(Deserialize)]
                #[serde(field_identifier, rename_all = "lowercase")]
                enum Field {
                    Action,
                }

                let mut action = None;

                while let Some(key) = map.next_key()? {
                    match key {
                        Field::Action => {
                            if action.is_some() {
                                return Err(serde::de::Error::duplicate_field("action"));
                            }

                            let action_dyn =
                                map.next_value_seed(ReflectDeserializer::new(self.registry))?;

                            // XXX TODO: Duplicates a bunch of `visit_seq`. Refactor?
                            let action_registration = self
                                .registry
                                .get_with_type_path(
                                    action_dyn
                                        .get_represented_type_info()
                                        .expect("XXX TODO?")
                                        .type_path(),
                                )
                                .expect("XXX TODO?");

                            let action_concrete = action_registration
                                .data::<ReflectFromReflect>()
                                .unwrap()
                                .from_reflect(&*action_dyn)
                                .unwrap();

                            action = Some(
                                action_registration
                                    .data::<ReflectBassetAction>()
                                    .expect("XXX TODO?")
                                    .get_boxed(action_concrete)
                                    .expect("XXX TODO?"),
                            );
                        }
                    }
                }

                let action = action.ok_or_else(|| serde::de::Error::missing_field("action"))?;

                Ok(RootAssetRef {
                    action: ErasedBassetAction(action.into()),
                })
            }
        }

        // XXX TODO: Review decision to use `deserialize_any` over `deserialize_struct`.
        // It makes the RON look much nicer, but limits support for non-RON serializers.
        //
        // deserializer.deserialize_struct(
        //     "RootAssetRef",
        //     &["action"],
        //     RootAssetRefVisitor { registry },
        // )
        deserializer.deserialize_any(RootAssetRefVisitor { registry })
    }
}

impl From<RootAssetRef> for AssetRef<'_> {
    fn from(value: RootAssetRef) -> Self {
        Self {
            action: value.action,
            label: None,
        }
    }
}

impl TryFrom<AssetRef<'_>> for RootAssetRef {
    type Error = (); // XXX TODO?

    fn try_from(value: AssetRef<'_>) -> Result<Self, Self::Error> {
        if value.label().is_some() {
            Err(())
        } else {
            Ok(Self::without_label(value))
        }
    }
}

impl From<RootAssetPath<'_>> for RootAssetRef {
    fn from(value: RootAssetPath) -> Self {
        RootAssetRef::new(LoadPath {
            path: value.to_string(),
            ..Default::default()
        })
    }
}

/// XXX TODO: Document. Review where this overlaps with `LoadContext` and check
/// if they can be combined.
pub struct ApplyContext<'a> {
    asset_server: &'a AssetServer,

    // Equivalent to `ErasedLoadedAsset::loader_dependencies`.
    // XXX TODO: Dependency cache key shouldn't be optional for us?
    loader_dependencies: HashMap<LoaderDependency, (AssetHash, Option<DependencyCacheKey>)>,

    dependency_loading: DependencyLoading,
}

impl<'a> ApplyContext<'a> {
    pub fn new(asset_server: &'a AssetServer, dependency_loading: DependencyLoading) -> Self {
        Self {
            asset_server,
            loader_dependencies: Default::default(),
            dependency_loading,
        }
    }

    // XXX TODO: Try to avoid exposing this?
    pub fn asset_server(&'a self) -> &'a AssetServer {
        self.asset_server
    }
}

impl ApplyContext<'_> {
    // XXX TODO: Avoid exposing this? Currently used by `LoadPath`. Maybe
    // the logic of `LoadPath` should mostly move into `ApplyContext`.
    pub fn dependency_loading(&self) -> DependencyLoading {
        self.dependency_loading
    }

    pub async fn erased_load_dependee(
        &mut self,
        path: &AssetRef<'static>,
    ) -> Result<ErasedLoadedAsset, BevyError> {
        // XXX TODO: Avoid clone?
        let (asset, dependency_key) = self
            .asset_server
            .basset_action_source()
            .apply(
                &RootAssetRef::without_label(path.clone()),
                self.asset_server,
                DependencyLoading::No,
            )
            .await?;

        // XXX TODO: Decide what to do here. The hash only appears to be used
        // for asset processing, so maybe can ignore for now.
        let hash = [0u8; 32];

        self.loader_dependencies.insert(
            LoaderDependency::Load(RootAssetRef::without_label(path.clone())),
            (hash, dependency_key),
        );

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
            // XXX TODO: Don't panic.
            Err(original) => panic!(
                "Should have made type {}, actually made type {}. Path: {path:?}",
                type_name::<T>(),
                original.asset_type_name(),
            ),
        }
    }

    // XXX TODO: Annoying that we're polluting the main asset server with handles
    // that might only be temporarily needed inside actions? Think this through again.
    pub fn make_handle<T: Asset, A: BassetAction>(&self, action: A) -> Handle<T> {
        self.asset_server
            .get_or_create_path_handle(AssetRef::new(action), None)
    }

    // XXX TODO: This is a slightly odd function, currently used to support saving
    // and reloading within an action - see `CompressImage` for use case.
    // Consider alternatives. Also need to think through what happens if the
    // asset has dependencies or path relative stuff.
    pub async fn load_from_reader(
        &self,
        reader: &mut dyn Reader,
        loader_type_name: &str,
        settings: &dyn Settings,
    ) -> Result<ErasedLoadedAsset, BevyError> {
        let maybe_loader = self
            .asset_server
            .read_loaders()
            .get_by_name(loader_type_name)
            .expect("XXX TODO");

        let loader = maybe_loader.get().await.expect("XXX TODO");

        let load_context = LoadContext::new(
            self.asset_server,
            // XXX TODO: Does this matter?
            AssetPath::parse("XXX TODO"),
            // XXX TODO: Review `load_dependencies` value?
            true,
            false,
        );

        loader.load(reader, settings, load_context).await

        // XXX TODO: Should we add the loaded asset's `loader_dependencies` to
        // our own `loader_dependencies`?
    }

    pub fn finish<A: Asset>(self, asset: A) -> BassetActionOutput {
        let mut loaded_asset = LoadedAsset::new_with_dependencies(asset);

        loaded_asset.loader_dependencies = self.loader_dependencies;

        let erased_asset = ErasedLoadedAsset::from(loaded_asset);

        load_dependencies(&erased_asset, self.asset_server, self.dependency_loading);

        BassetActionOutput {
            asset: erased_asset,
            saved: None,
        }
    }

    pub fn finish_erased_saved(
        self,
        mut asset: ErasedLoadedAsset,
        saved: StandaloneAssetData,
    ) -> BassetActionOutput {
        // XXX TODO: Note that we're overwriting the asset's own dependencies and
        // key. Check that this is correct.
        asset.loader_dependencies = self.loader_dependencies;

        load_dependencies(&asset, self.asset_server, self.dependency_loading);

        BassetActionOutput {
            asset,
            saved: Some(saved),
        }
    }
}

// XXX TODO: Move this to a utility file?
fn load_dependencies(
    asset: &ErasedLoadedAsset,
    asset_server: &AssetServer,
    dependency_loading: DependencyLoading,
) {
    // We don't want to load our own subassets, so make a list to check against.
    // XXX TODO: Inefficient and annoying. Reconsider.
    // XXX TODO: Actually... can we just early out if the handle's load status
    // is loaded? What is the status of our sub-assets at this point?
    let subasset_handles = asset
        .labeled_assets
        .iter()
        .map(|labeled_asset| labeled_asset.handle.clone())
        .collect::<Vec<_>>();

    if dependency_loading == DependencyLoading::Yes {
        asset.visit_dependencies(&mut |dependency| {
            match dependency {
                AssetDependency::Id(_) => panic!("XXX TODO: Not supported"),
                AssetDependency::Path(_) => (),
                AssetDependency::Handle(handle) => {
                    if !subasset_handles.contains(handle)
                        && let Some(path) = handle.path()
                    {
                        // XXX TODO: Review if there's a better way to load directly from
                        // a handle. Also check if this correctly early outs when the asset is loaded.
                        let _ = asset_server
                            .load_builder()
                            .load_erased(handle.type_id(), path.clone());
                    }
                }
            }
        });
    }
}

// XXX TODO: Review `Debug` bound.
#[reflect_trait]
pub trait BassetAction: Downcast + Send + Sync + 'static + Reflect + Debug {}

// XXX TODO: Review this. Duplicated from `bevy_asset::meta::Settings`.
impl_downcast!(BassetAction);

// XXX TODO: Document? See bevy_reflect where it does `impl TypePath for dyn Enemy`.
impl TypePath for dyn BassetAction {
    fn type_path() -> &'static str {
        "dyn bevy_asset::basset::BassetAction"
    }

    fn short_type_path() -> &'static str {
        "dyn BassetAction"
    }
}

// XXX TODO: Decide if member should be pub?
#[derive(Clone)]
pub struct ErasedBassetAction(pub Arc<dyn BassetAction>);

impl ErasedBassetAction {
    pub fn new(action: Arc<dyn BassetAction>) -> Self {
        Self(action)
    }

    pub fn type_path(&self) -> &'static str {
        // We assume that `get_represented_type_info` is available. This is
        // checked in `DevelopmentActionSourceSettings::with_action`.
        (*self.0)
            .get_represented_type_info()
            .map(TypeInfo::type_path)
            .unwrap_or("ERROR") // XXX TODO: What should this do?
    }
}

impl Hash for ErasedBassetAction {
    fn hash<H: Hasher>(&self, state: &mut H) {
        // We assume that `reflect_hash` is available. This is checked in
        // `DevelopmentActionSourceSettings::with_action`.
        state.write_u64((*self.0).reflect_hash().unwrap_or_default());
    }
}

impl PartialEq for ErasedBassetAction {
    fn eq(&self, other: &Self) -> bool {
        // We assume that `reflect_partial_eq` is available. This is checked in
        // `DevelopmentActionSourceSettings::with_action`.
        (*self.0).reflect_partial_eq(&*other.0) == Some(true)
    }
}

impl Eq for ErasedBassetAction {}

impl Debug for ErasedBassetAction {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        self.0.fmt(f)
    }
}

pub struct BassetActionOutput {
    pub asset: ErasedLoadedAsset,
    // XXX TODO: This value is only needed for the action cache. Is there any
    // benefit to telling the action that it won't be used?
    pub saved: Option<StandaloneAssetData>,
}

/// XXX TODO: Document.
///
/// An action that takes a parameter struct of a known type and returns an
/// `ErasedLoadedAsset`.
pub trait BassetActionFunction: Send + Sync + 'static {
    type Action: BassetAction + TypePath + Default;

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
        action: &Self::Action,
    ) -> impl ConditionalSendFuture<Output = Result<BassetActionOutput, Self::Error>>;

    /// XXX TODO: Consider name. It implies publishable as well?
    fn cacheable(&self) -> bool {
        true
    }
}

pub trait ErasedBassetActionFunction: Send + Sync + 'static {
    fn apply<'a>(
        &'a self,
        context: ApplyContext<'a>,
        // XXX TODO: Could consider taking `&' dyn BassetAction`? More general,
        // but also more risk if we need something from `ErasedBassetAction` like a debug type name.
        action: &'a ErasedBassetAction,
    ) -> BoxedFuture<'a, Result<BassetActionOutput, BevyError>>;

    fn cacheable(&self) -> bool;
}

impl<T> ErasedBassetActionFunction for T
where
    T: BassetActionFunction + Send + Sync,
{
    fn apply<'a>(
        &'a self,
        context: ApplyContext<'a>,
        action: &'a ErasedBassetAction,
    ) -> BoxedFuture<'a, Result<BassetActionOutput, BevyError>> {
        Box::pin(async move {
            let action = action.0.downcast_ref::<T::Action>().expect("XXX TODO");

            T::apply(self, context, action).await.map_err(Into::into)
        })
    }

    fn cacheable(&self) -> bool {
        BassetActionFunction::cacheable(self)
    }
}

#[derive(Error, Debug, Clone)]
#[error("Failed to apply action '{path}': {error}")]
pub struct ActionApplyError {
    // XXX TODO: Should this be `AssetRef` or `RootAssetRef`?
    pub path: AssetRef<'static>,
    pub error: Arc<BevyError>,
}

impl ActionApplyError {
    pub fn path(&self) -> &AssetRef<'static> {
        &self.path
    }

    pub fn error(&self) -> &BevyError {
        &self.error
    }
}

pub struct DevelopmentActionSourceSettings {
    file_cache_path: Option<PathBuf>,
    validate_dependency_cache: bool,
    validate_action_cache: bool,
    // XXX TODO: Better to use `TypeId` or type name?
    action_type_path_to_action_function: HashMap<&'static str, Box<dyn ErasedBassetActionFunction>>,
    // XXX TODO: We're using type name for the asset but type path for the action?
    // Might be necessary but double check.
    asset_type_name_to_saver: HashMap<&'static str, (Box<dyn ErasedAssetSaver>, Box<dyn Settings>)>,
    default_poly_saver: Option<(Box<dyn ErasedAssetSaver>, Box<dyn Settings>)>,
}

impl Default for DevelopmentActionSourceSettings {
    fn default() -> Self {
        let mut action_type_path_to_action_function =
            HashMap::<&'static str, Box<dyn ErasedBassetActionFunction>>::new();

        // XXX TODO: Review if we should be automatically adding `LoadPath`
        // here. Maybe should be added afterwards so it's not exposed to the
        // user. Maybe shouldn't even go in here since it could be special cased?
        // It's likely the `LoadPath` will be the most common action, so making
        // `DevelopmentActionSource::action_function` check for `LoadPath` before
        // the hash lookup might pay off.
        action_type_path_to_action_function
            .insert(LoadPath::type_path(), Box::new(LoadPathFunction));

        Self {
            file_cache_path: Default::default(),
            validate_dependency_cache: Default::default(),
            validate_action_cache: Default::default(),
            action_type_path_to_action_function,
            asset_type_name_to_saver: Default::default(),
            default_poly_saver: None,
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

    // XXX TODO: Rename since we're now taking a function rather than the action?
    // Or maybe we should make `BassetAction` have a reference to its function
    // rather than vice versa?
    pub fn with_action<T: BassetActionFunction>(mut self, action: T) -> Self {
        // XXX TODO: Error handling.
        // XXX TODO: Annoying that we're checking by instantiating the type.
        // Any other options?
        let default = T::Action::default();
        assert!(default.get_represented_type_info().is_some());
        assert!(default.reflect_partial_eq(&default).is_some());
        assert!(default.reflect_hash().is_some());

        self.action_type_path_to_action_function
            .insert(T::Action::type_path(), Box::new(action));

        self
    }

    pub fn with_saver<T: AssetSaver>(mut self, saver: T) -> Self {
        // XXX TODO: Do we ever want custom settings?
        let settings = T::Settings::default();

        self.asset_type_name_to_saver.insert(
            type_name::<T::Asset>(),
            (Box::new(ErasedUniAssetSaver(saver)), Box::new(settings)),
        );

        self
    }

    pub fn with_default_poly_saver<T: PolyAssetSaver>(mut self, saver: T) -> Self {
        // XXX TODO: Do we ever want custom settings?
        let settings = T::Settings::default();

        self.default_poly_saver = Some((Box::new(ErasedPolyAssetSaver(saver)), Box::new(settings)));

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
    fn build(
        &self,
        sources: Arc<AssetSources>,
        registry: TypeRegistryArc,
    ) -> Arc<dyn ActionSource> {
        Arc::new(DevelopmentActionSource::new(
            self.settings.clone(),
            sources,
            registry,
        ))
    }
}

// XXX TODO: Review if all this stuff actually needs to be shared.
pub(crate) struct DevelopmentActionSource {
    // XXX TODO: Keeping settings is kinda annoying, but it's the only way I can
    // see to avoid cloning things like `BassetSettings::asset_type_name_to_action'.
    // See also comment on `AssetPlugin::basset_settings`.
    settings: Arc<DevelopmentActionSourceSettings>,
    dependency_graph: Option<DependencyGraph>,
    action_cache: Option<MemoryAndFileCache<ActionCacheKey, Arc<[u8]>>>,
    registry: TypeRegistryArc,
}

// XXX TODO: Does this need to be an enum?
#[derive(Error, Debug, Copy, Clone)]
pub enum MissingActionFunctionError {
    #[error("Couldn't find action \"{0}\"")]
    NotFound(&'static str),
}

impl DevelopmentActionSource {
    pub(crate) fn new(
        settings: Arc<DevelopmentActionSourceSettings>,
        sources: Arc<AssetSources>,
        registry: TypeRegistryArc,
    ) -> Self {
        // TODO: Dependency graph should be optional based on a setting?
        let dependency_graph = Some(DependencyGraph::new(
            settings
                .file_cache_path
                .as_ref()
                .map(|p| p.join("dependency")),
            settings.validate_dependency_cache,
            sources,
            registry.clone(),
        ));

        let action_cache = Some(MemoryAndFileCache::new(
            "action_cache",
            settings.file_cache_path.as_ref().map(|p| p.join("action")),
            settings.validate_action_cache,
            registry.clone(),
        ));

        Self {
            settings,
            dependency_graph,
            action_cache,
            registry,
        }
    }

    // XXX TODO: Clarify that this is mapping the `BassetAction` type name to action.
    pub(crate) fn action_function<'a>(
        &'a self,
        action: &ErasedBassetAction,
    ) -> Result<&'a dyn ErasedBassetActionFunction, MissingActionFunctionError> {
        let type_path = action.type_path();

        self.settings
            .action_type_path_to_action_function
            .get(type_path)
            .map(AsRef::as_ref)
            .ok_or(MissingActionFunctionError::NotFound(type_path))
    }

    pub(crate) fn saver<'a>(
        &'a self,
        asset: &ErasedLoadedAsset,
        registry: &TypeRegistryArc,
    ) -> Option<(&'a dyn ErasedAssetSaver, &'a dyn Settings)> {
        // Try to find a saver for the asset type.
        if let Some((saver, settings)) = self
            .settings
            .asset_type_name_to_saver
            .get(asset.asset_type_name())
        {
            return Some((saver.as_ref(), settings.as_ref()));
        }

        // If there's no specific saver for the type and the asset is
        // reflectable, fall back to the default saver.
        //
        // XXX TODO: Relationship between "reflectable = default saver" is
        // unclear. Review. Is this a naming problem or is the structure wrong?
        //
        // XXX TODO: Looking up the partial reflect is annoying as it will
        // probably be done again in the saver. Any alternatives?
        if asset.as_partial_reflect(&registry.read()).is_some() {
            self.settings
                .default_poly_saver
                .as_ref()
                .map(|(saver, settings)| (saver.as_ref(), settings.as_ref()))
        } else {
            None
        }
    }

    // XXX TODO: Settings?
    fn register_dependencies<'a>(
        &'a self,
        path: &'a RootAssetRef,
        // XXX TODO: Settings?
        _settings: Option<&'a dyn Settings>,
        asset: &'a ErasedLoadedAsset,
        // XXX TODO: We don't do anything if this is `None`. Review and maybe
        // remove `Option`.
        dependency_key: Option<DependencyCacheKey>,
    ) -> Option<BoxedFuture<'a, Option<ActionCacheKey>>> {
        if let Some(dependency_key) = dependency_key
            && let Some(dependency_graph) = &self.dependency_graph
        {
            Some(Box::pin(async move {
                let mut loader_dependencies =
                    Vec::<CacheLoaderDependency>::with_capacity(asset.loader_dependencies.len());

                for (loader_dependency, (_, dependency_key)) in &asset.loader_dependencies {
                    if let Some(dependency_key) = dependency_key {
                        loader_dependencies.push(CacheLoaderDependency::new(
                            loader_dependency.clone(),
                            *dependency_key,
                        ));
                    } else {
                        warn!("Missing dependency key for loader dependency. path: {path:?} dependency: {loader_dependency:?}");
                        return None;
                    }
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

                // XXX TODO: Are we accounting for sub-asset dependencies?

                let dependency_value = DependencyCacheValue::new(
                    loader_dependencies.into_iter(),
                    external_dependees.into_iter(),
                );

                dependency_graph.register_dependencies_load(path, dependency_key, dependency_value)
            }))
        } else {
            None
        }
    }
}

impl ActionSource for DevelopmentActionSource {
    fn apply<'a>(
        &'a self,
        action: &'a RootAssetRef,
        asset_server: &'a AssetServer,
        dependency_loading: DependencyLoading,
    ) -> BoxedFuture<'a, Result<(ErasedLoadedAsset, Option<DependencyCacheKey>), AssetLoadError>>
    {
        Box::pin(async move {
            let action_function = self.action_function(action.action())?;

            // XXX TODO: Avoid clone?
            // XXX TODO: Could have a fast path here since we know the dependency key
            // is for an action?
            let dependency_key = self
                .dependency_graph
                .as_ref()
                .map(|dependency_graph| dependency_graph.action_dependency_key(action));

            if action_function.cacheable()
                && let Some(dependency_key) = dependency_key
                && let Some(action_cache) = &self.action_cache
                && let Some(dependency_graph) = &self.dependency_graph
            {
                // XXX TODO: Maybe early out here if there's no saver? Depends if we end
                // up in a situation where the saver has been compiled out but we still
                // want to read from the cache.

                if let Some((action_key, _)) = dependency_graph.action_key(action).await
                    && let Some(cached_standalone_asset) =
                        action_cache.get(&action_key, action).await
                {
                    let standalone_asset = read_standalone_asset(&cached_standalone_asset)?;

                    return Ok((
                        load_standalone_asset(&standalone_asset, asset_server, dependency_loading)
                            .await?,
                        Some(dependency_key),
                    ));
                }
            }

            let apply_context = ApplyContext::new(asset_server, dependency_loading);

            let output = action_function
                .apply(apply_context, action.action())
                .await
                .map_err(|e| ActionApplyError {
                    path: AssetRef::from(action.clone()),
                    error: e.into(),
                })?;

            // XXX TODO: Review logging. Bit spammy right now.
            /*
            if let Some(keys) = asset.keys.as_ref()
                && !keys.immediate_dependee_action_keys.is_empty()
            {
                info!(
                    "{:?}: Dependencies = {:?}",
                    action, keys.immediate_dependee_action_keys,
                );
            }
            */

            // XXX TODO: Is settings parameter correct?
            let action_key = if let Some(future) =
                self.register_dependencies(action, None, &output.asset, dependency_key)
            {
                future.await
            } else {
                None
            };

            if let Some(action_key) = action_key {
                if action_function.cacheable()
                    && let Some(action_cache) = &self.action_cache
                {
                    if let Some(saved) = output.saved {
                        // XXX TODO: Slightly duplicates the other path. Refactor?
                        let standalone_asset_blob = write_standalone_asset(&saved)
                            .map_err(|e| AssetLoadError::TodoError(e.into()))?;
                        action_cache.put(action_key, standalone_asset_blob.into(), action);
                    } else if let Some((saver, settings)) =
                        self.saver(&output.asset, &self.registry)
                    {
                        // XXX TODO: Support action outputs with sub-assets. Could be troublesome
                        // as there's two potential cases:
                        //
                        // 1. The asset saver for the root asset expects to be given the sub-assets.
                        // 2. The root asset and sub-assets should be saved by separate savers.
                        assert!(
                            output.asset.labeled_assets.is_empty(),
                            "XXX TODO. Not supported yet. {action:?}",
                        );

                        // XXX TODO: Verify loader matches saver? Need a way to get
                        // `AssetSaver::OutputLoader` out of `ErasedAssetSaver`.
                        let loader = asset_server
                            .get_asset_loader_with_type_name(saver.loader_type_name())
                            .await?;

                        let standalone_asset =
                            save_standalone_asset(&output.asset, &*loader, saver, settings)
                                .await
                                .map_err(|e| AssetLoadError::TodoError(e.into()))?;

                        let standalone_asset_blob = write_standalone_asset(&standalone_asset)
                            .map_err(|e| AssetLoadError::TodoError(e.into()))?;

                        action_cache.put(action_key, standalone_asset_blob.into(), action);
                    } else {
                        let type_name = output.asset.asset_type_name();
                        debug!(?type_name, ?action, "Cache ineligible, no saver for type.");
                    }
                }
            } else {
                warn!(
                    ?action,
                    "Register dependencies did not return an action key."
                );
            }

            Ok((output.asset, dependency_key))
        })
    }

    fn read_asset_bytes<'a>(
        &'a self,
        path: RootAssetPath<'static>,
        reader: &'a mut dyn Reader,
    ) -> BoxedFuture<'a, Result<Option<(Box<dyn Reader>, DependencyCacheKey)>, ReadAssetBytesError>>
    {
        // XXX TODO: Make future optional when there's no dependency graph?
        Box::pin(async move {
            if let Some(dependency_graph) = &self.dependency_graph {
                let (reader, dependency_key) =
                    dependency_graph.read_asset_bytes(&path, reader).await?;

                Ok(Some((reader, dependency_key)))
            } else {
                Ok(None)
            }
        })
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
    ) -> Option<BoxedFuture<'a, Result<(), BevyError>>> {
        Some(Box::pin(async move {
            let begin_time = Instant::now();

            std::dbg!(&input);

            let mut pack = WritablePackFile::default();

            // Assets that have already been published to `pack`.
            //
            // XXX TODO: Consider removing this and checking `pack` directly?
            // Kinda depends on multithreading approaches.
            let mut done = HashSet::<PublishDependency>::new();

            // XXXX TODO: Consume rather than clone?
            let mut input_stack = input.paths.clone();

            while let Some(input_asset) = input_stack.pop() {
                // XXX TODO: Clone is annoying, but not sure it's possible to avoid
                // without doing a separate `contains` then `insert`?
                //
                // XXX TODO: Should we be checking for done here or where we add stuff to
                // the input stack? Note that the latter would mean we have to de-dupe
                // before we initialize the input stack above.
                if !done.insert(input_asset.clone()) {
                    continue;
                }

                // XXX TODO: Prettier output, don't use `Debug`.
                info!(?input_asset, "Publishing");

                match &input_asset {
                    PublishDependency::Load(action) => {
                        let action_function = self.action_function(action.action())?;

                        // XXX TODO: Review all the code in here to avoid duplication.
                        //
                        // There's currently four cases:
                        //
                        // 1. Action cacheable, dependency and action cache hit.
                        //     - Copy from action cache, add external dependencies to stack.
                        // 2. Action cacheable, dependency or action cache miss.
                        //     - Apply action and save, add external dependencies to stack.
                        // 3. Action not cacheable, dependency cache hit.
                        //     - Add loader and external dependencies to stack.
                        // 4. Action not cacheable, dependency cache miss.
                        //     - Apply action, add loader and external dependencies to stack.
                        //
                        // Some of the paths are similar. And some already do redundant work,
                        // like the action cache miss repeats the action cache lookup.

                        if action_function.cacheable() {
                            if let Some(dependency_graph) = &self.dependency_graph
                                && let Some((action_key, dependency_value)) =
                                    dependency_graph.action_key(action).await
                                && let Some(action_cache) = &self.action_cache
                                && let Some(cached_standalone_asset) =
                                    action_cache.get(&action_key, action).await
                            {
                                let standalone_asset =
                                    read_standalone_asset(&cached_standalone_asset)?;

                                pack.actions.insert(
                                    Box::<str>::from(action.to_string()),
                                    StagedAsset {
                                        asset_bytes: standalone_asset.asset.into(),
                                        meta_bytes: Some(standalone_asset.meta.into()),
                                    },
                                );

                                for dependency in dependency_value.external_dependees() {
                                    input_stack.push(PublishDependency::Load(dependency.clone()));
                                }
                            } else {
                                // XXX TODO: This is duplicating some of the the action cache path that we've done above.
                                // Need to skip that part, but we still want to populate the action cache.

                                let (loaded, _) = self
                                    .apply(action, asset_server, DependencyLoading::No)
                                    .await
                                    .expect("XXX TODO");

                                // XXX TODO: Decide if we try to support the original path.
                                let fake_path = AssetPath::parse(
                                    "ERROR - Standalone assets shouldn't use their path",
                                );

                                // XXX TODO: Duplicates where `load_action` writes to the cache.
                                let (saver, saver_settings) =
                                    self.saver(&loaded, &self.registry).ok_or_else(|| {
                                        format!(
                                            "Couldn't find saver for asset type \"{}\".",
                                            loaded.asset_type_name()
                                        )
                                    })?;

                                let loader = asset_server
                                    .get_asset_loader_with_type_name(saver.loader_type_name())
                                    .await?;

                                // XXX TODO: Review and check that we're ok with default settings.
                                // Same decision made in `save_standalone_asset`.
                                let meta_bytes =
                                    loader.default_meta().serialize().into_boxed_slice();

                                let mut asset_bytes = Vec::<u8>::new();
                                saver
                                    .save(&mut asset_bytes, &loaded, saver_settings, fake_path)
                                    .await
                                    .expect("XXX TODO");

                                pack.actions.insert(
                                    Box::<str>::from(action.to_string()),
                                    StagedAsset {
                                        asset_bytes: asset_bytes.into(),
                                        meta_bytes: Some(meta_bytes),
                                    },
                                );

                                loaded.visit_dependencies(&mut |dependency| {
                                    if let Some(path) = match dependency {
                                        AssetDependency::Id(_) => todo!("Decide if we disallow ids. Dependency tracking requires the path."),
                                        AssetDependency::Handle(handle) => handle.path().cloned(),
                                        AssetDependency::Path(path) => Some(path.clone()),
                                    } {
                                        // XXX TODO: Similar to case above - shouldn't assume PublishDependency::Load.
                                        input_stack.push(PublishDependency::Load(RootAssetRef::without_label(path)));
                                    };
                                });
                            }
                        } else {
                            if let Some(dependency_graph) = &self.dependency_graph
                                && let Some((_, dependency_value)) =
                                    dependency_graph.action_key(action).await
                            {
                                for dependency in dependency_value.loader_dependees() {
                                    input_stack.push(dependency.0.clone().into());
                                }

                                for dependency in dependency_value.external_dependees() {
                                    input_stack.push(PublishDependency::Load(dependency.clone()));
                                }
                            } else {
                                let (loaded, _) = self
                                    .apply(action, asset_server, DependencyLoading::No)
                                    .await
                                    .expect("XXX TODO");

                                for dependency in loaded.loader_dependencies.keys() {
                                    input_stack.push(dependency.clone().into());
                                }

                                loaded.visit_dependencies(&mut |dependency| {
                                    if let Some(path) = match dependency {
                                        AssetDependency::Id(_) => todo!("Decide if we disallow ids. Dependency tracking requires the path."),
                                        AssetDependency::Handle(handle) => handle.path().cloned(),
                                        AssetDependency::Path(path) => Some(path.clone()),
                                    } {
                                        // XXX TODO: Similar to case above - shouldn't assume PublishDependency::Load.
                                        input_stack.push(PublishDependency::Load(RootAssetRef::without_label(path)));
                                    };
                                });
                            }
                        }

                        // XXX TODO: We're not accounting for the standalone asset having
                        // loader dependencies. Not sure if we can work them out unless
                        // we do a fake load?
                    }
                    PublishDependency::File(path) => {
                        let reader = asset_server
                            .get_source(path.source())
                            .expect("XXX TODO")
                            .reader();

                        let mut asset_bytes = Vec::new();
                        reader
                            .read(path.path())
                            .await
                            .expect("XXX TODO")
                            .read_to_end(&mut asset_bytes)
                            .await
                            .expect("XXX TODO");

                        // XXX TODO: Should throw error if the error isn't `NotFound`?
                        let meta_bytes =
                            if let Ok(mut meta_reader) = reader.read_meta(path.path()).await {
                                let mut meta_bytes = Vec::new();
                                meta_reader
                                    .read_to_end(&mut meta_bytes)
                                    .await
                                    .expect("XXX TODO");

                                Some(meta_bytes)
                            } else {
                                None
                            };

                        pack.paths.insert(
                            path.clone(),
                            StagedAsset {
                                asset_bytes: asset_bytes.into(),
                                meta_bytes: meta_bytes.map(Vec::into_boxed_slice),
                            },
                        );
                    }
                }
            }

            info!("Writing pack file {pack_path:?}");

            write_pack_file(pack, pack_path).await;

            info!(
                "Publishing finished in {:.2}s",
                begin_time.elapsed().as_secs_f32()
            );

            Ok(())
        }))
    }
}

#[derive(Debug, Clone, Reflect)]
struct BassetFileSerializable {
    // XXX TODO: Consider renaming, could get confused with "root asset" versus "sub-asset".
    // XXX TODO: What happens if the `AssetRef` has a label? Need to think through how
    // the dependencies work. Suspect it may be wrong, as we'll end up calling
    // `register_dependencies` with a sub-asset that has the root asset's dependency
    // key but not the root asset's dependencies.
    root: AssetRef<'static>,
    // TODO: Versioning?
}

// XXX TODO: Could be avoided if `LoadContext` or `AssetServer` exposed the
// type registry. Is that a step too far?
#[derive(TypePath)]
struct BassetLoader(AppTypeRegistry);

impl FromWorld for BassetLoader {
    fn from_world(world: &mut bevy_ecs::world::World) -> Self {
        // XXX TODO: Is this ok to panic?
        BassetLoader(world.resource::<AppTypeRegistry>().clone())
    }
}

impl PolyAssetLoader for BassetLoader {
    type Settings = ();
    type Error = BevyError;

    async fn load(
        &self,
        reader: &mut dyn Reader,
        _settings: &Self::Settings,
        load_context: LoadContext<'_>,
    ) -> Result<ErasedLoadedAsset, BevyError> {
        assert!(
            load_context.path().label().is_none(),
            "Paranoia. {:?}",
            load_context.path()
        );

        let mut bytes = Vec::new();
        reader.read_to_end(&mut bytes).await?;

        let basset = {
            let registry = self.0.read();

            // XXX TODO: Don't need this if we use `TypedReflectDeserializer::of`?
            // But that can panic.
            let registration = registry
                .get(TypeId::of::<BassetFileSerializable>())
                .expect("XXX TODO");

            TypedReflectDeserializer::new(registration, &registry)
                .deserialize(&mut ron::de::Deserializer::from_bytes(&bytes).expect("XXX TODO"))?
                .try_take::<BassetFileSerializable>()
                .expect("XXX TODO")
        };

        let root_without_label = RootAssetRef::without_label(basset.root.clone());

        let dependency_loading = if load_context.should_load_dependencies {
            DependencyLoading::Yes
        } else {
            DependencyLoading::No
        };

        let asset = load_context
            .asset_server()
            .basset_action_source()
            .apply(
                &root_without_label,
                load_context.asset_server(),
                dependency_loading,
            )
            .await
            .map(|(mut asset, dependency_key)| {
                // XXX TODO: See other cases of ignoring the hash.
                let hash = [0u8; 32];

                // XXX TODO: Justify this dependency replacement.
                asset.loader_dependencies.clear();
                asset.loader_dependencies.insert(
                    LoaderDependency::Load(RootAssetRef::without_label(basset.root.clone())),
                    (hash, dependency_key),
                );

                asset
            })?;

        // XXX TODO: Maybe wrong. See comment on `BassetFileSerializable::root`.
        asset
            .take_labeled(basset.root.label_cow())
            .map_err(|_| format!("Couldn't find labeled asset \"{:?}\".", &basset.root).into())
    }

    fn extensions(&self) -> &[&str] {
        &["basset"]
    }
}

// XXX TODO: Reconsider name?
#[derive(PartialEq, Eq, Copy, Clone)]
pub enum DependencyLoading {
    Yes,
    No,
}

impl DependencyLoading {
    pub fn new(value: bool) -> Self {
        if value {
            Self::Yes
        } else {
            Self::No
        }
    }

    pub fn as_bool(&self) -> bool {
        *self == Self::Yes
    }
}

// XXX TODO: Review name? Does have some similarities to `AssetSource` but not
// that much. `ActionHandler`?
//
// XXX TODO: Should be `pub`?
pub trait ActionSource: Send + Sync + 'static {
    fn apply<'a>(
        &'a self,
        // XXX TODO: Consider `RootAssetRef<'a>`?
        action: &'a RootAssetRef,
        // XXX TODO: Review and see if we can use something narrower than the
        // entire asset server.
        asset_server: &'a AssetServer,
        dependency_loading: DependencyLoading,
    ) -> BoxedFuture<'a, Result<(ErasedLoadedAsset, Option<DependencyCacheKey>), AssetLoadError>>;

    // XXX TODO: Review name. In practice it's more like `register_file_dependency`?
    fn read_asset_bytes<'a>(
        &'a self,
        _path: RootAssetPath<'static>,
        _reader: &'a mut dyn Reader,
    ) -> BoxedFuture<'a, Result<Option<(Box<dyn Reader>, DependencyCacheKey)>, ReadAssetBytesError>>
    {
        // XXX TODO: Make future optional?
        Box::pin(async move { Ok(None) })
    }

    // XXX TODO: Less hacky debugging.
    fn dump_dependency_graph(&self) {
        // XXX TODO?
        tracing::error!("dump_dependency_graph not implemented");
    }

    // XXX TODO: This shouldn't be here - only one source can actually publish.
    // Publishing kinda duplicates a lot of `DevelopmentActionSource`, so probably
    // needs refactoring to pull out that duplication and make publishing its
    // own action source?
    fn publish<'a>(
        &'a self,
        _input: PublishInput,
        _asset_server: &'a AssetServer,
        _pack_path: &'a Path,
    ) -> Option<BoxedFuture<'a, Result<(), BevyError>>> // XXX TODO: More specific error type?
    {
        todo!("XXX TODO: What should this do by default?")
    }
}

pub trait ActionSourceBuilder: Send + Sync + 'static {
    fn build(&self, sources: Arc<AssetSources>, registry: TypeRegistryArc)
        -> Arc<dyn ActionSource>;
}

// XXX TODO: Review use cases.
async fn try_load_path_action(
    action: &RootAssetRef,
    asset_server: &AssetServer,
    dependency_loading: DependencyLoading,
) -> Option<Result<ErasedLoadedAsset, AssetLoadError>> {
    // XXX TODO: Check if there's a better way to handle this.
    if action.action.type_path() == LoadPath::type_path() {
        Some(
            ErasedBassetActionFunction::apply(
                &LoadPathFunction,
                ApplyContext::new(asset_server, dependency_loading),
                &action.action,
            )
            .await
            .map(|output| output.asset)
            .map_err(|e| {
                ActionApplyError {
                    path: AssetRef::from(action.clone()),
                    error: e.into(),
                }
                .into()
            }),
        )
    } else {
        None
    }
}

// XXX TODO: Still needed?
#[expect(unused, reason = "XXX TODO")]
pub(crate) struct MinimalActionSourceBuilder;

impl ActionSourceBuilder for MinimalActionSourceBuilder {
    fn build(
        &self,
        _sources: Arc<AssetSources>,
        _registry: TypeRegistryArc,
    ) -> Arc<dyn ActionSource> {
        Arc::new(MinimalActionSource)
    }
}

// XXX TODO: Review use cases. Is useful if tests can support `load_with_settings`,
// which means supporting the `LoadPath` action.
pub(crate) struct MinimalActionSource;

impl ActionSource for MinimalActionSource {
    fn apply<'a>(
        &'a self,
        action: &'a RootAssetRef,
        // XXX TODO: Review and see if we can use something narrower than the
        // entire asset server.
        asset_server: &'a AssetServer,
        dependency_loading: DependencyLoading,
    ) -> BoxedFuture<'a, Result<(ErasedLoadedAsset, Option<DependencyCacheKey>), AssetLoadError>>
    {
        Box::pin(async move {
            if let Some(output) =
                try_load_path_action(action, asset_server, dependency_loading).await
            {
                Ok((output?, None))
            } else {
                // XXX TODO: More appropriate error? "Not found" is misleading - maybe "not supported"?
                Err(MissingActionFunctionError::NotFound(action.action().type_path()).into())
            }
        })
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
    fn build(
        &self,
        _sources: Arc<AssetSources>,
        _registry: TypeRegistryArc,
    ) -> Arc<dyn ActionSource> {
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
        action: &'a RootAssetRef,
        asset_server: &'a AssetServer,
        dependency_loading: DependencyLoading,
    ) -> BoxedFuture<'a, Result<(ErasedLoadedAsset, Option<DependencyCacheKey>), AssetLoadError>>
    {
        Box::pin(async move {
            // Early out if we've got a load path action. They're guaranteed to
            // not be published - only the file they're loading and the loader
            // dependencies are published.
            if let Some(load_path_action) =
                try_load_path_action(action, asset_server, dependency_loading).await
            {
                return Ok((load_path_action?, None));
            }

            // XXX TODO: Review if we should be treating these strings as
            // reliable paths. Maybe want a distinct type for safety at least?
            let action_string = action.to_string();

            // XXX TODO: The below duplicates a fair amount of `read_standalone_asset`.
            // Refactor?

            let Ok(mut readers) = self.pack_file.action(&action_string) else {
                return Err(AssetReaderError::NotFound(action.to_string().into()).into());
            };

            let Some(mut meta_reader) = readers.meta else {
                // XXX TODO: Review this. Maybe consider having separate storage
                // for actions so it's clear that a meta is required. Also a
                // possibility that we don't need the full meta for actions,
                // just the loader name?
                return Err(AssetLoadError::TodoError(Arc::new(
                    "Actions require a meta. XXX TODO: Improve".into(),
                )));
            };

            let mut meta_bytes = Vec::<u8>::new();
            meta_reader
                .read_to_end(&mut meta_bytes)
                .await
                .map_err(AssetReaderError::from)?;

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

            let populate_hashes = false;

            // XXX TODO: Ew? Need to decide if we try to support the original path.
            let fake_path = AssetPath::parse("ERROR - published assets shouldn't use their path");

            Ok((
                internal_load_with_settings_loader_and_reader(
                    asset_server,
                    &fake_path,
                    meta.loader_settings().expect("meta is set to Load"),
                    &*loader,
                    &mut readers.asset,
                    dependency_loading,
                    populate_hashes,
                )
                .await?,
                None,
            ))
        })
    }
}

pub(crate) async fn internal_load_with_settings_loader_and_reader(
    asset_server: &AssetServer,
    asset_path: &AssetPath<'_>,
    settings: &dyn Settings,
    loader: &dyn ErasedAssetLoader,
    reader: &mut dyn Reader,
    dependency_loading: DependencyLoading,
    populate_hashes: bool,
) -> Result<ErasedLoadedAsset, AssetLoadError> {
    // XXX TODO: Just take by value?
    let asset_path = asset_path.clone_owned();

    let load_context = LoadContext::new(
        asset_server,
        asset_path.clone(),
        dependency_loading.as_bool(),
        populate_hashes,
    );
    let load = AssertUnwindSafe(loader.load(reader, settings, load_context)).catch_unwind();
    #[cfg(feature = "trace")]
    let load = {
        use tracing::Instrument;

        let span = tracing::info_span!(
            "asset loading",
            loader = loader.type_path(),
            asset = asset_path.to_string()
        );
        load.instrument(span)
    };
    load.await
        .map_err(|_| AssetLoadError::AssetLoaderPanic {
            path: asset_path.clone_owned().into(),
            loader_name: loader.type_path(),
        })?
        .map_err(|e| {
            AssetLoadError::AssetLoaderError(AssetLoaderError::new(
                asset_path.clone_owned().into(),
                loader.type_path(),
                e.into(),
            ))
        })
}

pub mod action {
    use super::*;
    use alloc::string::String;

    pub struct LoadPathFunction;

    #[derive(Reflect, Default, Hash, PartialEq, Debug)]
    #[reflect(BassetAction, Hash, PartialEq)]
    pub struct LoadPath {
        // XXX TODO: Should be `RootAssetPath`? Avoiding for now to simplify lifetimes and defaults.
        pub path: String,
        // XXX TODO: Currently the settings are simply serialized RON, which means we're
        // going to get duplication due to being non-canonical. Ideally it would
        // be a `Box<dyn Settings>`, but serializing that will be a challenge.
        // Making `Settings` be `Reflect` is one option, but that's a fairly disruptive
        // change.
        #[reflect(default)]
        pub loader_settings: Option<String>,
        // XXX TODO: Consider this? Might be useful to explicitly choose a loader
        // rather than relying on extension/type. But have to be careful around
        // how dependency/action keys are calculated.
        //loader_name: Option<String>,
    }

    impl BassetAction for LoadPath {}

    impl BassetActionFunction for LoadPathFunction {
        type Action = LoadPath;
        type Error = BevyError;

        async fn apply(
            &self,
            context: ApplyContext<'_>,
            action: &Self::Action,
        ) -> Result<BassetActionOutput, Self::Error> {
            // XXX TODO: Try to avoid clones? But will mean changing lifetimes
            // of `BassetAction::apply`.
            let path = AssetPath::try_parse(&action.path)?.into_owned();

            // XXX TODO: Selecting a subasset should be done by the `RootAssetRef::label`,
            // not here. How do we make this more robust?
            assert!(path.label().is_none());

            let asset_server = context.asset_server();

            let (mut meta, loader, mut reader) = asset_server
                .get_meta_loader_and_reader(&path, None)
                .await
                .map_err(Into::<BevyError>::into)?;

            // XXX TODO: If we didn't override the settings then we're now dependent
            // on the meta's settings. These aren't currently expressed in the dependency
            // key or dependency graph. Decide if we make this happen or drop support
            // for metas.
            if let Some(override_settings) = &action.loader_settings {
                meta = loader.meta_from_settings(override_settings.as_bytes())?;
            }

            let settings = meta
                .loader_settings()
                .expect("XXX TODO: Sensible error message. We only support AssetAction::Load.");

            // XXX TODO: Avoid clone?
            let file_dependency =
                LoaderDependency::File(RootAssetPath::without_label(path.clone()));

            // XXX TODO: Replacement reader stuff is ugly. Compare against `ReaderRef`.
            let (mut replacement_reader, file_dependency_key) = match context
                .asset_server()
                .basset_action_source()
                .read_asset_bytes(
                    // XXX TODO: Review `clone_owned`. Are we really losing much by
                    // not doing fine grained lifetimes?
                    RootAssetPath::without_label(path.clone_owned()),
                    &mut reader,
                )
                .await?
            {
                // XXX TODO: Ugly destructuring. Review.
                Some((replacement_reader, dependency_key)) => {
                    (Some(replacement_reader), Some(dependency_key))
                }
                None => (None, None),
            };

            let chosen_reader = replacement_reader.as_mut().unwrap_or(&mut reader);

            let mut asset = internal_load_with_settings_loader_and_reader(
                asset_server,
                &path,
                settings,
                &*loader,
                chosen_reader,
                context.dependency_loading(),
                false,
            )
            .await?;

            // XXX TODO: Decide what to do here. The hash only appears to be used
            // for asset processing, so maybe can ignore for now.
            let hash = [0u8; 32];

            asset
                .loader_dependencies
                .insert(file_dependency, (hash, file_dependency_key));

            // XXX TODO: Should we be resolving the label here? See also comment
            // on `LoadPath` - we should probably rely on the `AssetRef` to handle that.
            Ok(BassetActionOutput {
                asset: asset.take_labeled(path.label_cow()).map_err(|_| {
                    BevyError::from(format!("Couldn't find labeled asset \"{path:?}\"."))
                })?,
                saved: None,
            })
        }

        fn cacheable(&self) -> bool {
            false
        }
    }
}
