//! Basset proof of concept.

use crate::{
    basset::{
        action::{LoadPath, LoadPathParams},
        cache::{
            ActionCacheKey, BassetHash, CacheLoaderDependency, ContentCache, DependencyCacheKey,
            DependencyCacheValue, MemoryAndFileCache,
        },
        dependency_graph::DependencyGraph,
        publisher::{
            write_pack_file, PublishDependency, PublishInput, ReadablePackFile, StagedAsset,
            WritablePackFile,
        },
        standalone::{read_standalone_asset, write_standalone_asset},
    },
    io::{AssetReaderError, AssetSourceId, AssetSources},
    meta::{AssetActionMinimal, AssetHash, AssetMetaMinimal},
    Asset, AssetApp, AssetDependency, AssetPath, AssetServer, LoaderDependency, PolyAssetLoader,
};
use alloc::{boxed::Box, string::ToString, sync::Arc, vec::Vec};
use atomicow::CowArc;
use bevy_app::{App, Plugin};
use bevy_asset::{
    io::Reader,
    meta::Settings,
    saver::{AssetSaver, ErasedAssetSaver},
    AssetRef, ErasedLoadedAsset, LoadContext, LoadedAsset,
};
use bevy_ecs::{error::BevyError, reflect::AppTypeRegistry};
use bevy_platform::{
    collections::{HashMap, HashSet},
    hash::FixedHasher,
};
use bevy_reflect::{
    reflect_trait,
    serde::{
        DeserializeWithRegistry, ReflectDeserializeWithRegistry, ReflectDeserializer,
        ReflectSerializeWithRegistry, ReflectSerializer, SerializeWithRegistry,
        TypedReflectDeserializer,
    },
    Reflect, ReflectDeserialize, ReflectFromReflect, ReflectSerialize, TypePath, TypeRegistry,
    TypeRegistryArc,
};
use bevy_tasks::{BoxedFuture, ConditionalSendFuture};
use core::{
    any::{type_name, TypeId},
    fmt::{Debug, Display},
    hash::{BuildHasher, Hash, Hasher},
    ops::Deref,
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
use tracing::{debug, info, warn};

mod blob;
pub mod cache;
mod dependency_graph;
pub mod publisher;
mod standalone;

// XXX TODO: Is this necessary any more? Maybe just fold into `AssetPlugin`.
pub struct BassetPlugin;

impl Plugin for BassetPlugin {
    fn build(&self, app: &mut App) {
        app.register_poly_asset_loader(BassetLoader(
            app.world()
                .get_resource::<AppTypeRegistry>()
                .expect("XXX TODO")
                .deref()
                .clone(),
        ));
    }
}

/// An `AssetPath` without a label.
//
// XXX TODO: Maybe think more about the name. "root asset" does match other parts
// of the asset system, but it's a bit ambiguous. E.g. publishing wants a list
// of "root assets", but they're the roots of the publishing tree.
#[derive(Eq, PartialEq, Ord, PartialOrd, Hash, Clone, Debug, Reflect)]
#[reflect(opaque)]
#[reflect(Serialize, Deserialize)]
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

/// An `AssetRef` without a label.
#[derive(Eq, PartialEq, Ord, PartialOrd, Hash, Clone, Debug, Reflect)]
#[reflect(opaque)]
#[reflect(SerializeWithRegistry, DeserializeWithRegistry)]
pub struct RootAssetRef {
    params: ErasedBassetActionParams,
}

impl RootAssetRef {
    pub fn new<P: BassetActionParams>(params: P) -> Self {
        Self {
            params: ErasedBassetActionParams::new(Arc::new(params)),
        }
    }

    pub fn without_label(value: AssetRef<'_>) -> Self {
        Self {
            params: value.params().clone(),
        }
    }

    fn params(&self) -> &ErasedBassetActionParams {
        &self.params
    }
}

impl Display for RootAssetRef {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        write!(f, "{:?}", self.params)
    }
}

impl SerializeWithRegistry for RootAssetRef {
    fn serialize<S>(&self, serializer: S, registry: &TypeRegistry) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let mut s = serializer.serialize_struct("AssetRef", 3)?;

        s.serialize_field(
            "params",
            &ReflectSerializer::new((*self.params.0).as_partial_reflect(), registry),
        )?;

        s.end()
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
                formatter.write_str("struct RootAssetRef")
            }

            fn visit_seq<V>(self, mut seq: V) -> Result<RootAssetRef, V::Error>
            where
                V: SeqAccess<'de>,
            {
                let params_dyn = seq
                    .next_element_seed(ReflectDeserializer::new(self.registry))?
                    .ok_or_else(|| serde::de::Error::invalid_length(0, &self))?;

                // XXX TODO: Do we need `params_dyn.get_represented_type_info()" if we already know the type?
                let params_registration = self
                    .registry
                    .get_with_type_path(params_dyn.get_represented_type_info().unwrap().type_path())
                    .expect("XXX TODO?");

                let params_concrete = params_registration
                    .data::<ReflectFromReflect>()
                    .unwrap()
                    .from_reflect(&*params_dyn)
                    .unwrap();

                let params = params_registration
                    .data::<ReflectBassetActionParams>()
                    .expect("XXX TODO?")
                    .get_boxed(params_concrete)
                    .expect("XXX TODO?");

                Ok(RootAssetRef {
                    params: ErasedBassetActionParams(params.into()),
                })
            }

            fn visit_map<V>(self, mut map: V) -> Result<RootAssetRef, V::Error>
            where
                V: MapAccess<'de>,
            {
                #[derive(Deserialize)]
                #[serde(field_identifier, rename_all = "lowercase")]
                enum Field {
                    Params,
                }

                let mut params = None;

                while let Some(key) = map.next_key()? {
                    match key {
                        Field::Params => {
                            if params.is_some() {
                                return Err(serde::de::Error::duplicate_field("params"));
                            }

                            let params_dyn =
                                map.next_value_seed(ReflectDeserializer::new(self.registry))?;

                            // XXX TODO: Duplicates a bunch of `visit_seq`. Refactor?
                            let params_registration = self
                                .registry
                                .get_with_type_path(
                                    params_dyn.get_represented_type_info().unwrap().type_path(),
                                )
                                .expect("XXX TODO?");

                            let params_concrete = params_registration
                                .data::<ReflectFromReflect>()
                                .unwrap()
                                .from_reflect(&*params_dyn)
                                .unwrap();

                            params = Some(
                                params_registration
                                    .data::<ReflectBassetActionParams>()
                                    .expect("XXX TODO?")
                                    .get_boxed(params_concrete)
                                    .expect("XXX TODO?"),
                            );
                        }
                    }
                }

                let params = params.ok_or_else(|| serde::de::Error::missing_field("params"))?;

                Ok(RootAssetRef {
                    params: ErasedBassetActionParams(params.into()),
                })
            }
        }

        deserializer.deserialize_struct(
            "RootAssetRef",
            &["params"],
            RootAssetRefVisitor { registry },
        )
    }
}

impl From<RootAssetRef> for AssetRef<'_> {
    fn from(value: RootAssetRef) -> Self {
        Self {
            params: value.params,
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
        RootAssetRef::new(LoadPathParams {
            path: value.to_string(),
            ..Default::default()
        })
    }
}

/// XXX TODO: Document. Review where this overlaps with `LoadContext` and check
/// if they can be combined.
pub struct ApplyContext<'a> {
    asset_server: &'a AssetServer,
    dependency_key: Option<DependencyCacheKey>,

    // Equivalent to `ErasedLoadedAsset::loader_dependencies`.
    // XXX TODO: Dependency cache key shouldn't be optional for us?
    loader_dependencies: HashMap<LoaderDependency, (AssetHash, Option<DependencyCacheKey>)>,
}

impl<'a> ApplyContext<'a> {
    pub fn new(asset_server: &'a AssetServer, dependency_key: Option<DependencyCacheKey>) -> Self {
        Self {
            asset_server,
            dependency_key,
            loader_dependencies: Default::default(),
        }
    }

    // XXX TODO: Don't expose this. Might only be used by `LoadPath`, so we should
    // probably move some of that logic into ApplyContext.
    pub(crate) fn asset_server(&'a self) -> &'a AssetServer {
        self.asset_server
    }
}

impl ApplyContext<'_> {
    pub async fn erased_load_dependee(
        &mut self,
        path: &AssetRef<'static>,
    ) -> Result<ErasedLoadedAsset, BevyError> {
        // XXX TODO: Avoid clone?
        let asset = self
            .asset_server
            .basset_action_source()
            .apply(
                &RootAssetRef::without_label(path.clone()),
                self.asset_server,
            )
            .await?;

        // XXX TODO: Decide what to do here. The hash only appears to be used
        // for asset processing, so maybe can ignore for now.
        let hash = [0u8; 32];

        self.loader_dependencies.insert(
            LoaderDependency::Load(RootAssetRef::without_label(path.clone())),
            (hash, asset.dependency_key),
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
        loaded_asset.dependency_key = self.dependency_key;

        loaded_asset.into()
    }
}

// XXX TODO: Review `Debug` bound.
#[reflect_trait]
pub trait BassetActionParams: Downcast + Send + Sync + 'static + Reflect + Debug {
    // XXX TODO: `PartialReflect` exposes some utilities for `hash` and `eq`. Can
    // they replace our implementation? See `PartialReflect::reflect_hash`.
    fn hash(&self) -> u64;
    fn eq(&self, other: &dyn BassetActionParams) -> bool;
    fn type_name(&self) -> &'static str;
}

// XXX TODO: Review this. Duplicated from `bevy_asset::meta::Settings`.
impl_downcast!(BassetActionParams);

// XXX TODO: Document? See bevy_reflect where it does `impl TypePath for dyn Enemy`.
impl TypePath for dyn BassetActionParams {
    fn type_path() -> &'static str {
        "dyn bevy_asset::basset::BassetActionParams"
    }

    fn short_type_path() -> &'static str {
        "dyn BassetActionParams"
    }
}

impl<T> BassetActionParams for T
where
    T: Send + Sync + 'static + Hash + PartialEq + Reflect + Debug,
{
    fn hash(&self) -> u64 {
        // XXX TODO: Should we include the type name of `T` as well?
        FixedHasher.hash_one(self)
    }

    fn eq(&self, other: &dyn BassetActionParams) -> bool {
        if let Some(downcast) = other.downcast_ref::<T>() {
            self == downcast
        } else {
            false
        }
    }

    fn type_name(&self) -> &'static str {
        type_name::<T>()
    }
}

// XXX TODO: Decide if member should be pub?
#[derive(Clone)]
pub struct ErasedBassetActionParams(pub Arc<dyn BassetActionParams>);

impl ErasedBassetActionParams {
    pub fn new(params: Arc<dyn BassetActionParams>) -> Self {
        Self(params)
    }

    pub fn type_id(&self) -> TypeId {
        self.0.type_id()
    }

    pub fn type_name(&self) -> &'static str {
        self.0.type_name()
    }
}

impl Hash for ErasedBassetActionParams {
    fn hash<H: Hasher>(&self, state: &mut H) {
        state.write_u64(self.0.hash());
    }
}

impl PartialEq for ErasedBassetActionParams {
    fn eq(&self, other: &Self) -> bool {
        self.0.eq(&*other.0)
    }
}

impl Eq for ErasedBassetActionParams {}

// XXX TODO: Can't we automatically derive this since we've implemented `Ord`?
impl PartialOrd for ErasedBassetActionParams {
    fn partial_cmp(&self, other: &Self) -> Option<core::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for ErasedBassetActionParams {
    fn cmp(&self, other: &Self) -> core::cmp::Ordering {
        // XXX TODO: Decide if this is good enough or if we need a proper Ord.
        // Making clients implement `Ord` for all params is gonna suck though.
        self.0.hash().cmp(&other.0.hash())
    }
}

impl Debug for ErasedBassetActionParams {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        self.0.fmt(f)
    }
}

/// XXX TODO: Document.
///
/// An action that takes a parameter struct of a known type and returns an
/// `ErasedLoadedAsset`.
pub trait BassetAction: Send + Sync + 'static {
    type Params: BassetActionParams;

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

    /// XXX TODO: Consider name. It implies publishable as well?
    fn cacheable(&self) -> bool {
        true
    }
}

pub trait ErasedBassetAction: Send + Sync + 'static {
    fn apply<'a>(
        &'a self,
        context: ApplyContext<'a>,
        // XXX TODO: Could consider taking `&' dyn BassetActionParams`? More general,
        // but also more risk if we need something from `ErasedBassetActionParams` like a debug type name.
        params: &'a ErasedBassetActionParams,
    ) -> BoxedFuture<'a, Result<ErasedLoadedAsset, BevyError>>;

    fn cacheable(&self) -> bool;
}

impl<T> ErasedBassetAction for T
where
    T: BassetAction + Send + Sync,
{
    fn apply<'a>(
        &'a self,
        context: ApplyContext<'a>,
        params: &'a ErasedBassetActionParams,
    ) -> BoxedFuture<'a, Result<ErasedLoadedAsset, BevyError>> {
        Box::pin(async move {
            let params = params.0.downcast_ref::<T::Params>().expect("XXX TODO");

            T::apply(self, context, params).await.map_err(Into::into)
        })
    }

    fn cacheable(&self) -> bool {
        BassetAction::cacheable(self)
    }
}

pub struct DevelopmentActionSourceSettings {
    file_cache_path: Option<PathBuf>,
    validate_dependency_cache: bool,
    validate_action_cache: bool,
    // XXX TODO: Better to use `TypeId` or type name?
    action_params_type_id_to_action_function: HashMap<TypeId, Box<dyn ErasedBassetAction>>,
    asset_type_name_to_saver: HashMap<&'static str, (Box<dyn ErasedAssetSaver>, Box<dyn Settings>)>,
}

impl Default for DevelopmentActionSourceSettings {
    fn default() -> Self {
        let mut action_params_type_id_to_action_function =
            HashMap::<TypeId, Box<dyn ErasedBassetAction>>::new();

        // XXX TODO: Review if we should be automatically adding `LoadPath`
        // here. Maybe should be added afterwards so it's not exposed to the
        // user. Maybe shouldn't even go in here since it could be special cased?
        // It's likely the `LoadPath` will be the most common action, so making
        // `DevelopmentActionSource::action_function` check for `LoadPath` before
        // the hash lookup might pay off.
        action_params_type_id_to_action_function
            .insert(TypeId::of::<LoadPathParams>(), Box::new(LoadPath));

        Self {
            file_cache_path: Default::default(),
            validate_dependency_cache: Default::default(),
            validate_action_cache: Default::default(),
            action_params_type_id_to_action_function,
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
        self.action_params_type_id_to_action_function
            .insert(TypeId::of::<T::Params>(), Box::new(action));

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
    content_cache: ContentCache,
    dependency_graph: Option<DependencyGraph>,
    action_cache: Option<MemoryAndFileCache<ActionCacheKey, Arc<[u8]>>>,
}

impl DevelopmentActionSource {
    pub(crate) fn new(
        settings: Arc<DevelopmentActionSourceSettings>,
        sources: Arc<AssetSources>,
        registry: TypeRegistryArc,
    ) -> Self {
        let dependency_graph = Some(DependencyGraph::new(
            settings
                .file_cache_path
                .as_ref()
                .map(|p| p.join("dependency")),
            settings.validate_dependency_cache,
            registry.clone(),
        ));

        let action_cache = Some(MemoryAndFileCache::new(
            "action_cache",
            settings.file_cache_path.as_ref().map(|p| p.join("action")),
            settings.validate_action_cache,
            registry,
        ));

        Self {
            settings,
            content_cache: ContentCache::new(sources),
            dependency_graph,
            action_cache,
        }
    }

    // XXX TODO: Clarify that this is mapping the `BassetActionParams` type name to action.
    pub(crate) fn action_function<'a>(
        &'a self,
        type_id: TypeId,
    ) -> Option<&'a dyn ErasedBassetAction> {
        self.settings
            .action_params_type_id_to_action_function
            .get(&type_id)
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

    // XXX TODO: If we know the path is an action then we don't need async? Is that
    // worth optimizing for?
    async fn dependency_key_internal(
        &self,
        path: &LoaderDependency,
        _settings: Option<&dyn Settings>,
    ) -> DependencyCacheKey {
        // XXX TODO: Seed hash?
        let mut hasher = blake3::Hasher::new();

        // XXX TODO: Should we be including settings in the dependency key? Unclear
        // if needed now with that we have the `LoadPath` action.
        //
        // if let Some(meta) = meta {
        //     hasher.update(&meta.serialize());
        // }

        // XXX TODO: Should we also mix in something here to make sure that the two
        // `LoaderDependency` variants are separate?

        match path {
            LoaderDependency::Load(action) => {
                // XXX TODO: Double check this is appropriate for actions.
                hasher.update(action.to_string().as_bytes());
            }
            LoaderDependency::File(path) => {
                hasher.update(path.to_string().as_bytes());
                let content_hash = self.content_cache.get(path).await.expect("XXX TODO");

                hasher.update(&content_hash.as_bytes());
            }
        }

        // XXX TODO: if `LoaderDependency::Load` then we should include the loader versions.

        DependencyCacheKey(BassetHash::new(*hasher.finalize().as_bytes()))
    }
}

impl ActionSource for DevelopmentActionSource {
    fn apply<'a>(
        &'a self,
        action: &'a RootAssetRef,
        asset_server: &'a AssetServer,
    ) -> BoxedFuture<'a, Result<ErasedLoadedAsset, BevyError>> {
        Box::pin(async move {
            let action_function =
                self.action_function(action.params().type_id())
                    .ok_or_else(|| {
                        // XXX TODO: Clarify error?
                        BevyError::from(format!(
                            "Couldn't find action \"{}\")",
                            action.params().type_name()
                        ))
                    })?;

            if action_function.cacheable()
                && let Some(action_cache) = &self.action_cache
                && let Some(dependency_graph) = &self.dependency_graph
            {
                // XXX TODO: Maybe early out here if there's no saver? Depends if we end
                // up in a situation where the saver has been compiled out but we still
                // want to read from the cache.

                let action_key = dependency_graph.action_key(action, self).await;

                if let Some(action_key) = action_key
                    && let Some(cached_standalone_asset) =
                        action_cache.get(&action_key, action).await
                {
                    return read_standalone_asset(&cached_standalone_asset, asset_server).await;
                }
            }

            // XXX TODO: Avoid clone?
            // XXX TODO: Could have a fast path here since we know the dependency key
            // is for an action?
            let dependency_key = self
                .dependency_key_internal(&LoaderDependency::Load(action.clone()), None)
                .await;

            let apply_context = ApplyContext::new(asset_server, Some(dependency_key));

            let asset = action_function
                .apply(apply_context, action.params())
                .await?;

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
            let action_key = if let Some(future) = self.register_dependencies(action, None, &asset)
            {
                future.await
            } else {
                None
            };

            if let Some(action_key) = action_key {
                if action_function.cacheable()
                    && let Some(action_cache) = &self.action_cache
                {
                    if let Some((saver, settings)) = self.saver(asset.asset_type_name()) {
                        // XXX TODO: Support action outputs with sub-assets. Could be troublesome
                        // as there's two potential cases:
                        //
                        // 1. The asset saver for the root asset expects to be given the sub-assets.
                        // 2. The root asset and sub-assets should be saved by separate savers.
                        assert!(
                            asset.labeled_assets.is_empty(),
                            "XXX TODO. Not supported yet. {action:?}",
                        );

                        // XXX TODO: Verify loader matches saver? Need a way to get
                        // `AssetSaver::OutputLoader` out of `ErasedAssetSaver`.
                        let loader = asset_server
                            .get_asset_loader_with_type_name(saver.loader_type_name())
                            .await
                            .expect("XXX TODO");

                        let blob =
                            write_standalone_asset(&asset, &*loader, saver, settings).await?;

                        action_cache.put(action_key, blob.into(), action);
                    } else {
                        let type_name = asset.asset_type_name();
                        debug!(?type_name, ?action, "Cache ineligible, no saver for type.");
                    }
                }
            } else {
                warn!(
                    ?action,
                    "Register dependencies did not return an action key."
                );
            }

            Ok(asset)
        })
    }

    fn dependency_key<'a>(
        &'a self,
        path: &'a LoaderDependency,
        settings: Option<&'a dyn Settings>,
    ) -> Option<BoxedFuture<'a, Option<DependencyCacheKey>>> {
        Some(Box::pin(async move {
            Some(self.dependency_key_internal(path, settings).await)
        }))
    }

    // XXX TODO: Settings?
    fn register_dependencies<'a>(
        &'a self,
        path: &'a RootAssetRef,
        // XXX TODO: Settings?
        _settings: Option<&'a dyn Settings>,
        asset: &'a ErasedLoadedAsset,
    ) -> Option<BoxedFuture<'a, Option<ActionCacheKey>>> {
        Some(Box::pin(async move {
            // XXX TODO: Can move branch outside of `Box::pin`?
            if let Some(dependency_key) = asset.dependency_key
                && let Some(dependency_graph) = &self.dependency_graph
            {
                let Some(loader_dependencies) = asset
                    .loader_dependencies
                    .iter()
                    .map(|(path, (_, dependency_key))| {
                        CacheLoaderDependency::optional(path.clone(), *dependency_key)
                    })
                    .collect::<Option<Vec<_>>>()
                else {
                    // XXX TODO: Double check we're correct to bail here. Should be a warning
                    // since the development source always gives dependency keys?
                    return None;
                };

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
            } else {
                None
            }
        }))
    }

    fn register_bytes_dependency<'a>(
        &'a self,
        path: &'a RootAssetPath<'static>,
        dependency_key: Option<DependencyCacheKey>,
    ) -> Option<BoxedFuture<'a, Option<ActionCacheKey>>> {
        Some(Box::pin(async move {
            // XXX TODO: Can move branch outside of `Box::pin`?
            if let Some(dependency_key) = dependency_key
                && let Some(dependency_graph) = &self.dependency_graph
            {
                dependency_graph.register_dependencies_file(path, dependency_key)
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
            let mut done = HashSet::<PublishDependency>::new();

            // XXXX TODO: Consume rather than clone?
            let mut input_stack = input.paths.clone();

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
                    // PublishDependency::Load(RootAssetRef::Path(path)) => {
                    //     // XXX TODO: Try using dependency key to avoid this load - it's
                    //     // only needed for discovering dependencies and getting the meta.
                    //     // Can we try and reuse the dependency graph? Implies that
                    //     // dependency graph should store the loader name?

                    //     // XXX TODO: Settings parameter?
                    //     let (loaded, loader) = load_path(asset_server, path, &None)
                    //         .await
                    //         .expect("XXX TODO");

                    //     // XXX TODO: Duplicated in `RootAssetRef::Action` case?
                    //     loaded.visit_dependencies(&mut |dependency| {
                    //         if let Some(path) = match dependency {
                    //             AssetDependency::Id(_) => todo!("Decide if we disallow ids. Dependency tracking requires the path."),
                    //             AssetDependency::Handle(handle) => handle.path().cloned(),
                    //             AssetDependency::Path(path) => Some(path.clone()),
                    //         } {
                    //             // XXX TODO: Oh, this is awkward. We shouldn't assume
                    //             // `PublishDependency::Load` here. Does `VisitAssetDependencies`
                    //             // need to make a distinction? That will be ugly.
                    //             input_stack.push(PublishDependency::Load(RootAssetRef::without_label(path)));
                    //         };
                    //     });

                    //     input_stack.extend(
                    //         loaded
                    //             .loader_dependencies
                    //             .keys()
                    //             .cloned()
                    //             .map(PublishDependency::from),
                    //     );

                    //     let asset_bytes = {
                    //         let mut reader = asset_server
                    //             .get_source(path.source())
                    //             .expect("XXX TODO")
                    //             .reader()
                    //             .read(path.path())
                    //             .await
                    //             .expect("XXX TODO");
                    //         let mut bytes = Vec::new();
                    //         reader.read_to_end(&mut bytes).await.expect("XXX TODO");
                    //         bytes.into()
                    //     };

                    //     // XXX TODO: Sort out loader settings? See above where we call `load_path`.
                    //     let meta_bytes = loader.default_meta().serialize().into_boxed_slice();

                    //     // XXX TODO: The action case somewhat duplicates this. Refactor?

                    //     pack.paths.insert(
                    //         ManifestPath::from(path),
                    //         StagedAsset {
                    //             asset_bytes,
                    //             meta_bytes,
                    //         },
                    //     );
                    // }
                    PublishDependency::Load(action) => {
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
                                // XXX TODO: Similar to case above - shouldn't assume PublishDependency::Load.
                                input_stack.push(PublishDependency::Load(RootAssetRef::without_label(path)));
                            };
                        });

                        // XXX TODO: We're not accounting for the standalone asset having
                        // loader dependencies. Not sure if we can work them out unless
                        // we do a fake load?
                    }
                    PublishDependency::File(_path) => {
                        // Should we check done so that we're not publishing the same
                        // bytes as `PublishDependency::Load` and `PublishDependency::File`?
                        todo!();
                    }
                }

                // XXX TODO: Prettier output, don't use `Debug`.
                info!(?input_asset, "Publishing");
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

#[derive(Debug, Clone, Reflect)]
struct BassetFileSerializable {
    // XXX TODO: Consider renaming, could get confused with "root asset" versus "sub-asset".
    root: AssetRef<'static>,
    // TODO: Versioning?
}

// XXX TODO: Could be avoided if `LoadContext` or `AssetServer` exposed the
// type registry. Is that a step too far?
#[derive(TypePath)]
struct BassetLoader(TypeRegistryArc);

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

        let asset_server = load_context.asset_server();

        let mut bytes = Vec::new();
        reader.read_to_end(&mut bytes).await?;

        let basset = {
            let registry = self.0.read();

            let registration = registry
                .get(TypeId::of::<BassetFileSerializable>())
                .expect("XXX TODO");

            TypedReflectDeserializer::new(registration, &registry)
                .deserialize(&mut ron::de::Deserializer::from_bytes(&bytes).expect("XXX TODO"))
                .expect("XXX TODO")
                .try_take::<BassetFileSerializable>()
                .expect("XXX TODO")
        };

        let root_without_label = RootAssetRef::without_label(basset.root.clone());

        let asset = asset_server
            .basset_action_source()
            .apply(&root_without_label, asset_server)
            .await
            .map(|mut asset| {
                // XXX TODO: See other cases of ignoring the hash.
                let hash = [0u8; 32];

                // XXX TODO: Justify this dependency replacement.
                asset.loader_dependencies.clear();
                asset.loader_dependencies.insert(
                    LoaderDependency::Load(RootAssetRef::without_label(basset.root.clone())),
                    (hash, asset.dependency_key),
                );

                asset
            })?;

        asset
            .take_labeled(basset.root.label_cow())
            .map_err(|_| format!("Couldn't find labeled asset \"{:?}\".", &basset.root).into())
    }

    fn extensions(&self) -> &[&str] {
        &["basset"]
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
    ) -> BoxedFuture<'a, Result<ErasedLoadedAsset, BevyError>>;

    fn dependency_key<'a>(
        &'a self,
        _path: &'a LoaderDependency,
        _settings: Option<&'a dyn Settings>,
    ) -> Option<BoxedFuture<'a, Option<DependencyCacheKey>>> {
        None
    }

    // XXX TODO: Try and avoid exposing this. It's currently only for development
    // mode to fill out the dependency graph from `AssetServer::load_with_settings_loader_and_reader`.
    //
    // XXX TODO: Review return type. Returning an option is a faff, but avoids
    // redundantly created a boxed future it's a noop.
    fn register_dependencies<'a>(
        &'a self,
        // XXX TODO: Consider `RootAssetRef<'a>`?
        _path: &'a RootAssetRef,
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
    //
    // XXX TODO: Review name. Compare to `LoaderDependency::File`.
    fn register_bytes_dependency<'a>(
        &'a self,
        // XXX TODO: Consider `RootAssetRef<'a>`?
        _path: &'a RootAssetPath<'static>,
        _dependency_key: Option<DependencyCacheKey>,
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
    fn build(&self, sources: Arc<AssetSources>, registry: TypeRegistryArc)
        -> Arc<dyn ActionSource>;
}

// XXX TODO: Review use cases.
async fn fallback_action_handler(
    action: &RootAssetRef,
    asset_server: &AssetServer,
    action_source: &dyn ActionSource,
) -> Result<ErasedLoadedAsset, BevyError> {
    // XXX TODO: Check if there's a better way to handle this.
    if action.params.type_id() == TypeId::of::<LoadPathParams>() {
        // XXX TODO: Is there ever a situation where the dependency key will be found
        // here? `fallback_action_handler` is currently only used by minimal and
        // published action sources, which don't support dependency keys.
        // XXX TODO: Avoid clone?
        // XXX TODO: Could have a fast path here since we know the dependency
        // key is for an action?
        let dependency_key = if let Some(future) =
            action_source.dependency_key(&LoaderDependency::Load(action.clone()), None)
        {
            future.await
        } else {
            None
        };

        ErasedBassetAction::apply(
            &LoadPath,
            ApplyContext::new(asset_server, dependency_key),
            &action.params,
        )
        .await
    } else {
        // XXX TODO?
        Err(AssetReaderError::NotFound(action.to_string().into()).into())
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
    ) -> BoxedFuture<'a, Result<ErasedLoadedAsset, BevyError>> {
        Box::pin(async move { fallback_action_handler(action, asset_server, self).await })
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
    ) -> BoxedFuture<'a, Result<ErasedLoadedAsset, BevyError>> {
        Box::pin(async move {
            // XXX TODO: Review how we should be referencing assets. Maybe want
            // a distinct type for safety?
            let action_string = action.to_string();

            // XXX TODO: The below duplicates a fair amount of `read_standalone_asset`.
            // Refactor?

            let Ok(mut readers) = self.pack_file.action(&action_string) else {
                return fallback_action_handler(action, asset_server, self).await;
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
            let fake_path = AssetPath::parse("ERROR - published assets shouldn't use their path");

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
    use core::panic::AssertUnwindSafe;

    use crate::{AssetLoadError, AssetLoaderError};

    use super::*;
    use alloc::string::String;

    // XXX TODO: Consider renaming to `LoadWithSettings` and making the `settings`
    // member non-optional? Not sure if we have any use case outside of `load_with_settings.`
    pub struct LoadPath;

    #[derive(Reflect, Default, Hash, PartialEq, Debug)]
    #[reflect(BassetActionParams)]
    pub struct LoadPathParams {
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

    impl BassetAction for LoadPath {
        type Params = LoadPathParams;
        type Error = BevyError;

        async fn apply(
            &self,
            context: ApplyContext<'_>,
            params: &Self::Params,
        ) -> Result<ErasedLoadedAsset, Self::Error> {
            // XXX TODO: Try to avoid clones? But will mean changing lifetimes
            // of `BassetAction::apply`.
            let path = AssetPath::parse(&params.path).into_owned();

            // XXX TODO: Selecting a subasset should be done by the `RootAssetRef::label`,
            // not here. How do we make this more robust?
            assert!(path.label().is_none());

            let asset_server = context.asset_server();

            let (mut meta, loader, mut reader) = asset_server
                .get_meta_loader_and_reader(&path, None)
                .await
                .map_err(Into::<BevyError>::into)?;

            if let Some(override_settings) = &params.loader_settings {
                meta = loader.meta_from_settings(override_settings.as_bytes())?;
            }

            let settings = meta
                .loader_settings()
                .expect("XXX TODO: We should only support load metas");

            let file_dependency_path = RootAssetPath::without_label(path.clone());

            // XXX TODO: Avoid clone?
            let file_dependency = LoaderDependency::File(file_dependency_path.clone());

            let file_dependency_key = if let Some(future) = asset_server
                .basset_action_source()
                .dependency_key(&file_dependency, None)
            {
                future.await
            } else {
                None
            };

            if let Some(future) = asset_server
                .basset_action_source()
                .register_bytes_dependency(&file_dependency_path, file_dependency_key)
            {
                future.await;
            };

            // XXX TODO: This is mostly a copy and paste of `load_with_settings_loader_and_reader`,
            // minus dependency registration. Investigate refactoring.

            // XXX TODO: Not loading dependencies can be wrong? Should be context
            // dependent - if we're a loader dependency of another action then we
            // shouldn't be loading dependencies. But if we're a regular load
            // then we should?
            let load_dependencies = true;

            let populate_hashes = false;

            let load_context = LoadContext::new(
                asset_server,
                path.clone(),
                load_dependencies,
                populate_hashes,
                context.dependency_key,
            );
            let load =
                AssertUnwindSafe(loader.load(&mut reader, settings, load_context)).catch_unwind();
            #[cfg(feature = "trace")]
            let load = {
                use tracing::Instrument;

                let span = tracing::info_span!(
                    "asset loading",
                    loader = loader.type_path(),
                    asset = path.to_string()
                );
                load.instrument(span)
            };
            let mut asset = load
                .await
                .map_err(|_| AssetLoadError::AssetLoaderPanic {
                    path: path.clone_owned().into(),
                    loader_name: loader.type_path(),
                })?
                .map_err(|e| {
                    AssetLoadError::AssetLoaderError(AssetLoaderError {
                        path: path.clone_owned().into(),
                        loader_name: loader.type_path(),
                        error: e.into(),
                    })
                })
                .map_err(Into::<BevyError>::into)?;

            // XXX TODO: Decide what to do here. The hash only appears to be used
            // for asset processing, so maybe can ignore for now.
            let hash = [0u8; 32];

            asset
                .loader_dependencies
                .insert(file_dependency, (hash, file_dependency_key));

            // XXX TODO: Should we be resolving the label here? See also comment
            // on `LoadPath` - we should probably rely on the `AssetRef` to handle that.
            asset
                .take_labeled(path.label_cow())
                .map_err(|_| format!("Couldn't find labeled asset \"{path:?}\".").into())
        }

        fn cacheable(&self) -> bool {
            false
        }
    }
}
