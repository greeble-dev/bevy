//! XXX TODO: Documentation.
//
// XXX TODO: Consider adding a UTF8 version. This would have the (very minor?)
// benefit that an asset serialized as UTF8 (e.g. RON) could be opened in a
// text editor without binary data getting in the way.
//
// XXX TODO: Consider alternatives to "standalone asset" name. Right now it's
// only used for the action cache, so maybe "cached asset" is better.

use crate::{
    basset::{
        blob::{BlobReader, BlobWriter},
        internal_load_with_settings_loader_and_reader, DependencyLoading, RootAssetRef,
    },
    io::{SliceReader, VecReader},
    meta::Settings,
    saver::ErasedAssetSaver,
    AssetDependency, AssetLoadError, AssetPath, AssetServer, ErasedAssetLoader, ErasedLoadedAsset,
    LoadContext, LoaderDependency,
};
use alloc::{
    boxed::Box,
    string::{String, ToString},
    sync::Arc,
    vec::Vec,
};
use bevy_ecs::error::BevyError;
use bevy_platform::collections::HashSet;
use bevy_reflect::{
    serde::{ReflectDeserializer, ReflectSerializer},
    Reflect, TypeRegistryArc,
};
use serde::de::DeserializeSeed;

// XXX TODO: Could get fancier with magic file headers. See section 3.1 of
// https://registry.khronos.org/KTX/specs/2.0/ktxspec.v2.html. Although not
// convinced it would be worth it since this isn't an interchange format.
const STANDALONE_MAGIC: &[u8] = b"BEVY_STANDALONE_ASSET\n";
// XXX TODO: Is u16 maybe a bit risky? Might want to break into a bigger semver
// style.
const STANDALONE_VERSION: u16 = 1;

#[derive(Default, Reflect)]
pub struct StandaloneAssetHeader {
    // XXX TODO: Maybe rename to `loader_type_path` so it's more explicit.
    // XXX TODO: Could be a &str?
    pub loader: String,
    pub loader_settings: String,
    pub value_dependees: Vec<LoaderDependency>,
    // XXX TODO: This doesn't distinguish between `AssetDependency::Handle` and
    // `AssetDependency::Path`. That might matter if we start prefetching, which
    // wants the former but not the latter.
    pub handle_dependees: Vec<RootAssetRef>,
}

impl StandaloneAssetHeader {
    pub fn new(
        loader: &dyn ErasedAssetLoader,
        loader_settings: &dyn Settings,
        value_dependees: Vec<LoaderDependency>,
        handle_dependees: Vec<RootAssetRef>,
    ) -> Self {
        Self {
            loader: loader.type_path().to_string(),
            loader_settings: loader.serialize_settings(loader_settings),
            value_dependees,
            handle_dependees,
        }
    }
}

pub struct StandaloneAssetData {
    pub header: StandaloneAssetHeader,
    pub asset_bytes: Vec<u8>,
}

// XXX TODO: More specific error type?
pub(crate) fn read_standalone_asset(
    blob: &[u8],
    registry: &TypeRegistryArc,
) -> Result<StandaloneAssetData, AssetLoadError> {
    let mut blob = BlobReader::new(blob);

    let magic = blob.bytes(STANDALONE_MAGIC.len()).expect("XXX TODO");

    if magic != STANDALONE_MAGIC {
        return Err(AssetLoadError::TodoError(Arc::new("XXX TODO".into())));
    }

    let version = blob.u16().expect("XXX TODO");

    if version != STANDALONE_VERSION {
        return Err(AssetLoadError::TodoError(Arc::new("XXX TODO".into())));
    }

    let header_bytes = blob.bytes_sized().expect("XXX TODO");
    let asset_bytes = blob.bytes_sized().expect("XXX TODO");

    let header = {
        let registry = registry.read();

        let mut deserializer = ron::de::Deserializer::from_bytes(header_bytes).expect("XXX TODO");

        let reflect_deserializer = ReflectDeserializer::new(&registry);

        let reflect_header = reflect_deserializer
            .deserialize(&mut deserializer)
            .expect("XXX TODO");

        reflect_header
            .try_take::<StandaloneAssetHeader>()
            .expect("XXX TODO")
    };

    Ok(StandaloneAssetData {
        header,
        asset_bytes: asset_bytes.into(),
    })
}

// XXX TODO: More specific error type?
pub(crate) async fn load_standalone_asset(
    data: &StandaloneAssetData,
    asset_server: &AssetServer,
    path: &RootAssetRef,
    dependency_loading: DependencyLoading,
) -> Result<ErasedLoadedAsset, AssetLoadError> {
    let loader = asset_server
        .get_asset_loader_with_type_name(&data.header.loader)
        .await
        .expect("XXX TODO");

    let loader_settings = loader
        .deserialize_settings(data.header.loader_settings.as_bytes())
        .expect("XXX TODO");

    let mut reader = SliceReader::new(&data.asset_bytes);

    let populate_hashes = false;

    internal_load_with_settings_loader_and_reader(
        asset_server,
        path.clone(),
        &*loader_settings,
        &*loader,
        &mut reader,
        dependency_loading,
        populate_hashes,
    )
    .await
}

// XXX TODO: More specific error type?
pub(crate) async fn save_standalone_asset(
    asset: &ErasedLoadedAsset,
    path: RootAssetRef,
    saver: &dyn ErasedAssetSaver,
    saver_settings: &dyn Settings,
    asset_server: &AssetServer,
) -> Result<StandaloneAssetData, BevyError> {
    let mut asset_bytes = Vec::<u8>::new();

    // XXX TODO: Try to avoid. We can use `path`, but that means changing
    // `AssetSaver::save` to take a `AssetRef` instead of an `AssetPath`.
    let fake_path = AssetPath::parse("ERROR - Standalone assets shouldn't use their path");

    let loader_settings = saver
        .save(&mut asset_bytes, asset, saver_settings, fake_path)
        .await?;

    let load_context = LoadContext::new(asset_server, path, false, false);

    let loader = asset_server
        .get_asset_loader_with_type_name(saver.loader_type_name())
        .await?;

    // XXX TODO: Consider how we can avoid reloading the asset to find the
    // loader dependencies. Probably requires `AssetLoader` to return the
    // dependencies, similar to `BassetAction::dependencies`. But then how do
    // we get the handle dependencies? Or we we assume the reloaded asset must
    // have the same handle dependencies as the original asset?
    let reloaded_asset = loader
        .load(
            // XXX TODO: Avoid clone?
            &mut VecReader::new(asset_bytes.clone()),
            &*loader_settings,
            load_context,
        )
        .await
        .expect("XXX TODO");

    let mut handle_dependees = HashSet::<RootAssetRef>::new();

    // XXX TODO: Exclude the asset's own sub-assets.
    reloaded_asset.visit_dependencies(asset_server.type_registry(), &mut |dependency| {
        if let Some(path) = match dependency {
            AssetDependency::Id(_) => {
                todo!("Decide if we disallow ids. Dependency tracking requires the path.")
            }
            AssetDependency::Handle(handle) => handle.path().cloned(),
            AssetDependency::Path(path) => Some(path.clone()),
        } {
            handle_dependees.insert(RootAssetRef::without_label(path));
        }
    });

    let handle_dependees = Vec::from_iter(handle_dependees);

    let value_dependees = reloaded_asset.loader_dependencies.keys().cloned().collect();

    let header = StandaloneAssetHeader::new(
        &*loader,
        &*loader_settings,
        value_dependees,
        handle_dependees,
    );

    Ok(StandaloneAssetData {
        header,
        asset_bytes,
    })
}

pub(crate) fn write_standalone_asset(
    data: &StandaloneAssetData,
    registry: &TypeRegistryArc,
) -> Result<Box<[u8]>, BevyError> {
    let header_bytes = {
        let registry = registry.read();
        let serializer = ReflectSerializer::new(&data.header, &registry);

        ron::ser::to_string(&serializer)
            .expect("XXX TODO")
            .into_bytes()
    };

    let mut writer = Vec::<u8>::new();
    {
        let mut blob = BlobWriter::new(&mut writer);

        blob.bytes(STANDALONE_MAGIC);
        blob.u16(STANDALONE_VERSION);
        blob.bytes_sized(&header_bytes);
        blob.bytes_sized(&data.asset_bytes);
    }

    Ok(writer.into())
}
