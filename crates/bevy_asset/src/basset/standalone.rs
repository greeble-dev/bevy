//! XXX TODO: Documentation.
//
// XXX TODO: Consider adding a UTF8 version. This would have the (very minor?)
// benefit that an asset serialized as UTF8 (e.g. RON) could be opened in a
// text editor without binary data getting in the way.

use crate::{
    basset::{
        blob::{BlobReader, BlobWriter},
        cache::DependencyCacheKey,
    },
    io::SliceReader,
    meta::{AssetActionMinimal, AssetMetaMinimal, Settings},
    saver::ErasedAssetSaver,
    AssetPath, AssetServer, ErasedAssetLoader, ErasedLoadedAsset,
};
use alloc::{boxed::Box, vec::Vec};
use bevy_ecs::error::BevyError;

const STANDALONE_MAGIC: &[u8] = b"BEVY_STANDALONE_ASSET\n";
// XXX TODO: Is u16 maybe a bit risky? Might want to break into a bigger semver
// style.
const STANDALONE_VERSION: u16 = 1;

pub struct StandaloneAssetData {
    meta: Vec<u8>,
    asset: Vec<u8>,
}

// XXX TODO: More specific error type?
pub(crate) fn read_standalone_asset(blob: &[u8]) -> Result<StandaloneAssetData, BevyError> {
    let mut blob = BlobReader::new(blob);

    let magic = blob.bytes(STANDALONE_MAGIC.len()).expect("XXX TODO");

    if magic != STANDALONE_MAGIC {
        return Err("XXX TODO".into());
    }

    let version = blob.u16().expect("XXX TODO");

    if version != STANDALONE_VERSION {
        return Err("XXX TODO".into());
    }

    let meta = blob.bytes_sized().expect("XXX TODO");
    let asset = blob.bytes_sized().expect("XXX TODO");

    Ok(StandaloneAssetData {
        meta: meta.into(),
        asset: asset.into(),
    })
}

// XXX TODO: More specific error type?
pub(crate) async fn load_standalone_asset(
    data: &StandaloneAssetData,
    asset_server: &AssetServer,
    dependency_key: DependencyCacheKey,
) -> Result<ErasedLoadedAsset, BevyError> {
    let minimal_meta = ron::de::from_bytes::<AssetMetaMinimal>(&data.meta).expect("XXX TODO");

    let loader_name = match &minimal_meta.asset {
        AssetActionMinimal::Load { loader } => loader.as_str(),
        _ => todo!("XXX TODO"),
    };

    let loader = asset_server
        .get_asset_loader_with_type_name(loader_name)
        .await
        .expect("XXX TODO");

    let meta = loader.deserialize_meta(&data.meta).expect("XXX TODO");

    let mut reader = SliceReader::new(&data.asset);

    // XXX TODO: What's the correct value here? If we're in an action apply
    // context then we shouldn't load dependencies, since we only need the
    // asset value. But if we're a regular load then we do load dependencies.
    // Need to work up the call chain and see what the choice should be made.
    let load_dependencies = true;

    let populate_hashes = false;

    // Don't update the dependency cache. If we're loading from the action cache
    // then the dependencies must be already known, otherwise we wouldn't have
    // been able to calculate the action key.
    //
    // XXX TODO: This feels janky. Maybe we should be sidestepping
    // `load_with_settings_loader_and_reader` for clarity? Or generally rethink
    // how the dependency cache gets filled out.
    let update_dependency_cache = false;

    // XXX TODO: Ew? Need to decide if we try to support the original path.
    let fake_path = AssetPath::parse("ERROR - Standalone assets shouldn't use their path");

    let mut asset = asset_server
        .load_with_settings_loader_and_reader(
            &fake_path,
            meta.loader_settings().expect("meta is set to Load"),
            &*loader,
            &mut reader,
            load_dependencies,
            populate_hashes,
            update_dependency_cache,
        )
        .await
        .map_err(Into::<BevyError>::into)?;

    // XXX TODO: Janky. Need to review as this isn't the only place with some
    // fragile poking of dependency stuff (see `BassetLoader`, `LoadPath`).
    asset.dependency_key = Some(dependency_key);

    Ok(asset)
}

// XXX TODO: More specific error type?
pub(crate) async fn save_standalone_asset(
    asset: &ErasedLoadedAsset,
    loader: &dyn ErasedAssetLoader,
    saver: &dyn ErasedAssetSaver,
    saver_settings: &dyn Settings,
) -> Result<StandaloneAssetData, BevyError> {
    let mut asset_bytes = Vec::<u8>::new();

    // XXX TODO: As with reading, need to decide if we try to support the original path.
    let fake_path = AssetPath::parse("ERROR - Standalone assets shouldn't use their path");

    saver
        .save(&mut asset_bytes, asset, saver_settings, fake_path)
        .await?;

    // XXX TODO: Think through loader settings. Firstly, if the asset was loaded
    // with certain settings then we should preserve them here? There might also
    // be situations where a saver/loader pair are expecting certain settings?
    // Could get messy.
    //
    // XXX TODO: Consider using a custom meta rather than `AssetMeta` - we don't
    // want processing, so we just need the equivalent of `AssetAction::Load`.
    let meta_bytes = loader.default_meta().serialize();

    Ok(StandaloneAssetData {
        meta: meta_bytes,
        asset: asset_bytes,
    })
}

// XXX TODO: Error unnecessary?
pub(crate) fn write_standalone_asset(data: &StandaloneAssetData) -> Result<Box<[u8]>, BevyError> {
    let mut writer = Vec::<u8>::new();
    {
        let mut blob = BlobWriter::new(&mut writer);

        blob.bytes(STANDALONE_MAGIC);
        blob.u16(STANDALONE_VERSION);
        blob.bytes_sized(&data.meta);
        blob.bytes_sized(&data.asset);
    }

    Ok(writer.into())
}
