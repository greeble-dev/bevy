//! XXX TODO: Documentation.
//
// XXX TODO: Consider adding a UTF8 version. This would have the (very minor?)
// benefit that an asset serialized as UTF8 (e.g. RON) could be opened in a
// text editor without binary data getting in the way.

use crate::{
    basset::{
        blob::{BlobReader, BlobWriter},
        ApplyContext,
    },
    io::SliceReader,
    meta::Settings,
    saver::ErasedAssetSaver,
    AssetPath, ErasedAssetLoader, ErasedLoadedAsset,
};
use alloc::{boxed::Box, vec::Vec};
use bevy_ecs::error::BevyError;

const STANDALONE_MAGIC: &[u8] = b"BEVY_STANDALONE_ASSET\n";
const STANDALONE_VERSION: u16 = 1;

pub async fn read_standalone_asset(
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

    let mut reader = SliceReader::new(asset_bytes);

    let load_dependencies = false;
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

    context
        .asset_server
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
        .map_err(Into::<BevyError>::into)
}

pub async fn write_standalone_asset(
    asset: &ErasedLoadedAsset,
    loader: &dyn ErasedAssetLoader,
    saver: &dyn ErasedAssetSaver,
    saver_settings: &dyn Settings,
) -> Result<Box<[u8]>, BevyError> {
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

    let meta_bytes = loader.default_meta().serialize();

    let mut writer = Vec::<u8>::new();
    {
        let mut blob = BlobWriter::new(&mut writer);

        blob.bytes(STANDALONE_MAGIC);
        blob.u16(STANDALONE_VERSION);
        blob.string(loader.type_path());
        blob.bytes_sized(&meta_bytes);
        blob.bytes_sized(&asset_bytes);
    }

    Ok(writer.into())
}
