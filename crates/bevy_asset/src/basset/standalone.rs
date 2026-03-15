//! XXX TODO

use crate::{
    basset::{
        blob::{BlobReader, BlobWriter},
        ActionCacheKey, ApplyContext, DependencyCacheKey, ImmediateDependeeActionKeys,
    },
    io::SliceReader,
    meta::{AssetHash, Settings},
    saver::ErasedAssetSaver,
    AssetPath, AssetRef, ErasedAssetLoader, ErasedLoadedAsset,
};
use alloc::{boxed::Box, vec::Vec};
use bevy_ecs::error::BevyError;
use bevy_platform::collections::HashMap;
use serde::{Deserialize, Serialize};

const STANDALONE_MAGIC: &[u8] = b"BEVY_STANDALONE_ASSET\n";
const STANDALONE_VERSION: u16 = 1;

#[derive(Clone, PartialEq, Eq, Debug, Serialize, Deserialize)]
pub struct StandaloneAssetInfo {
    pub action_key: ActionCacheKey,
    pub dependency_key: DependencyCacheKey,
    pub immediate_dependee_action_keys: ImmediateDependeeActionKeys,
}

pub async fn read_standalone_asset(
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
    // XXX TODO: Maybe better if we sidestepped `load_with_settings_loader_and_reader`
    // for clarity? Or generally rethink how the dependency cache gets filled out.
    let update_dependency_cache = false;

    let mut asset = context
        .asset_server
        .load_with_settings_loader_and_reader(
            original_path,
            meta.loader_settings().expect("meta is set to Load"),
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

pub async fn write_standalone_asset(
    asset: &ErasedLoadedAsset,
    loader: &dyn ErasedAssetLoader,
    saver: &dyn ErasedAssetSaver,
    saver_settings: &dyn Settings,
    info: &StandaloneAssetInfo,
) -> Result<Box<[u8]>, BevyError> {
    let mut asset_bytes = Vec::<u8>::new();

    let dummy_path = AssetPath::parse("NOT IMPLEMENTED"); // XXX TODO?

    saver
        .save(&mut asset_bytes, asset, saver_settings, dummy_path)
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
    blob.string(loader.type_path());
    blob.bytes_sized(&meta_bytes);
    blob.bytes_sized(&info_bytes);
    blob.bytes_sized(&asset_bytes);

    Ok(writer.into())
}
