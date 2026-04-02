#![expect(unused, reason = "XXX TODO")]

use crate::{
    basset::{
        blob::{BlobReader, BlobWriter},
        load_path,
        standalone::write_standalone_asset,
        RootAssetAction2, RootAssetPath, RootAssetRef,
    },
    io::{
        AssetReader, AssetReaderError, AssetSourceBuilder, AssetSourceId, PathStream, Reader,
        SliceReader,
    },
    AssetPath, AssetRef, AssetServer,
};
use alloc::{boxed::Box, string::ToString, sync::Arc, vec::Vec};
// XXX TODO: Try and replace `async_fs` with `AssetSource`.
use async_fs::File;
use bevy_ecs::error::BevyError;
use bevy_platform::collections::{HashMap, HashSet};
use core::{
    pin::Pin,
    task::{Context, Poll},
};
use futures_lite::{AsyncWriteExt, Stream};
use serde::{Deserialize, Serialize};
use std::{
    format,
    path::{Path, PathBuf},
};
use tracing::error;

struct PublishInput {
    paths: Vec<AssetRef<'static>>,
}

struct StagedAsset {
    meta_bytes: Box<[u8]>,
    asset_bytes: Box<[u8]>,
}

// XXX TODO: Needs major work.
//
// 1. Consider how to reuse dependency graph and action cache where possible.
// 2. Multithreading.
async fn publish(input: PublishInput, asset_server: &AssetServer, pack_path: &Path) {
    let shared = asset_server.basset_action_source();

    let mut pack = WritablePackFile::default();

    // Assets that have already been published to `pack`.
    //
    // XXX TODO: Consider removing this and checking `pack` directly?
    // Kinda depends on multithread approaches.
    let mut done = HashSet::<RootAssetRef>::new();

    let mut input_stack = input
        .paths
        .iter()
        .map(|p| RootAssetRef::without_label(p.clone()))
        .collect::<Vec<_>>();

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
            RootAssetRef::Path(path) => {
                // XXX TODO: Consider how we can avoid this load - it's only
                // needed for discovering dependencies and getting the meta.
                // XXX TODO: Settings parameter?
                let (loaded, loader) = load_path(asset_server, path, &None)
                    .await
                    .expect("XXX TODO");

                let asset_bytes = {
                    let mut reader = asset_server
                        .get_source(path.source())
                        .expect("XXX TODO")
                        .reader()
                        .read(path.path())
                        .await
                        .expect("XXX TODO");
                    let mut bytes = Vec::new();
                    reader.read_to_end(&mut bytes).await.expect("XXX TODO");
                    bytes.into()
                };

                // XXX TODO: Sort out loader settings? See above where we call `load_path`.
                let meta_bytes = loader.default_meta().serialize().into_boxed_slice();

                // XXX TODO: The action case somewhat duplicates this. Refactor?

                pack.paths.insert(
                    ManifestPath::from(path),
                    StagedAsset {
                        asset_bytes,
                        meta_bytes,
                    },
                );

                input_stack.extend(
                    loaded
                        .dependencies
                        .values()
                        .flatten()
                        .cloned()
                        .map(RootAssetRef::without_label),
                );

                input_stack.extend(
                    loaded
                        .loader_dependencies
                        .keys()
                        .cloned()
                        .map(RootAssetRef::without_label),
                );
            }
            RootAssetRef::Action(action) => {
                // XXX TODO: Can this handle actions that can't be saved?

                // XXX TODO: Can this read directly out of the action cache? We're
                // mostly replicating what that's already done. Maybe the standalone
                // files can be organized in such a way that we can grab `meta_bytes`
                // and `action_bytes` directly. Or maybe there's better options for
                // copying the cache.

                /*
                xxx todo, maybe need to move this into the source? that way we reuse
                the action cache paths

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
                    shared.saver(loaded.asset_type_name()).expect("XXX TODO");

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

                */

                let meta_bytes: Vec<u8> = todo!();
                let asset_bytes: Vec<u8> = todo!();
                let loaded: crate::ErasedLoadedAsset = todo!();

                pack.actions.insert(
                    Box::<str>::from(action.to_string()),
                    StagedAsset {
                        asset_bytes: asset_bytes.into(),
                        meta_bytes: meta_bytes.into(),
                    },
                );

                input_stack.extend(
                    loaded
                        .dependencies
                        .values()
                        .flatten()
                        .cloned()
                        .map(RootAssetRef::without_label),
                );

                // XXX TODO: We're not accounting for the standalone asset having
                // loader dependencies. Not sure if we can work them out unless
                // we do a fake load?
            }
        }
    }

    write_pack_file(pack, pack_path).await;
}

// XXX TODO: Is this duplicating `RootAssetPath`? Maybe justified if we don't
// need `CowArc`.
#[derive(Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
struct ManifestPath {
    source: Option<Box<str>>,
    path: Box<Path>,
}

impl<'a> From<&RootAssetPath<'a>> for ManifestPath {
    fn from(value: &RootAssetPath) -> Self {
        ManifestPath::new(value.source(), value.path())
    }
}

impl ManifestPath {
    fn new(source: &AssetSourceId, path: &Path) -> Self {
        Self {
            source: source.as_str().map(Box::<str>::from),
            path: path.into(),
        }
    }
}

#[derive(Clone, Default, Serialize, Deserialize)]
struct ReadableManifest {
    paths: HashMap<ManifestPath, MetaAndAssetPackLocation>,
    actions: HashMap<Box<str>, MetaAndAssetPackLocation>,
}

impl ReadableManifest {
    fn path(&self, source: &AssetSourceId, path: &Path) -> Option<MetaAndAssetPackLocation> {
        self.paths
            // XXX TODO: Avoid allocations hidden in `ManifestPath::new`.
            .get(&ManifestPath::new(source, path))
            .cloned()
    }

    fn action(&self, action: &str) -> Option<MetaAndAssetPackLocation> {
        self.actions.get(action).cloned()
    }
}

#[derive(Copy, Clone, Default, Serialize, Deserialize)]
struct PackLocation {
    offset: usize,
    length: usize,
}

#[derive(Copy, Clone, Default, Serialize, Deserialize)]
struct MetaAndAssetPackLocation {
    meta: PackLocation,
    asset: PackLocation,
}

struct PublishedAssetReader {
    source: AssetSourceId<'static>,
    pack_file: Arc<ReadablePackFile>,
}

// XXX TODO: Duplicated from crate::io. Refactor? Or try to avoid in the first
// place - it's only used to stub out `PublishedAssetReader::read_directory`.
struct EmptyPathStream;

impl Stream for EmptyPathStream {
    type Item = PathBuf;

    fn poll_next(self: Pin<&mut Self>, _cx: &mut Context<'_>) -> Poll<Option<Self::Item>> {
        Poll::Ready(None)
    }
}

impl AssetReader for PublishedAssetReader {
    async fn read<'a>(&'a self, path: &'a Path) -> Result<impl Reader + 'a, AssetReaderError> {
        self.pack_file.path_asset(&self.source, path)
    }

    async fn read_meta<'a>(&'a self, path: &'a Path) -> Result<impl Reader + 'a, AssetReaderError> {
        self.pack_file.path_meta(&self.source, path)
    }

    async fn read_directory<'a>(
        &'a self,
        _path: &'a Path,
    ) -> Result<Box<PathStream>, AssetReaderError> {
        // XXX TODO: Seems janky?
        error!("Unsupported");
        Ok(Box::new(EmptyPathStream))
    }

    async fn is_directory<'a>(&'a self, _path: &'a Path) -> Result<bool, AssetReaderError> {
        // XXX TODO: Seems janky?
        error!("Unsupported");
        Ok(false)
    }
}

const PACK_MAGIC: &[u8] = b"BEVY_PACK_FILE\n";

// XXX TODO: Is u16 maybe a bit risky? Might want to break into a bigger semver
// style.
const PACK_VERSION: u16 = 1;

struct ReadableStorage(Box<[u8]>);

impl<'a> ReadableStorage {
    fn read(&'a self, location: PackLocation) -> Result<impl Reader + 'a, AssetReaderError> {
        // XXX TODO: Bounds checking.

        Ok(SliceReader::new(
            // XXX TODO: Surprised that there isn't a helper function for this
            // kind of sub-slicing? Maybe didn't spot it.
            &(*self.0)[location.offset..][..location.length],
        ))
    }
}

pub(crate) struct ReadablePackFile {
    manifest: ReadableManifest,
    storage: ReadableStorage,
}

impl<'a> ReadablePackFile {
    pub(crate) fn path_meta(
        &'a self,
        source: &AssetSourceId,
        path: &Path,
    ) -> Result<impl Reader + 'a, AssetReaderError> {
        let location = self
            .manifest
            .path(source, path)
            .ok_or_else(|| AssetReaderError::NotFound(path.into()))?;

        self.storage.read(location.meta)
    }

    // XXX TODO: Very similar to `ReadablePackFile::path_meta`. Refactor?
    pub(crate) fn path_asset(
        &'a self,
        source: &AssetSourceId,
        path: &Path,
    ) -> Result<impl Reader + 'a, AssetReaderError> {
        let location = self
            .manifest
            .path(source, path)
            .ok_or_else(|| AssetReaderError::NotFound(path.into()))?;

        self.storage.read(location.asset)
    }

    // XXX TODO: Very similar to `ReadablePackFile::path_meta`. Refactor?
    pub(crate) fn action_meta(
        &'a self,
        action: &str,
    ) -> Result<impl Reader + 'a, AssetReaderError> {
        let location = self
            .manifest
            .action(action)
            .ok_or_else(|| AssetReaderError::NotFound("XXX TODO".into()))?;

        self.storage.read(location.meta)
    }

    // XXX TODO: Very similar to `ReadablePackFile::path_meta`. Refactor?
    pub(crate) fn action_asset(
        &'a self,
        action: &str,
    ) -> Result<impl Reader + 'a, AssetReaderError> {
        let location = self
            .manifest
            .action(action)
            .ok_or_else(|| AssetReaderError::NotFound("XXX TODO".into()))?;

        self.storage.read(location.asset)
    }
}

async fn read_pack_file(path: &Path) -> ReadablePackFile {
    let mut file = async_fs::OpenOptions::new()
        .read(true)
        .open(&path)
        .await
        .expect("XXX TODO");

    // XXX TODO: Avoid full load. Need `BlobReader` or an alternative to support
    // reading from `Read`.
    let mut bytes = Vec::<u8>::new();
    file.read_to_end(&mut bytes).await.expect("XXX TODO");

    let mut blob = BlobReader::new(&bytes);

    let magic = blob.bytes(PACK_MAGIC.len()).expect("XXX TODO");

    // XXX TODO: Error handling.
    assert_eq!(magic, PACK_MAGIC);

    let version = blob.u16().expect("XXX TODO");

    // XXX TODO: Error handling.
    assert_eq!(version, PACK_VERSION);

    // XXX TODO: Read version.

    let manifest_bytes = blob.bytes_sized().expect("XXX TODO");
    let storage_bytes = blob.bytes_sized().expect("XXX TODO");

    let manifest = ron::de::from_bytes::<ReadableManifest>(manifest_bytes).expect("XXX TODO");

    // XXX TODO: Surely there's one function to convert to boxed slice?
    let storage = storage_bytes.to_vec().into_boxed_slice();

    ReadablePackFile {
        manifest,
        storage: ReadableStorage(storage),
    }
}

#[derive(Default)]
struct WritablePackFile {
    paths: HashMap<ManifestPath, StagedAsset>,
    actions: HashMap<Box<str>, StagedAsset>,
}

async fn write_pack_file(pack: WritablePackFile, path: &Path) {
    // XXX TODO: Consider sorting the paths? Reasoning is that assets in the
    // same folder will likely be accessed together. Not sure if there's anything
    // sensible we can do for actions though.

    // Predict the storage length so that we can allocate it in one chunk.
    //
    // XXX TODO: Could consider writing directly to the file instead of memory?

    // XXX TODO: Avoid implicit panic in `sum`?
    let path_storage_length = pack
        .paths
        .iter()
        .map(|(_, file)| file.meta_bytes.len() + file.asset_bytes.len())
        .sum::<usize>();

    // XXX TODO: Reduce duplication of path version.
    let action_storage_length = pack
        .actions
        .iter()
        .map(|(_, file)| file.meta_bytes.len() + file.asset_bytes.len())
        .sum::<usize>();

    let storage_length = path_storage_length + action_storage_length;

    let mut storage_bytes = Vec::<u8>::with_capacity(storage_length);
    let mut manifest = ReadableManifest::default();

    // XXX TODO: More efficient if we drain?
    for (path, file) in pack.paths {
        let meta_location = PackLocation {
            offset: storage_bytes.len(),
            length: file.meta_bytes.len(),
        };

        // XXX TODO: Review if `extend_from_slice` is best.
        storage_bytes.extend_from_slice(&file.meta_bytes);

        let asset_location = PackLocation {
            offset: storage_bytes.len(),
            length: file.asset_bytes.len(),
        };

        // XXX TODO: Review if `extend_from_slice` is best.
        storage_bytes.extend_from_slice(&file.asset_bytes);

        manifest.paths.insert(
            path,
            MetaAndAssetPackLocation {
                meta: meta_location,
                asset: asset_location,
            },
        );
    }

    // XXX TODO: Reduce duplication of path case?
    for (action, file) in pack.actions {
        let meta_location = PackLocation {
            offset: storage_bytes.len(),
            length: file.meta_bytes.len(),
        };

        // XXX TODO: Review if `extend_from_slice` is best.
        storage_bytes.extend_from_slice(&file.meta_bytes);

        let asset_location = PackLocation {
            offset: storage_bytes.len(),
            length: file.asset_bytes.len(),
        };

        // XXX TODO: Review if `extend_from_slice` is best.
        storage_bytes.extend_from_slice(&file.asset_bytes);

        manifest.actions.insert(
            action,
            MetaAndAssetPackLocation {
                meta: meta_location,
                asset: asset_location,
            },
        );
    }

    assert_eq!(storage_length, storage_bytes.len());

    let manifest_bytes = ron::ser::to_string(&manifest)
        .expect("XXX TODO")
        .into_bytes()
        .into_boxed_slice();

    // XXX TODO: Eventually we should write direct to the file.
    let mut file_bytes = Vec::<u8>::new();

    let mut blob = BlobWriter::new(&mut file_bytes);
    {
        blob.bytes(PACK_MAGIC);
        blob.u16(PACK_VERSION);
        blob.bytes_sized(&manifest_bytes);
        blob.bytes_sized(&storage_bytes);
    }

    // XXX TODO: This pattern of creating the folder and using a temp file is
    // duplicated in `FileCache::put`. Refactor?
    async_fs::create_dir_all(path.parent().expect("XXX TODO"))
        .await
        .expect("TODO");

    let temp_path = path.with_extension("tmp");

    let mut temp_file = async_fs::OpenOptions::new()
        .write(true)
        .create_new(true)
        .open(&path)
        .await
        .expect("XXX TODO");

    temp_file.write_all(&file_bytes).await.expect("XXX TODO");

    async_fs::rename(temp_path, path).await.expect("XXX TODO");
}

async fn published_asset_source(source: AssetSourceId<'static>, path: &Path) -> AssetSourceBuilder {
    let pack_file = Arc::new(read_pack_file(path).await);

    AssetSourceBuilder::new(move || {
        Box::new(PublishedAssetReader {
            source: source.clone(),
            pack_file: pack_file.clone(),
        })
    })
}
