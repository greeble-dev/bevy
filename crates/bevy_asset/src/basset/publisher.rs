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

#[derive(Debug)]
pub struct PublishInput {
    pub paths: Vec<AssetRef<'static>>,
}

pub(crate) struct StagedAsset {
    pub(crate) meta_bytes: Box<[u8]>,
    pub(crate) asset_bytes: Box<[u8]>,
}

// XXX TODO: Is this duplicating `RootAssetPath`? Maybe justified if we don't
// need `CowArc`.
#[derive(Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub(crate) struct ManifestPath {
    pub(crate) source: Option<Box<str>>,
    pub(crate) path: Box<Path>,
}

impl<'a> From<&RootAssetPath<'a>> for ManifestPath {
    fn from(value: &RootAssetPath) -> Self {
        ManifestPath::new(value.source(), value.path())
    }
}

impl ManifestPath {
    pub(crate) fn new(source: &AssetSourceId, path: &Path) -> Self {
        Self {
            source: source.as_str().map(Box::<str>::from),
            path: path.into(),
        }
    }
}

#[derive(Clone, Default, Serialize, Deserialize)]
pub(crate) struct ReadableManifest {
    pub(crate) paths: HashMap<ManifestPath, MetaAndAssetPackLocation>,
    pub(crate) actions: HashMap<Box<str>, MetaAndAssetPackLocation>,
}

impl ReadableManifest {
    pub(crate) fn path(
        &self,
        source: &AssetSourceId,
        path: &Path,
    ) -> Option<MetaAndAssetPackLocation> {
        self.paths
            // XXX TODO: Avoid allocations hidden in `ManifestPath::new`.
            .get(&ManifestPath::new(source, path))
            .cloned()
    }

    pub(crate) fn action(&self, action: &str) -> Option<MetaAndAssetPackLocation> {
        self.actions.get(action).cloned()
    }
}

#[derive(Copy, Clone, Default, Serialize, Deserialize)]
pub(crate) struct PackLocation {
    pub(crate) offset: usize,
    pub(crate) length: usize,
}

#[derive(Copy, Clone, Default, Serialize, Deserialize)]
pub(crate) struct MetaAndAssetPackLocation {
    pub(crate) meta: PackLocation,
    pub(crate) asset: PackLocation,
}

struct PublishedAssetReader {
    pub(crate) source: AssetSourceId<'static>,
    pub(crate) pack_file: Arc<ReadablePackFile>,
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

pub(crate) struct ReadableStorage(pub(crate) Box<[u8]>);

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

// XXX TODO: Review `pub`.
pub struct ReadablePackFile {
    pub(crate) manifest: ReadableManifest,
    pub(crate) storage: ReadableStorage,
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

pub async fn read_pack_file(path: &Path) -> ReadablePackFile {
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
pub(crate) struct WritablePackFile {
    pub(crate) paths: HashMap<ManifestPath, StagedAsset>,
    pub(crate) actions: HashMap<Box<str>, StagedAsset>,
}

pub(crate) async fn write_pack_file(pack: WritablePackFile, path: &Path) {
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

    {
        let mut blob = BlobWriter::new(&mut file_bytes);
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
    {
        let mut temp_file = async_fs::OpenOptions::new()
            .write(true)
            .create_new(true)
            .open(&temp_path)
            .await
            .expect("XXX TODO");

        temp_file.write_all(&file_bytes).await.expect("XXX TODO");

        // XXX TODO: Is this necessary?
        temp_file.sync_all().await.expect("XXX TODO");
    }

    async_fs::rename(temp_path, path).await.expect("XXX TODO");
}

pub fn published_asset_source(
    source: AssetSourceId<'static>,
    pack_file: Arc<ReadablePackFile>,
) -> AssetSourceBuilder {
    AssetSourceBuilder::new(move || {
        Box::new(PublishedAssetReader {
            source: source.clone(),
            pack_file: pack_file.clone(),
        })
    })
}
