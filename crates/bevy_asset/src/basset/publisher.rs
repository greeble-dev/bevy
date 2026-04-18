#![expect(unused, reason = "XXX TODO")]

use crate::{
    basset::{
        blob::{BlobReader, BlobWriter},
        standalone::write_standalone_asset,
        RootAssetPath, RootAssetRef,
    },
    io::{
        AssetReader, AssetReaderError, AssetSourceBuilder, AssetSourceId, PathStream, Reader,
        SliceReader,
    },
    AssetPath, AssetRef, AssetServer, LoaderDependency,
};
use alloc::{boxed::Box, string::ToString, sync::Arc, vec::Vec};
// XXX TODO: Try and replace `async_fs` with `AssetSource`?
use async_fs::File;
use atomicow::CowArc;
use bevy_ecs::error::BevyError;
use bevy_platform::collections::{HashMap, HashSet};
use core::{
    fmt::Debug,
    pin::Pin,
    task::{Context, Poll},
};
use futures_lite::{AsyncReadExt, AsyncWriteExt, Stream};
use serde::{Deserialize, Serialize};
use std::{
    format,
    path::{Path, PathBuf},
};
use tracing::{debug, error};

// XXX TODO: Document. Currently duplicates `LoaderDependency` - should pick one?
// XXX TODO: Decide if we use `RootAssetRef` or `AssetRef`. The labels don't matter
// for publishing, but it might be convenient for the user or help debugging?
#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub enum PublishDependency {
    Load(RootAssetRef),
    File(RootAssetPath<'static>),
}

impl From<LoaderDependency> for PublishDependency {
    fn from(value: LoaderDependency) -> Self {
        match value {
            LoaderDependency::Load(path) => PublishDependency::Load(path),
            LoaderDependency::File(path) => PublishDependency::File(path),
        }
    }
}

#[derive(Debug, Clone)]
pub struct PublishInput {
    pub paths: Vec<PublishDependency>,
}

pub(crate) struct StagedAsset {
    pub(crate) meta_bytes: Option<Box<[u8]>>,
    pub(crate) asset_bytes: Box<[u8]>,
}

#[derive(Clone, Default, Serialize, Deserialize)]
pub(crate) struct ReadableManifest {
    pub(crate) paths: HashMap<RootAssetPath<'static>, AssetPackLocation>,
    pub(crate) actions: HashMap<Box<str>, AssetPackLocation>,
}

impl ReadableManifest {
    pub(crate) fn path(&self, source: &AssetSourceId, path: &Path) -> Option<AssetPackLocation> {
        self.paths
            // XXX TODO: Can we look this up without cloning the source?
            .get(&RootAssetPath::new(source.clone(), CowArc::Borrowed(path)))
            .cloned()
    }

    pub(crate) fn action(&self, action: &str) -> Option<AssetPackLocation> {
        self.actions.get(action).cloned()
    }
}

#[derive(Copy, Clone, Default, Serialize, Deserialize)]
pub(crate) struct PackLocation {
    pub(crate) offset: usize,
    pub(crate) length: usize,
}

#[derive(Copy, Clone, Default, Serialize, Deserialize)]
pub(crate) struct AssetPackLocation {
    pub(crate) meta: Option<PackLocation>,
    pub(crate) asset: PackLocation,
}

struct PublishedAssetReader {
    pub(crate) source: AssetSourceId<'static>,
    pub(crate) pack_file: Arc<ReadablePackFile>,
}

// XXX TODO: Duplicated from `bevy_asset::io::EmptyPathStream`. Refactor? Or try to avoid in the first
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
    fn read(&'a self, location: PackLocation) -> Result<SliceReader<'a>, AssetReaderError> {
        // XXX TODO: Bounds checking.

        Ok(SliceReader::new(
            // XXX TODO: Surprised that there isn't a helper function for this
            // kind of sub-slicing? Maybe didn't spot it.
            &(*self.0)[location.offset..][..location.length],
        ))
    }
}

// XXX TODO: Review `pub(crate)`.
pub struct ReadablePackFile {
    pub(crate) manifest: ReadableManifest,
    pub(crate) storage: ReadableStorage,
}

pub(crate) struct MetaAndAssetReader<'a> {
    pub(crate) meta: Option<SliceReader<'a>>,
    pub(crate) asset: SliceReader<'a>,
}

impl<'a> ReadablePackFile {
    pub(crate) fn path_meta(
        &'a self,
        source: &AssetSourceId,
        path: &Path,
    ) -> Result<SliceReader<'a>, AssetReaderError> {
        debug!("Read file meta: {path:?}");

        let location = self
            .manifest
            .path(source, path)
            // XXX TODO: Would be clearer if error mentioned pack files. Maybe
            // need to catch and add more content higher up the chain?
            .ok_or_else(|| AssetReaderError::NotFound(path.into()))?;

        if let Some(location) = location.meta {
            self.storage.read(location)
        } else {
            Err(AssetReaderError::NotFound(path.into()))
        }
    }

    // XXX TODO: Very similar to `ReadablePackFile::path_meta`. Refactor?
    pub(crate) fn path_asset(
        &'a self,
        source: &AssetSourceId,
        path: &Path,
    ) -> Result<SliceReader<'a>, AssetReaderError> {
        debug!("Read file asset: {path:?}");

        let location = self
            .manifest
            .path(source, path)
            .ok_or_else(|| AssetReaderError::NotFound(path.into()))?;

        self.storage.read(location.asset)
    }

    pub(crate) fn action(
        &'a self,
        action: &str,
    ) -> Result<MetaAndAssetReader<'a>, AssetReaderError> {
        debug!("Read action: {action:?}");

        let location = self
            .manifest
            .action(action)
            .ok_or_else(|| AssetReaderError::NotFound(action.into()))?;

        let meta = if let Some(meta_location) = location.meta {
            Some(self.storage.read(meta_location)?)
        } else {
            None
        };

        let asset = self.storage.read(location.asset)?;

        Ok(MetaAndAssetReader { meta, asset })
    }
}

pub async fn read_pack_file(path: &Path) -> ReadablePackFile {
    let mut file = async_fs::OpenOptions::new()
        .read(true)
        .open(&path)
        .await
        .expect("XXX TODO");

    // XXX TODO: Avoid full load? Need `BlobReader` or an alternative to support
    // reading from `Read`.
    let mut bytes = Vec::<u8>::new();
    AsyncReadExt::read_to_end(&mut file, &mut bytes)
        .await
        .expect("XXX TODO");

    let mut blob = BlobReader::new(&bytes);

    let magic = blob.bytes(PACK_MAGIC.len()).expect("XXX TODO");

    // XXX TODO: Error handling.
    assert_eq!(magic, PACK_MAGIC);

    let version = blob.u16().expect("XXX TODO");

    // XXX TODO: Error handling.
    assert_eq!(version, PACK_VERSION);

    let manifest_bytes = blob.bytes_sized().expect("XXX TODO");
    let storage_bytes = blob.bytes_sized().expect("XXX TODO");

    let manifest = ron::de::from_bytes::<ReadableManifest>(manifest_bytes).expect("XXX TODO");

    // XXX TODO: Surely there's one function that can convert slice to boxed slice?
    let storage = ReadableStorage(storage_bytes.to_vec().into_boxed_slice());

    ReadablePackFile { manifest, storage }
}

#[derive(Default)]
pub(crate) struct WritablePackFile {
    pub(crate) paths: HashMap<RootAssetPath<'static>, StagedAsset>,
    pub(crate) actions: HashMap<Box<str>, StagedAsset>,
}

struct StorageBuilder {
    files: Vec<StagedAsset>,
    length: usize,
}

impl StorageBuilder {
    fn new() -> Self {
        StorageBuilder {
            files: Vec::new(),
            length: 0,
        }
    }

    fn add(&mut self, asset: StagedAsset) -> AssetPackLocation {
        let meta_location = if let Some(meta_bytes) = &asset.meta_bytes {
            let meta_offset = self.length;
            self.length += meta_bytes.len();

            Some(PackLocation {
                offset: meta_offset,
                length: meta_bytes.len(),
            })
        } else {
            None
        };

        let asset_offset = self.length;
        self.length += asset.asset_bytes.len();

        let asset_location = PackLocation {
            offset: asset_offset,
            length: asset.asset_bytes.len(),
        };

        self.files.push(asset);

        AssetPackLocation {
            meta: meta_location,
            asset: asset_location,
        }
    }

    fn finish(self) -> Box<[u8]> {
        // XXX TODO: Could maybe add some verification here if we keep a copy
        // of the `AssetPackLocation` returned by `add`? Not sure if worth it.

        let mut result = Vec::with_capacity(self.length);

        for file in self.files {
            if let Some(meta_bytes) = &file.meta_bytes {
                result.extend_from_slice(meta_bytes);
            }
            result.extend_from_slice(&file.asset_bytes);
        }

        assert_eq!(result.len(), self.length);

        result.into()
    }
}

pub(crate) async fn write_pack_file(pack: WritablePackFile, path: &Path) {
    // XXX TODO: Consider sorting the paths? Reasoning is that assets in the
    // same folder will likely be accessed together. Not sure if there's anything
    // sensible we can do for actions though.

    let mut storage_builder = StorageBuilder::new();
    let mut manifest = ReadableManifest::default();

    for (path, file) in pack.paths {
        debug!("Writing path \"{path}\"");
        manifest.paths.insert(path, storage_builder.add(file));
    }

    for (action, file) in pack.actions {
        debug!("Writing action \"{action}\"");
        manifest.actions.insert(action, storage_builder.add(file));
    }

    let storage = storage_builder.finish();

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
        blob.bytes_sized(&storage);
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
        .open(&temp_path)
        .await
        .expect("XXX TODO");

    temp_file.write_all(&file_bytes).await.expect("XXX TODO");

    // XXX TODO: Is it necessary to make sure writes are synced before renaming?
    // Seems unlikely.
    temp_file.sync_all().await.expect("XXX TODO");

    drop(temp_file);

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
