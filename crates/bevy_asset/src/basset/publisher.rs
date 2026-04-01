#![expect(unused, reason = "XXX TODO")]

use crate::{
    basset::{
        blob::{BlobReader, BlobWriter},
        load_action, load_path,
        standalone::write_standalone_asset,
        BassetShared, RootAssetAction2, RootAssetPath, RootAssetRef,
    },
    io::{AssetReader, AssetReaderError, AssetSourceBuilder, PathStream, Reader, SliceReader},
    AssetPath, AssetRef, AssetServer,
};
use alloc::{boxed::Box, string::ToString, sync::Arc, vec::Vec};
// XXX TODO: Try and replace `async_fs` with `AssetSource`.
use async_fs::File;
use bevy_platform::collections::HashMap;
use core::{
    pin::Pin,
    task::{Context, Poll},
};
use futures_lite::Stream;
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

fn published_path(path: &RootAssetRef) -> Box<Path> {
    Box::<Path>::from(PathBuf::from(match path {
        // XXX TODO: Verify that `to_string` is what we want.
        RootAssetRef::Path(path) => format!("path:{}", path),
        RootAssetRef::Action(action) => format!("action:{}", action),
    }))
}

async fn publish(input: PublishInput, asset_server: &AssetServer) {
    let shared = asset_server.basset_shared();

    let mut staged_assets = HashMap::<Box<Path>, StagedAsset>::new();

    let mut input_stack = input
        .paths
        .iter()
        .map(|p| RootAssetRef::without_label(p.clone()))
        .collect::<Vec<_>>();

    while let Some(input_asset) = input_stack.pop() {
        // XXX TODO: Settings parameter?
        let dependency_key = shared.dependency_key(&input_asset, None).await;

        match &input_asset {
            RootAssetRef::Path(path) => {
                // XXX TODO: Settings?
                let loaded = load_path(asset_server, path, &None)
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

                let meta_bytes = todo!();

                // XXX TODO: The action path somewhat duplicates this. Refactor?

                staged_assets.insert(
                    published_path(&input_asset),
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
                let loaded = load_action(asset_server, action).await.expect("XXX TODO");

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

                staged_assets.insert(
                    published_path(&input_asset),
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
        /*
        match AssetRef<if action
            try get dependencies and action key from cache
            else load

            push dependencies to stack if not already in staged_assets
            add to staged
        if regular
            try to get dependencies from cache
            else load
            add to staged
        */
    }

    /*
    if compression enabled then compress files now
        TODO: Maybe bake this into standalone files? So they can individually
        enable compression? although that means action cache has to use the
        same compression settings if we want to simply copy

        TODO: Do this while staging?
    }
    */
}

#[derive(Clone, Default, Serialize, Deserialize)]
struct Manifest(HashMap<Box<Path>, ManifestLocation>);

// XXX TODO: Could be cheeky and use slices? But that won't fly if we do move
// to reading from the file.
#[derive(Copy, Clone, Default, Serialize, Deserialize)]
struct ManifestLocation {
    meta_offset: usize,
    meta_length: usize,
    asset_offset: usize,
    asset_length: usize,
}

struct PublishedAssetReader {
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
        if let Some(location) = self.pack_file.manifest.0.get(path) {
            Ok(SliceReader::new(
                // XXX TODO: Surely there's a utility for this somewhere?
                &(*self.pack_file.storage)[location.asset_offset..][..location.asset_length],
            ))
        } else {
            Err(AssetReaderError::NotFound(path.into()))
        }
    }

    async fn read_meta<'a>(&'a self, path: &'a Path) -> Result<impl Reader + 'a, AssetReaderError> {
        // XXX TODO: Somewhat duplicates `read`. Refactor?
        if let Some(location) = self.pack_file.manifest.0.get(path) {
            Ok(SliceReader::new(
                &self.pack_file.storage[location.meta_offset..][..location.meta_length],
            ))
        } else {
            Err(AssetReaderError::NotFound(path.into()))
        }
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

struct ReadablePackFile {
    manifest: Manifest,
    storage: Box<[u8]>,
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

    let manifest = ron::de::from_bytes::<Manifest>(manifest_bytes).expect("XXX TODO");

    // XXX TODO: Surely there's one function to convert to boxed slice?
    let storage = storage_bytes.to_vec().into_boxed_slice();

    ReadablePackFile { manifest, storage }
}

struct WritablePackFile {
    files: HashMap<Box<Path>, StagedAsset>,
}

fn write_pack_file(pack: WritablePackFile) -> Box<[u8]> {
    let files = pack.files.into_iter().collect::<Vec<_>>();

    let manifest = Manifest(HashMap::from_iter(files.iter().scan(
        0usize,
        |cursor, (path, asset)| {
            let meta_offset = *cursor;
            let meta_length = asset.meta_bytes.len();
            *cursor += meta_length;

            let asset_offset = *cursor;
            let asset_length = asset.asset_bytes.len();
            *cursor += asset_length;

            Some((
                path.clone(),
                ManifestLocation {
                    meta_offset,
                    meta_length,
                    asset_offset,
                    asset_length,
                },
            ))
        },
    )));

    let manifest_bytes = ron::ser::to_string(&manifest)
        .expect("XXX TODO")
        .into_bytes()
        .into_boxed_slice();

    let mut storage_bytes = Vec::<u8>::new();

    for (path, file) in files {
        // XXX TODO: For debugging only?
        let manifest_location = manifest.0[&path];

        assert_eq!(storage_bytes.len(), manifest_location.meta_offset);
        assert_eq!(file.meta_bytes.len(), manifest_location.meta_length);

        // XXX TODO: Review if `extend_from_slice` is best.
        storage_bytes.extend_from_slice(&file.meta_bytes);

        assert_eq!(storage_bytes.len(), manifest_location.asset_offset);
        assert_eq!(file.meta_bytes.len(), manifest_location.asset_length);

        // XXX TODO: Review if `extend_from_slice` is best.
        storage_bytes.extend_from_slice(&file.asset_bytes);
    }

    let mut file_bytes = Vec::<u8>::new();

    let mut blob = BlobWriter::new(&mut file_bytes);
    {
        blob.bytes(PACK_MAGIC);
        blob.u16(PACK_VERSION);
        blob.bytes_sized(&manifest_bytes);
        blob.bytes_sized(&storage_bytes);
    }

    file_bytes.into()
}

async fn published_asset_source(path: &Path) -> AssetSourceBuilder {
    let pack_file = Arc::new(read_pack_file(path).await);

    AssetSourceBuilder::new(move || {
        Box::new(PublishedAssetReader {
            pack_file: pack_file.clone(),
        })
    })
}
