#![expect(unused, reason = "XXX TODO")]

use crate::{
    basset::{
        load_action, load_path, standalone::write_standalone_asset, BassetShared, RootAssetAction2,
        RootAssetPath, RootAssetRef,
    },
    io::{AssetReader, AssetReaderError, AssetSourceBuilder, PathStream, Reader, SliceReader},
    AssetPath, AssetRef, AssetServer,
};
use alloc::{boxed::Box, string::ToString, sync::Arc, vec::Vec};
use bevy_platform::collections::HashMap;
use core::{
    pin::Pin,
    task::{Context, Poll},
};
use futures_lite::Stream;
use std::{
    format,
    path::{Path, PathBuf},
};
use tracing::error;

struct InputManifest {
    paths: Vec<AssetRef<'static>>,
}

struct StagedAsset {
    bytes: Box<[u8]>,
    meta_bytes: Box<[u8]>,
}

fn published_path(path: &RootAssetRef) -> Box<Path> {
    Box::<Path>::from(PathBuf::from(match path {
        // XXX TODO: Verify that `to_string` is what we want.
        RootAssetRef::Path(path) => format!("path:{}", path),
        RootAssetRef::Action(action) => format!("action:{}", action),
    }))
}

async fn publish(input_manifest: InputManifest, asset_server: &AssetServer) {
    let shared = asset_server.basset_shared();

    let mut staged_assets = HashMap::<Box<Path>, StagedAsset>::new();

    let mut input_stack = input_manifest
        .paths
        .iter()
        .map(|p| RootAssetRef::without_label(p.clone()))
        .collect::<Vec<_>>();

    while let Some(input_asset) = input_stack.pop() {
        // XXX TODO: Settings parameter?
        let dependency_key = shared
            .dependency_key(&input_asset, None, asset_server)
            .await;

        match &input_asset {
            RootAssetRef::Action(action) => {
                let loaded = load_action(asset_server, action).await.expect("XXX TODO");

                // XXX TODO: Duplicates where `load_action` writes to the cache.
                let (saver, settings) = shared.saver(loaded.asset_type_name()).expect("XXX TODO");

                let loader = asset_server
                    .get_asset_loader_with_type_name(saver.loader_type_name())
                    .await
                    .expect("XXX TODO");

                /*
                let bytes = write_standalone_asset(&loaded, &*loader, saver, settings)
                    .await
                    .expect("XXX TODO");
                */
                let bytes = todo!();
                let meta_bytes = todo!();

                staged_assets.insert(
                    published_path(&input_asset),
                    StagedAsset { bytes, meta_bytes },
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

            RootAssetRef::Path(path) => {
                // XXX TODO: Settings?
                let loaded = load_path(asset_server, path, &None)
                    .await
                    .expect("XXX TODO");

                let bytes = {
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
                    StagedAsset { bytes, meta_bytes },
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

// XXX TODO: Could be cheeky and use slices? But that won't fly if we do move
// to reading from the file.
struct ManifestLocation {
    asset_offset: usize,
    asset_length: usize,
    meta_offset: usize,
    meta_length: usize,
}

struct PublishedAssetReader {
    bytes: Arc<Box<[u8]>>,
    manifest: HashMap<Box<Path>, ManifestLocation>,
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
        if let Some(location) = self.manifest.get(path) {
            Ok(SliceReader::new(
                &self.bytes[location.asset_offset..(location.asset_offset + location.asset_length)],
            ))
        } else {
            Err(AssetReaderError::NotFound(path.into()))
        }
    }

    async fn read_meta<'a>(&'a self, path: &'a Path) -> Result<impl Reader + 'a, AssetReaderError> {
        // XXX TODO: Somewhat duplicates `read`. Refactor?
        if let Some(location) = self.manifest.get(path) {
            Ok(SliceReader::new(
                &self.bytes[location.meta_offset..(location.meta_offset + location.meta_length)],
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

fn published_asset_source() -> AssetSourceBuilder {
    let bytes: Arc<Box<[u8]>> = todo!();

    // read from file, extract manifest, push subslice to reader.

    //AssetSourceBuilder::new(move ||

    todo!()
}
