//! XXX TODO

use crate::{
    basset::{RootAssetPath, RootAssetRef},
    io::AssetSources,
    AsyncWriteExt, LoaderDependency,
};
use alloc::{boxed::Box, string::String, string::ToString, sync::Arc, vec::Vec};
// XXX TODO: Try and replace `async_fs` with `AssetSource`.
use async_fs::File;
use bevy_ecs::error::BevyError;
use bevy_platform::{
    collections::HashMap,
    sync::{PoisonError, RwLock},
};
use bevy_tasks::{ConditionalSendFuture, IoTaskPool};
use core::{
    fmt::{Debug, Display},
    result::Result,
};
use core::{hash::Hash, marker::PhantomData};
use serde::{Deserialize, Serialize};
use std::{format, path::PathBuf};
use tracing::debug;

// XXX TODO: The RON serialization of `BassetHash` is verbose. Maybe do a custom
// serialize that uses hex?
#[derive(Hash, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Serialize, Deserialize)]
pub struct BassetHash([u8; 32]);

impl BassetHash {
    // XXX TODO: Should take bytes by value or ref?
    pub(crate) fn new(bytes: [u8; 32]) -> Self {
        Self(bytes)
    }

    // XXX TODO: Naming convention suggests `as_bytes` should return a reference?
    // So this should be `to_bytes`?
    pub(crate) fn as_bytes(&self) -> [u8; 32] {
        self.0
    }
}

impl Debug for BassetHash {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        // XXX TODO: Review. Duplicated elsewhere. Could be optimized. Avoid `std`?
        let hex = String::from_iter(self.0.iter().map(|b| std::format!("{:x}", b)));

        Debug::fmt(&hex, f)
    }
}

impl Display for BassetHash {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        // Display only the first 8 characters.
        //
        // XXX TODO: Review. Duplicated elsewhere. Could be optimized. Avoid `std`?
        let hex = String::from_iter(self.0.iter().take(4).map(|b| std::format!("{:x}", b)));

        Display::fmt(&hex, f)
    }
}

pub(crate) trait CacheKey: Copy + Clone + Eq + Hash + Debug + Send + Sync {
    fn as_hash(&self) -> BassetHash;
}

pub(crate) trait MemoryCacheValue: Send + Sync + Clone + Eq + Debug {}

impl<T: Send + Sync + Clone + Eq + Debug> MemoryCacheValue for T {}

struct MemoryCache<K: CacheKey, V: MemoryCacheValue> {
    name: &'static str,
    key_to_value: HashMap<K, V>,
    validate: bool,
}

// XXX TODO: Where should this go?
pub(crate) fn should_log(path: &RootAssetRef<'static>) -> bool {
    // Skip embedded sources for now as they're too spammy.
    // XXX TODO: Need more robust way of ignoring these.
    !path.to_string().contains("embedded://")
}

fn log(name: &'static str, path: &RootAssetRef<'static>, key: BassetHash, string: &'static str) {
    if should_log(path) {
        debug!(%key, %path, "{name}: {string}");
    }
}

impl<K: CacheKey, V: MemoryCacheValue> MemoryCache<K, V> {
    fn new(name: &'static str, validate: bool) -> Self {
        Self {
            name,
            key_to_value: Default::default(),
            validate,
        }
    }

    // XXX TODO: `asset_path` is only for debugging. Maybe make it more opaque?
    fn get(&self, key: &K, asset_path: &RootAssetRef<'static>) -> Option<V> {
        if self.validate {
            return None;
        }

        let result = self.key_to_value.get(key).cloned();

        if result.is_some() {
            log(self.name, asset_path, key.as_hash(), "Memory cache hit.");
        } else {
            log(self.name, asset_path, key.as_hash(), "Memory cache miss.");
        }

        result
    }

    // XXX TODO: `asset_path` is only for debugging. Maybe make it more opaque?
    fn put(&mut self, key: K, value: V, asset_path: &RootAssetRef<'static>) {
        log(self.name, asset_path, key.as_hash(), "Memory cache put.");

        if self.validate
            && let Some(existing) = self.key_to_value.get(&key)
        {
            assert_eq!(&value, existing, "{key:?}");
        }

        self.key_to_value.insert(key, value);
    }
}

// XXX TODO: Why do we need this `static` bound to make the async happy?
pub(crate) trait FileCacheValue: Send + Sync + Clone + Eq + Debug + 'static {
    fn write(&self, file: &mut File) -> impl ConditionalSendFuture<Output = Result<(), BevyError>>;

    fn read(bytes: Box<[u8]>) -> Self;
}

impl FileCacheValue for Arc<[u8]> {
    async fn write(&self, file: &mut File) -> Result<(), BevyError> {
        file.write_all(self).await.map_err(BevyError::from)
    }

    fn read(bytes: Box<[u8]>) -> Self {
        bytes.into()
    }
}

#[derive(Default)]
pub(crate) struct FileCache<K: CacheKey, V: FileCacheValue> {
    name: &'static str,
    base_path: PathBuf,
    validate: bool,
    phantom: PhantomData<(K, V)>,
}

impl<K: CacheKey, V: FileCacheValue> FileCache<K, V> {
    pub(crate) fn new(name: &'static str, base_path: PathBuf, validate: bool) -> Self {
        Self {
            name,
            base_path,
            validate,
            phantom: PhantomData::<(K, V)>,
        }
    }

    pub(crate) fn value_path(&self, key: &K) -> PathBuf {
        // XXX TODO: Duplicates `BassetHash` debug?
        // XXX TODO: Check if this is worth optimizing.
        let hex = String::from_iter(key.as_hash().0.iter().map(|b| format!("{:x}", b)));

        // Spread files across multiple folders by using the first few digits of
        // the hash. This is a hedge against filesystems that don't like
        // thousands of files in a single folder.
        //
        // XXX TODO: Could be refined? Review the probabilities.
        // XXX TODO: Can be optimized if the file separator is a single character?
        let relative_path = [&hex[0..2], &hex[2..4], &hex[..]]
            .iter()
            .collect::<PathBuf>();

        self.base_path.join(relative_path)
    }

    // XXX TODO: `asset_path` is only for debugging. Maybe make it more opaque?
    pub(crate) async fn get(&self, key: &K, asset_path: &RootAssetRef<'static>) -> Option<V> {
        if self.validate {
            return None;
        }

        match self.unvalidated_get(key).await {
            Some(value) => {
                log(self.name, asset_path, key.as_hash(), "File cache hit.");
                Some(value)
            }
            None => {
                log(self.name, asset_path, key.as_hash(), "File cache miss.");
                None
            }
        }
    }

    // XXX TODO: `asset_path` is only for debugging. Maybe make it more opaque?
    pub(crate) async fn unvalidated_get(&self, key: &K) -> Option<V> {
        let value_path = self.value_path(key);

        if let Ok(value) = async_fs::read(value_path).await {
            Some(<V as FileCacheValue>::read(value.into()))
        } else {
            None
        }
    }

    // XXX TODO: `asset_path` is only for debugging. Maybe make it more opaque?
    pub(crate) fn put(&self, key: K, value: V, asset_path: &RootAssetRef<'static>) {
        let value_path = self.value_path(&key);

        // XXX TODO: Review faff that avoids lifetime issues.
        let value = value.clone();

        log(self.name, asset_path, key.as_hash(), "File cache put.");

        // XXX TODO: Restore validation, but without needing this function to
        // be async.
        //
        //if self.validate
        //    && let Some(existing) = self.unvalidated_get(&key).await
        //{
        //    assert_eq!(&value, &existing, "{key:?}");
        //}

        IoTaskPool::get()
            .spawn(async move {
                // XXX TODO: Early out if file already exists?

                async_fs::create_dir_all(value_path.parent().expect("XXX TODO"))
                    .await
                    .expect("XXX TODO");

                let temp_path = value_path.with_extension("tmp");

                let Ok(mut temp_file) = async_fs::OpenOptions::new()
                    .write(true)
                    .create_new(true)
                    .open(&temp_path)
                    .await
                else {
                    // TODO: Temp files could be left behind if interrupted
                    // during write or rename. Should we try to clean them up?
                    return;
                };

                <V as FileCacheValue>::write(&value, &mut temp_file)
                    .await
                    .expect("XXX TODO");

                // XXX TODO: Is this necessary?
                temp_file.sync_all().await.expect("XXX TODO");
                drop(temp_file);

                async_fs::rename(temp_path, value_path)
                    .await
                    .expect("XXX TODO");
            })
            .detach();
    }
}

pub(crate) struct MemoryAndFileCache<K: CacheKey, V: FileCacheValue + MemoryCacheValue> {
    memory: Arc<RwLock<MemoryCache<K, V>>>,
    file: Option<FileCache<K, V>>,
}

impl<K: CacheKey, V: FileCacheValue> MemoryAndFileCache<K, V> {
    pub(crate) fn new(
        name: &'static str,
        file_cache_path: Option<PathBuf>,
        validate: bool,
    ) -> Self {
        MemoryAndFileCache {
            memory: Arc::new(RwLock::new(MemoryCache::new(name, validate))),
            file: file_cache_path.map(|p| FileCache::new(name, p, validate)),
        }
    }

    // XXX TODO: `asset_path` is only for debugging. Maybe make it more opaque?
    pub(crate) async fn get(&self, key: &K, asset_path: &RootAssetRef<'static>) -> Option<V> {
        if let Some(from_memory) = self
            .memory
            .read()
            .unwrap_or_else(PoisonError::into_inner)
            .get(key, asset_path)
        {
            return Some(from_memory);
        }

        if let Some(file) = &self.file {
            let from_file = file.get(key, asset_path).await?;

            self.memory
                .write()
                .unwrap_or_else(PoisonError::into_inner)
                .put(*key, from_file.clone(), asset_path);

            Some(from_file)
        } else {
            None
        }
    }

    // XXX TODO: `asset_path` is only for debugging. Maybe make it more opaque?
    pub(crate) fn put(&self, key: K, value: V, asset_path: &RootAssetRef<'static>) {
        // XXX TODO: Avoid `blob.clone()` if there's no file cache?
        self.memory
            .write()
            .unwrap_or_else(PoisonError::into_inner)
            .put(key, value.clone(), asset_path);

        if let Some(file) = &self.file {
            file.put(key, value, asset_path);
        }
    }
}

#[derive(Copy, Clone, Hash, Eq, PartialEq)]
pub(crate) struct ContentHash(BassetHash);

pub(crate) struct ContentCache {
    path_to_hash: RwLock<HashMap<RootAssetPath<'static>, ContentHash>>,
    sources: Arc<AssetSources>,
}

impl ContentHash {
    pub(crate) fn as_bytes(&self) -> [u8; 32] {
        self.0.as_bytes()
    }
}

impl ContentCache {
    pub(crate) fn new(sources: Arc<AssetSources>) -> Self {
        Self {
            path_to_hash: Default::default(),
            sources,
        }
    }

    // XXX TODO: Can we avoid `AssetPath` being `<'static>`?
    // XXX TODO: Returning BassetHash by value is probably the practical choice? But check.
    pub(crate) async fn get(
        &self,
        path: &RootAssetPath<'static>,
    ) -> Result<ContentHash, BevyError> {
        if let Some(hash) = self
            .path_to_hash
            .read()
            .unwrap_or_else(PoisonError::into_inner)
            .get(path)
        {
            // XXX TODO: Review. Too spammy for now.
            //info!(?path, "Content cache hit.");

            return Ok(*hash);
        }

        // XXX TODO: Review. Too spammy for now.
        //info!(?path, "Content cache miss.");

        let source = self.sources.get(path.source()).map_err(BevyError::from)?;

        let mut reader = source
            .reader()
            .read(path.path())
            .await
            .map_err(BevyError::from)?;

        let mut bytes = Vec::<u8>::new();
        reader
            .read_to_end(&mut bytes)
            .await
            .map_err(BevyError::from)?;

        let hash = content_hash(&bytes);

        self.path_to_hash
            .write()
            .unwrap_or_else(PoisonError::into_inner)
            .insert(path.clone(), hash);

        Ok(hash)
    }
}

#[derive(Hash, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Debug, Serialize, Deserialize)]
pub struct DependencyCacheKey(pub BassetHash);

impl DependencyCacheKey {
    pub fn as_bytes(&self) -> [u8; 32] {
        self.0.as_bytes()
    }
}

impl Display for DependencyCacheKey {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        Display::fmt(&self.0, f)
    }
}

impl CacheKey for DependencyCacheKey {
    fn as_hash(&self) -> BassetHash {
        self.0
    }
}

#[derive(Default, Debug, Eq, PartialEq)]
pub(crate) struct DependencyCacheValue {
    loader_dependees: Vec<CacheLoaderDependency>,
    // XXX TODO: Reconsider name? These are any `Handle` or `AssetRef` dependencies
    // stored in the asset value.
    external_dependees: Vec<RootAssetRef<'static>>,
    // XXX TODO: Consider storing more info for debugging. Loader name for one.
}

fn collect_sort_dedup<T: Ord + PartialEq>(iter: impl Iterator<Item = T>) -> Vec<T> {
    let mut value = iter.collect::<Vec<_>>();

    value.sort();
    value.dedup();

    value
}

// XXX TODO: Review. This is the same as `LoaderDependency` but with a dependency
// key. Check if we can avoid duplication or restructure to be nicer. Note that
// we shouldn't - in theory - ever have a mix of None and Some dependency keys.
// We should be consistently keying everything or nothing. Check if that's ever true
// and maybe the dependency key yes/no decision can be moved higher up.
#[derive(Clone, PartialEq, Eq, Hash, Debug, PartialOrd, Ord)]
pub struct CacheLoaderDependency(pub LoaderDependency, pub DependencyCacheKey);

impl CacheLoaderDependency {
    pub(crate) fn new(
        loader_dependency: LoaderDependency,
        dependency_key: DependencyCacheKey,
    ) -> Self {
        Self(loader_dependency, dependency_key)
    }

    pub(crate) fn optional(
        loader_dependency: LoaderDependency,
        dependency_key: Option<DependencyCacheKey>,
    ) -> Option<Self> {
        dependency_key.map(|k| Self::new(loader_dependency, k))
    }
}

impl DependencyCacheValue {
    pub(crate) fn new(
        loader_dependees: impl Iterator<Item = CacheLoaderDependency>,
        external_dependees: impl Iterator<Item = RootAssetRef<'static>>,
    ) -> Self {
        Self {
            loader_dependees: collect_sort_dedup(loader_dependees),
            external_dependees: collect_sort_dedup(external_dependees),
        }
    }

    #[expect(unused, reason = "XXX TODO?")]
    pub(crate) fn empty() -> Self {
        Self {
            loader_dependees: Default::default(),
            external_dependees: Default::default(),
        }
    }

    pub(crate) fn loader_dependees<'a>(&'a self) -> &'a [CacheLoaderDependency] {
        &self.loader_dependees
    }

    #[expect(unused, reason = "XXX TODO")]
    pub(crate) fn external_dependees<'a>(&'a self) -> &'a [RootAssetRef<'static>] {
        &self.external_dependees
    }
}

impl FileCacheValue for Arc<DependencyCacheValue> {
    async fn write(&self, _file: &mut File) -> Result<(), BevyError> {
        // XXX TODO: Should have a serialized struct that includes a version number and an opaque value.
        // XXX TODO: Consider non-pretty serialization. Will be annoying for debugging,
        // but files will be a bit smaller and might\ be faster to save?
        todo!("XXX TODO");
        // let string = ron::ser::to_string_pretty(self.as_ref(), Default::default())
        //     .map_err(BevyError::from)?;

        // file.write_all(string.as_bytes())
        //     .await
        //     .map_err(BevyError::from)
    }

    fn read(_bytes: Box<[u8]>) -> Self {
        todo!("XXX TODO");
        //Arc::new(ron::de::from_bytes::<DependencyCacheValue>(&bytes).expect("TODO"))
    }
}

fn content_hash(bytes: &[u8]) -> ContentHash {
    let mut hasher = blake3::Hasher::new();
    hasher.update(bytes);
    ContentHash(BassetHash::new(*hasher.finalize().as_bytes()))
}

#[derive(Hash, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Debug, Serialize, Deserialize)]
pub struct ActionCacheKey(pub BassetHash);

impl ActionCacheKey {
    fn as_bytes(&self) -> [u8; 32] {
        self.0.as_bytes()
    }

    pub(crate) fn new(
        dependency_key: DependencyCacheKey,
        dependees: impl Iterator<Item = ActionCacheKey>,
    ) -> Self {
        // XXX TODO: In theory we could make the action key the same as the
        // dependency key if there's zero dependees.
        //
        // Pros:
        //   - Saves a hash.
        //   - Can quickly check for leaf from keys alone.
        // Cons:
        //   - Probably gonna cause a bug where the wrong key type is used and
        //     it kinda seems to work.

        // XXX TODO: Should hasher be seeded?
        let mut hasher = blake3::Hasher::new();

        hasher.update(&dependency_key.as_bytes());

        for dependee in dependees {
            hasher.update(&dependee.as_bytes());
        }

        Self(BassetHash::new(*hasher.finalize().as_bytes()))
    }
}

impl Display for ActionCacheKey {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        Display::fmt(&self.0, f)
    }
}

impl CacheKey for ActionCacheKey {
    fn as_hash(&self) -> BassetHash {
        self.0
    }
}
