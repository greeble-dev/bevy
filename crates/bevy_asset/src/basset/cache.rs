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
use bevy_reflect::{
    serde::{ReflectDeserializer, ReflectSerializer},
    Reflect, ReflectDeserialize, ReflectSerialize, TypeRegistryArc,
};
use bevy_tasks::{ConditionalSendFuture, IoTaskPool};
use core::{
    fmt::{Debug, Display},
    result::Result,
};
use core::{hash::Hash, marker::PhantomData};
use serde::{
    de::{DeserializeSeed, Visitor},
    Deserialize, Serialize,
};
use std::path::PathBuf;
use tracing::debug;

// XXX TODO: Can we avoid reflect?
#[derive(Hash, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Reflect)]
#[reflect(Serialize, Deserialize)]
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
        hex::write_hex(f, &self.0)
    }
}

impl Display for BassetHash {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        // Display only the first four bytes for brevity. `Debug` emits
        // all characters.
        hex::write_hex(f, &self.0[0..4])
    }
}

impl Serialize for BassetHash {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        // XXX TODO: How can we avoid allocation? We know the length of the string.
        // XXX TODO: If we avoid allocating then also try to avoid error handling
        // since we should be infallible.
        let mut string = String::with_capacity(64);
        hex::write_hex(&mut string, &self.0).expect("XXX TODO: Handle error?");

        string.serialize(serializer)
    }
}

impl<'de> Deserialize<'de> for BassetHash {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        deserializer.deserialize_string(BassetHashVisitor)
    }
}

struct BassetHashVisitor;

impl<'de> Visitor<'de> for BassetHashVisitor {
    type Value = BassetHash;

    fn expecting(&self, formatter: &mut core::fmt::Formatter) -> core::fmt::Result {
        formatter.write_str("string BassetHash")
    }

    fn visit_str<E>(self, v: &str) -> Result<Self::Value, E>
    where
        E: serde::de::Error,
    {
        Ok(BassetHash(hex::read_hex(v.as_bytes()).expect("XXX TODO")))
    }

    fn visit_string<E>(self, v: String) -> Result<Self::Value, E>
    where
        E: serde::de::Error,
    {
        Ok(BassetHash(hex::read_hex(v.as_bytes()).expect("XXX TODO")))
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
pub(crate) fn should_log(path: &RootAssetRef) -> bool {
    // Skip embedded sources for now as they're too spammy.
    // XXX TODO: Need more robust way of ignoring these.
    !path.to_string().contains("embedded://")
}

fn log(name: &'static str, path: &RootAssetRef, key: BassetHash, string: &'static str) {
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
    fn get(&self, key: &K, asset_path: &RootAssetRef) -> Option<V> {
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
    fn put(&mut self, key: K, value: V, asset_path: &RootAssetRef) {
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
    fn write(
        &self,
        file: &mut File,
        registry: &TypeRegistryArc,
    ) -> impl ConditionalSendFuture<Output = Result<(), BevyError>>;

    fn read(bytes: Box<[u8]>, registry: &TypeRegistryArc) -> Self;
}

impl FileCacheValue for Arc<[u8]> {
    async fn write(&self, file: &mut File, _registry: &TypeRegistryArc) -> Result<(), BevyError> {
        file.write_all(self).await.map_err(BevyError::from)
    }

    fn read(bytes: Box<[u8]>, _registry: &TypeRegistryArc) -> Self {
        bytes.into()
    }
}

#[derive(Default)]
pub(crate) struct FileCache<K: CacheKey, V: FileCacheValue> {
    name: &'static str,
    base_path: PathBuf,
    validate: bool,
    registry: TypeRegistryArc,
    phantom: PhantomData<(K, V)>,
}

impl<K: CacheKey, V: FileCacheValue> FileCache<K, V> {
    pub(crate) fn new(
        name: &'static str,
        base_path: PathBuf,
        validate: bool,
        registry: TypeRegistryArc,
    ) -> Self {
        Self {
            name,
            base_path,
            validate,
            registry,
            phantom: PhantomData::<(K, V)>,
        }
    }

    pub(crate) fn value_path(&self, key: &K) -> PathBuf {
        // XXX TODO: Similar to `Serialize` for `BassetHash`, try to avoid allocating
        // since we know the length of the string.
        let mut hex = String::with_capacity(64);
        hex::write_hex(&mut hex, &key.as_hash().as_bytes()).expect("XXX TODO");

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
    pub(crate) async fn get(&self, key: &K, asset_path: &RootAssetRef) -> Option<V> {
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
            Some(<V as FileCacheValue>::read(value.into(), &self.registry))
        } else {
            None
        }
    }

    // XXX TODO: `asset_path` is only for debugging. Maybe make it more opaque?
    pub(crate) fn put(&self, key: K, value: V, asset_path: &RootAssetRef) {
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

        // XXX TODO: This clone is annoying if the writer doesn't need it.
        let registry = self.registry.clone();

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

                <V as FileCacheValue>::write(&value, &mut temp_file, &registry)
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
        registry: TypeRegistryArc,
    ) -> Self {
        MemoryAndFileCache {
            memory: Arc::new(RwLock::new(MemoryCache::new(name, validate))),
            file: file_cache_path.map(|p| FileCache::new(name, p, validate, registry)),
        }
    }

    // XXX TODO: `asset_path` is only for debugging. Maybe make it more opaque?
    pub(crate) async fn get(&self, key: &K, asset_path: &RootAssetRef) -> Option<V> {
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
    pub(crate) fn put(&self, key: K, value: V, asset_path: &RootAssetRef) {
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

// XXX TODO: Can we avoid reflect? Currently needed by `CacheLoaderDependency`
// since it indirectly uses an `AssetRef` that requires reflection and also
// a `DependencyCacheKey`.
#[derive(Hash, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Debug, Reflect)]
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

#[derive(Default, Debug, Eq, PartialEq, Reflect)]
pub(crate) struct DependencyCacheValue {
    loader_dependees: Vec<CacheLoaderDependency>,
    // XXX TODO: Reconsider name? These are any `Handle` or `AssetRef` dependencies
    // stored in the asset value.
    external_dependees: Vec<RootAssetRef>,
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
#[derive(Clone, PartialEq, Eq, Hash, Debug, PartialOrd, Ord, Reflect)]
pub struct CacheLoaderDependency(pub LoaderDependency, pub DependencyCacheKey);

impl CacheLoaderDependency {
    pub(crate) fn new(
        loader_dependency: LoaderDependency,
        dependency_key: DependencyCacheKey,
    ) -> Self {
        Self(loader_dependency, dependency_key)
    }
}

impl DependencyCacheValue {
    pub(crate) fn new(
        loader_dependees: impl Iterator<Item = CacheLoaderDependency>,
        external_dependees: impl Iterator<Item = RootAssetRef>,
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

    pub(crate) fn external_dependees<'a>(&'a self) -> &'a [RootAssetRef] {
        &self.external_dependees
    }
}

impl FileCacheValue for Arc<DependencyCacheValue> {
    async fn write(&self, file: &mut File, registry: &TypeRegistryArc) -> Result<(), BevyError> {
        // XXX TODO: Should have a serialized struct that includes a version number and an opaque value.
        // XXX TODO: Consider non-pretty serialization. Will be annoying for debugging,
        // but files will be a bit smaller and might be faster to save?

        let string = ron::ser::to_string_pretty(
            &ReflectSerializer::new(self.as_ref(), &registry.read()),
            Default::default(),
        )
        .map_err(BevyError::from)?;

        file.write_all(string.as_bytes())
            .await
            .map_err(BevyError::from)
    }

    fn read(bytes: Box<[u8]>, registry: &TypeRegistryArc) -> Self {
        Arc::new(
            ReflectDeserializer::new(&registry.read())
                .deserialize(&mut ron::de::Deserializer::from_bytes(&bytes).expect("XXX TODO"))
                .expect("XXX TODO")
                .try_take::<DependencyCacheValue>()
                .expect("XXX TODO"),
        )
    }
}

fn content_hash(bytes: &[u8]) -> ContentHash {
    let mut hasher = blake3::Hasher::new();
    hasher.update(bytes);
    ContentHash(BassetHash::new(*hasher.finalize().as_bytes()))
}

#[derive(Hash, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Debug, Reflect)]
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

// XXX TODO: Move this somewhere else. Also should consider just using the common `hex` crate.
mod hex {
    use core::fmt::{Debug, Display, Write};

    // XXX TODO: Annoying that we have to return a result even for the (almost?)
    // infallible case of writing to a string. We also have cases where the
    // length is known and we want to avoid allocation (see `BassetHash::serialize`).
    // Maybe needs a rethink, although
    pub(crate) fn write_hex(f: &mut dyn Write, bytes: &[u8]) -> core::fmt::Result {
        const TABLE: [char; 16] = [
            '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'a', 'b', 'c', 'd', 'e', 'f',
        ];

        for byte in bytes {
            f.write_char(TABLE[(byte >> 4) as usize])?;
            f.write_char(TABLE[(byte & 0xf) as usize])?;
        }

        Ok(())
    }

    #[derive(Debug, PartialEq, Eq)]
    pub(crate) enum HexReadError {
        InvalidCharCount(usize),
        OutOfRangeChar(u8),
    }

    // XXX TODO: Should probably do a proper `thiserror`.
    impl Display for HexReadError {
        fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
            Debug::fmt(self, f)
        }
    }

    pub(crate) fn read_hex_char(value: u8) -> Result<u8, HexReadError> {
        match value {
            b'0'..=b'9' => Ok(value - b'0'),
            b'a'..=b'f' => Ok(value - b'a' + 10),
            // XXX TODO: Consider being stricter and requiring lowercase.
            b'A'..=b'F' => Ok(value - b'A' + 10),
            _ => Err(HexReadError::OutOfRangeChar(value)),
        }
    }

    pub(crate) fn read_hex<const BYTE_COUNT: usize>(
        chars: &[u8],
    ) -> Result<[u8; BYTE_COUNT], HexReadError> {
        let char_count = BYTE_COUNT * 2;

        if char_count != chars.len() {
            return Err(HexReadError::InvalidCharCount(chars.len()));
        }

        let mut result = [0u8; BYTE_COUNT];

        for i in 0..BYTE_COUNT {
            let hi = read_hex_char(chars[i * 2])?;
            let lo = read_hex_char(chars[(i * 2) + 1])?;

            result[i] = (hi << 4) | lo;
        }

        Ok(result)
    }

    #[cfg(test)]
    mod tests {
        use super::*;
        use alloc::{string::String, vec::Vec};
        use std::format;

        #[test]
        fn hex() {
            let expected_bytes = (0..=255u8).collect::<Vec<_>>();

            let expected_string = expected_bytes
                .iter()
                .map(|b| format!("{b:02x}"))
                .collect::<String>();

            let mut actual_string = String::new();
            write_hex(&mut actual_string, &expected_bytes).unwrap();

            assert_eq!(expected_string, actual_string);

            let actual_bytes = read_hex::<256>(actual_string.as_bytes()).unwrap();

            assert_eq!(expected_bytes, actual_bytes);

            assert_eq!(
                read_hex::<1>("123".as_bytes()),
                Err(HexReadError::InvalidCharCount(3))
            );

            assert_eq!(
                read_hex::<1>("1".as_bytes()),
                Err(HexReadError::InvalidCharCount(1))
            );

            assert_eq!(
                read_hex::<1>("".as_bytes()),
                Err(HexReadError::InvalidCharCount(0))
            );

            assert_eq!(read_hex::<0>("".as_bytes()).unwrap().len(), 0);

            for byte in 0..=255u8 {
                if byte.is_ascii_hexdigit() {
                    assert_eq!(
                        read_hex::<1>(&[byte, b'0']).unwrap(),
                        [read_hex_char(byte).unwrap() << 4]
                    );
                    assert_eq!(
                        read_hex::<1>(&[b'0', byte]).unwrap(),
                        [read_hex_char(byte).unwrap()]
                    );
                } else {
                    assert_eq!(
                        read_hex::<1>(&[byte, b'0']),
                        Err(HexReadError::OutOfRangeChar(byte))
                    );
                    assert_eq!(
                        read_hex::<1>(&[b'0', byte]),
                        Err(HexReadError::OutOfRangeChar(byte))
                    );
                }
            }
        }
    }
}
