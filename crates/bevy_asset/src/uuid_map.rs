use core::any::TypeId;

use alloc::sync::Arc;

use bevy_ecs::resource::Resource;
use bevy_platform::{
    collections::{hash_map::Entry, HashMap, HashSet},
    sync::{PoisonError, RwLock, RwLockReadGuard},
};
use bevy_utils::TypeIdMap;
use thiserror::Error;
use uuid::Uuid;

use crate::{
    Asset, AssetEntity, EntityHandle, ErasedAssetId, ErasedEntityHandle, ErasedHandle, Handle,
    UntypedAssetId,
};

/// Maps asset UUIDs to the asset handle assigned to it.
#[derive(Resource, Clone, Default)]
pub struct AssetUuidMap(Arc<RwLock<TypeIdMap<AssetUuidMapInner>>>);

#[derive(Default)]
pub(crate) struct AssetUuidMapInner {
    pub(crate) uuid_to_handle: HashMap<Uuid, ErasedEntityHandle>,
    entity_to_uuids: HashMap<AssetEntity, HashSet<Uuid>>,
}

impl AssetUuidMap {
    /// Sets the handle that a UUID refers to.
    pub fn set_uuid(&mut self, uuid: Uuid, handle: ErasedEntityHandle) {
        let mut type_id_map = self.0.write().unwrap_or_else(PoisonError::into_inner);
        let inner = type_id_map.entry(handle.type_id()).or_default();
        let new_entity = handle.entity();
        match inner.uuid_to_handle.entry(uuid) {
            Entry::Vacant(entry) => {
                entry.insert(handle);
            }
            Entry::Occupied(mut entry) => {
                let old_entity = entry.get().entity();
                inner
                    .entity_to_uuids
                    .get_mut(&old_entity)
                    .unwrap()
                    .remove(&uuid);
                entry.insert(handle);
            }
        }
        inner
            .entity_to_uuids
            .entry(new_entity)
            .or_default()
            .insert(uuid);
    }

    /// Convenience function for accessing the internal uuid map.
    pub(crate) fn read(&self) -> RwLockReadGuard<'_, TypeIdMap<AssetUuidMapInner>> {
        self.0.read().unwrap_or_else(PoisonError::into_inner)
    }

    /// Converts an untyped handle into the corresponding [`UntypedEntityHandle`].
    ///
    /// For [`UntypedHandle::Strong`], this is a no-op. For [`UntypedHandle::Uuid`], this lookups
    /// the corresponding UUID and returns [`Err`] if missing.
    pub fn resolve_erased_handle(
        &self,
        handle: ErasedHandle,
    ) -> Result<ErasedEntityHandle, ResolveUuidError> {
        match handle {
            ErasedHandle::Strong { type_id, handle } => Ok(ErasedEntityHandle { type_id, handle }),
            ErasedHandle::Uuid { type_id, uuid } => self
                .read()
                .get(&type_id)
                .and_then(|inner| inner.uuid_to_handle.get(&uuid))
                .cloned()
                .ok_or(ResolveUuidError(uuid)),
        }
    }

    /// Converts a handle into the corresponding [`EntityHandle`].
    ///
    /// For [`Handle::Strong`], this is a no-op. For [`Handle::Uuid`], this lookups the
    /// corresponding UUID and returns [`Err`] if missing.
    pub fn resolve_handle<A: Asset>(
        &self,
        handle: Handle<A>,
    ) -> Result<EntityHandle<A>, ResolveUuidError> {
        self.resolve_erased_handle(handle.erased())
            // It's safe to unwrap, since either the handle was just passed through, or we looked up
            // the handle by its type ID, so the types must match.
            .map(|handle| handle.try_typed().unwrap())
    }

    /// Converts an asset ID into the corresponding [`AssetEntity`].
    ///
    /// This is the same as [`Self::resolve_handle`], but is slightly more efficient for
    /// cases where you don't need the resolved handle.
    pub fn resolve_entity(
        &self,
        id: impl Into<ErasedAssetId>,
    ) -> Result<AssetEntity, ResolveUuidError> {
        match id.into() {
            ErasedAssetId::Entity { entity, .. } => Ok(entity),
            ErasedAssetId::Uuid { type_id, uuid } => self
                .read()
                .get(&type_id)
                .and_then(|inner| inner.uuid_to_handle.get(&uuid))
                .map(|value| value.handle.entity)
                .ok_or(ResolveUuidError(uuid)),
        }
    }

    // XXX TODO FATAL?: Maybe not fatal but annoying - `AssetServer::get_load_status`
    // and others need to do the look up without knowing the type id. Is this
    // slower loop acceptable or are there other workarounds? Or can we let this
    // slide since UUID handles are expected to go away at some point?
    pub fn resolve_entity_untyped(
        &self,
        id: impl Into<UntypedAssetId>,
    ) -> Result<AssetEntity, ResolveUuidError> {
        match id.into() {
            UntypedAssetId::Entity(entity) => Ok(entity),
            UntypedAssetId::Uuid(uuid) => {
                let type_id_map = self.read();

                for inner in type_id_map.values() {
                    if let Some(handle) = inner.uuid_to_handle.get(&uuid) {
                        return Ok(handle.handle.entity);
                    }
                }
                Err(ResolveUuidError(uuid))
            }
        }
    }

    /// Returns a reverse mapping from an entity to all UUIDs that reference it.
    pub(crate) fn entity_to_uuids(
        &self,
        entity: AssetEntity,
        type_id: TypeId,
    ) -> Option<HashSet<Uuid>> {
        Some(
            self.read()
                .get(&type_id)?
                .entity_to_uuids
                .get(&entity)?
                .clone(),
        )
    }
}

/// An error while resolve a [`Uuid`] in the [`AssetUuidMap`].
#[derive(Error, Debug)]
#[error("There is no asset handle assigned to uuid {0}")]
pub struct ResolveUuidError(pub Uuid);
