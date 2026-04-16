use core::{
    any::TypeId,
    fmt::{Debug, Display},
    hash::Hash,
    marker::PhantomData,
};

use bevy_ecs::entity::Entity;
use bevy_reflect::{prelude::ReflectDefault, Reflect};
use derive_more::{Display, From};
use thiserror::Error;
use uuid::Uuid;

use crate::Asset;

/// A wrapper around an [`Entity`] indicating this refers to an asset.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug, Display, Reflect)]
pub struct AssetEntity(Entity);

impl AssetEntity {
    /// Creates a new instance for `entity`.
    ///
    /// No checking is performed that this entity actually refers to an asset. It is up to the
    /// caller to ensure this is an asset.
    #[inline]
    pub fn new_unchecked(entity: Entity) -> Self {
        Self(entity)
    }

    /// Returns the entity being referenced.
    #[inline]
    pub fn raw_entity(&self) -> Entity {
        self.0
    }
}

/// A unique runtime-only identifier for an [`Asset`]. This is cheap to [`Copy`]/[`Clone`] and is not directly tied to the
/// lifetime of the Asset. This means it _can_ point to an [`Asset`] that no longer exists.
///
/// For an identifier tied to the lifetime of an asset, see [`Handle`](`crate::Handle`).
///
/// For an "untyped" / "generic-less" id, see [`UntypedAssetId`].
#[derive(Reflect, From)]
#[reflect(Clone, Default, Debug, PartialEq, Hash)]
pub enum AssetId<A: Asset> {
    /// The entity on which the asset data is stored. This is the "default" identifier used for
    /// assets. The alternative(s) (ex: [`AssetId::Uuid`]) will only be used if assets are
    /// explicitly registered that way.
    Entity {
        /// The entity which (will) store the data.
        entity: AssetEntity,
        /// A marker to store the type information of the asset.
        #[reflect(ignore, clone)]
        marker: PhantomData<fn() -> A>,
    },
    /// A stable-across-runs / const asset identifier. This will only be used if an asset is
    /// explicitly registered with one (through [`DirectAssetAccessExt::spawn_uuid_asset`] or
    /// [`AssetCommands::spawn_uuid_asset`]).
    ///
    /// [`DirectAssetAccessExt::spawn_uuid_asset`]: crate::DirectAssetAccessExt::spawn_uuid_asset
    /// [`AssetCommands::spawn_uuid_asset`]: crate::AssetCommands::spawn_uuid_asset
    Uuid {
        /// The UUID provided during asset registration.
        uuid: Uuid,
    },
}

impl<A: Asset> AssetId<A> {
    /// The uuid for the default [`AssetId`]. It is valid to assign a value to this and by
    /// convention (where appropriate) assets should support this pattern.
    pub const DEFAULT_UUID: Uuid = Uuid::from_u128(200809721996911295814598172825939264631);

    /// This asset id _should_ never be valid. Assigning a value to this will produce undefined
    /// behavior, so don't do it!
    pub const INVALID_UUID: Uuid = Uuid::from_u128(108428345662029828789348721013522787528);

    /// Returns an [`AssetId`] with [`Self::INVALID_UUID`], which _should_ never be assigned to.
    #[inline]
    pub const fn invalid() -> Self {
        Self::Uuid {
            uuid: Self::INVALID_UUID,
        }
    }

    /// Returns the entity if this is an entity ID.
    #[inline]
    pub fn entity(&self) -> Option<AssetEntity> {
        match self {
            Self::Entity { entity, .. } => Some(*entity),
            Self::Uuid { .. } => None,
        }
    }

    /// Returns the UUID if this is a UUID ID.
    #[inline]
    pub fn uuid(&self) -> Option<Uuid> {
        match self {
            Self::Uuid { uuid } => Some(*uuid),
            Self::Entity { .. } => None,
        }
    }

    /// Converts this to an "untyped" / "generic-less" [`Asset`] identifier that stores the type information
    /// _inside_ the [`UntypedAssetId`].
    #[inline]
    pub fn untyped(self) -> UntypedAssetId {
        match self {
            AssetId::Entity { entity, .. } => UntypedAssetId::Entity(entity),
            AssetId::Uuid { uuid } => UntypedAssetId::Uuid(uuid),
        }
    }

    // XXX TODO: Document.
    #[inline]
    pub fn erased(self) -> ErasedAssetId {
        self.into()
    }

    #[inline]
    fn internal(self) -> InternalAssetId {
        match self {
            AssetId::Entity { entity, .. } => InternalAssetId::Entity(entity),
            AssetId::Uuid { uuid } => InternalAssetId::Uuid(uuid),
        }
    }
}

impl<A: Asset> Default for AssetId<A> {
    fn default() -> Self {
        AssetId::Uuid {
            uuid: Self::DEFAULT_UUID,
        }
    }
}

impl<A: Asset> Clone for AssetId<A> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<A: Asset> Copy for AssetId<A> {}

impl<A: Asset> Display for AssetId<A> {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        Debug::fmt(self, f)
    }
}

impl<A: Asset> Debug for AssetId<A> {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        match self {
            AssetId::Entity { entity, .. } => {
                write!(
                    f,
                    "AssetId<{}>{{ entity: {} }}",
                    core::any::type_name::<A>(),
                    *entity,
                )
            }
            AssetId::Uuid { uuid } => {
                write!(
                    f,
                    "AssetId<{}>{{uuid: {}}}",
                    core::any::type_name::<A>(),
                    uuid
                )
            }
        }
    }
}

impl<A: Asset> Hash for AssetId<A> {
    #[inline]
    fn hash<H: core::hash::Hasher>(&self, state: &mut H) {
        self.internal().hash(state);
        TypeId::of::<A>().hash(state);
    }
}

impl<A: Asset> PartialEq for AssetId<A> {
    #[inline]
    fn eq(&self, other: &Self) -> bool {
        self.internal().eq(&other.internal())
    }
}

impl<A: Asset> Eq for AssetId<A> {}

impl<A: Asset> PartialOrd for AssetId<A> {
    fn partial_cmp(&self, other: &Self) -> Option<core::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl<A: Asset> Ord for AssetId<A> {
    fn cmp(&self, other: &Self) -> core::cmp::Ordering {
        self.internal().cmp(&other.internal())
    }
}

impl<A: Asset> From<AssetEntity> for AssetId<A> {
    #[inline]
    fn from(value: AssetEntity) -> Self {
        Self::Entity {
            entity: value,
            marker: PhantomData,
        }
    }
}

/// An "untyped" / "generic-less" [`Asset`] identifier that behaves much like [`AssetId`], but stores the [`Asset`] type
/// information at runtime instead of compile-time. This increases the size of the type, but it enables storing asset ids
/// across asset types together and enables comparisons between them.
#[derive(Debug, Copy, Clone, Reflect)]
pub enum ErasedAssetId {
    /// The entity on which the data is stored. This is the "default" identifier used for assets.
    /// The alternative(s) (ex: [`ErasedAssetId::Uuid`]) will only be used if assets are explicitly
    /// registered that way.
    Entity {
        /// An identifier that records the underlying asset type.
        type_id: TypeId,
        /// The entity storing this asset.
        entity: AssetEntity,
    },
    /// A stable-across-runs / const asset identifier. This will only be used if an asset is
    /// explicitly registered with one.
    Uuid {
        /// An identifier that records the underlying asset type.
        type_id: TypeId,
        /// The UUID provided during asset registration.
        uuid: Uuid,
    },
}

impl ErasedAssetId {
    /// Returns the entity if this is an entity ID.
    #[inline]
    pub fn entity(&self) -> Option<AssetEntity> {
        match self {
            Self::Entity { entity, .. } => Some(*entity),
            Self::Uuid { .. } => None,
        }
    }

    /// Returns the UUID if this is a UUID ID.
    #[inline]
    pub fn uuid(&self) -> Option<Uuid> {
        match self {
            Self::Uuid { uuid, .. } => Some(*uuid),
            Self::Entity { .. } => None,
        }
    }

    /// Converts this to a "typed" [`AssetId`] without checking the stored type to see if it matches the target `A` [`Asset`] type.
    /// This should only be called if you are _absolutely certain_ the asset type matches the stored type. And even then, you should
    /// consider using [`ErasedAssetId::typed_debug_checked`] instead.
    #[inline]
    pub fn typed_unchecked<A: Asset>(self) -> AssetId<A> {
        match self {
            ErasedAssetId::Entity { entity, .. } => AssetId::Entity {
                entity,
                marker: PhantomData,
            },
            ErasedAssetId::Uuid { uuid, .. } => AssetId::Uuid { uuid },
        }
    }

    /// Converts this to a "typed" [`AssetId`]. When compiled in debug-mode it will check to see if the stored type
    /// matches the target `A` [`Asset`] type. When compiled in release-mode, this check will be skipped.
    ///
    /// # Panics
    ///
    /// Panics if compiled in debug mode and the [`TypeId`] of `A` does not match the stored [`TypeId`].
    #[inline]
    pub fn typed_debug_checked<A: Asset>(self) -> AssetId<A> {
        debug_assert_eq!(
            self.type_id(),
            TypeId::of::<A>(),
            "The target AssetId<{}>'s TypeId does not match the TypeId of this ErasedAssetId",
            core::any::type_name::<A>()
        );
        self.typed_unchecked()
    }

    /// Converts this to a "typed" [`AssetId`].
    ///
    /// # Panics
    ///
    /// Panics if the [`TypeId`] of `A` does not match the stored type id.
    #[inline]
    pub fn typed<A: Asset>(self) -> AssetId<A> {
        let Ok(id) = self.try_typed() else {
            panic!(
                "The target AssetId<{}>'s TypeId does not match the TypeId of this ErasedAssetId",
                core::any::type_name::<A>()
            )
        };

        id
    }

    /// Try to convert this to a "typed" [`AssetId`].
    #[inline]
    pub fn try_typed<A: Asset>(self) -> Result<AssetId<A>, ErasedAssetIdConversionError> {
        AssetId::try_from(self)
    }

    /// Returns the stored [`TypeId`] of the referenced [`Asset`].
    #[inline]
    pub fn type_id(&self) -> TypeId {
        match self {
            ErasedAssetId::Entity { type_id, .. } | ErasedAssetId::Uuid { type_id, .. } => *type_id,
        }
    }

    #[inline]
    pub fn untyped(&self) -> UntypedAssetId {
        UntypedAssetId::from(*self)
    }

    #[inline]
    fn internal(self) -> InternalAssetId {
        match self {
            ErasedAssetId::Entity { entity, .. } => InternalAssetId::Entity(entity),
            ErasedAssetId::Uuid { uuid, .. } => InternalAssetId::Uuid(uuid),
        }
    }
}

impl Display for ErasedAssetId {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        let mut writer = f.debug_struct("ErasedAssetId");
        match self {
            ErasedAssetId::Entity { entity, type_id } => {
                writer.field("type_id", type_id).field("entity", &entity);
            }
            ErasedAssetId::Uuid { uuid, type_id } => {
                writer.field("type_id", type_id).field("uuid", uuid);
            }
        }
        writer.finish()
    }
}

impl PartialEq for ErasedAssetId {
    #[inline]
    fn eq(&self, other: &Self) -> bool {
        self.type_id() == other.type_id() && self.internal().eq(&other.internal())
    }
}

impl Eq for ErasedAssetId {}

impl Hash for ErasedAssetId {
    #[inline]
    fn hash<H: core::hash::Hasher>(&self, state: &mut H) {
        self.internal().hash(state);
        self.type_id().hash(state);
    }
}

impl Ord for ErasedAssetId {
    fn cmp(&self, other: &Self) -> core::cmp::Ordering {
        self.type_id()
            .cmp(&other.type_id())
            .then_with(|| self.internal().cmp(&other.internal()))
    }
}

impl PartialOrd for ErasedAssetId {
    fn partial_cmp(&self, other: &Self) -> Option<core::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

/// An asset id without static or dynamic types associated with it.
///
/// This is provided to make implementing traits easier for the many different asset ID types.
#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash, PartialOrd, Ord, From)]
enum InternalAssetId {
    Entity(AssetEntity),
    Uuid(Uuid),
}

// Cross Operations

impl<A: Asset> PartialEq<ErasedAssetId> for AssetId<A> {
    #[inline]
    fn eq(&self, other: &ErasedAssetId) -> bool {
        TypeId::of::<A>() == other.type_id() && self.internal().eq(&other.internal())
    }
}

impl<A: Asset> PartialEq<AssetId<A>> for ErasedAssetId {
    #[inline]
    fn eq(&self, other: &AssetId<A>) -> bool {
        other.eq(self)
    }
}

impl<A: Asset> PartialOrd<ErasedAssetId> for AssetId<A> {
    #[inline]
    fn partial_cmp(&self, other: &ErasedAssetId) -> Option<core::cmp::Ordering> {
        if TypeId::of::<A>() != other.type_id() {
            None
        } else {
            Some(self.internal().cmp(&other.internal()))
        }
    }
}

impl<A: Asset> PartialOrd<AssetId<A>> for ErasedAssetId {
    #[inline]
    fn partial_cmp(&self, other: &AssetId<A>) -> Option<core::cmp::Ordering> {
        Some(other.partial_cmp(self)?.reverse())
    }
}

impl<A: Asset> From<AssetId<A>> for ErasedAssetId {
    #[inline]
    fn from(value: AssetId<A>) -> Self {
        let type_id = TypeId::of::<A>();

        match value {
            AssetId::Entity { entity, .. } => ErasedAssetId::Entity { type_id, entity },
            AssetId::Uuid { uuid } => ErasedAssetId::Uuid { type_id, uuid },
        }
    }
}

impl<A: Asset> TryFrom<ErasedAssetId> for AssetId<A> {
    type Error = ErasedAssetIdConversionError;

    #[inline]
    fn try_from(value: ErasedAssetId) -> Result<Self, Self::Error> {
        let found = value.type_id();
        let expected = TypeId::of::<A>();

        match value {
            ErasedAssetId::Entity { entity, type_id } if type_id == expected => {
                Ok(AssetId::Entity {
                    entity,
                    marker: PhantomData,
                })
            }
            ErasedAssetId::Uuid { uuid, type_id } if type_id == expected => {
                Ok(AssetId::Uuid { uuid })
            }
            _ => Err(ErasedAssetIdConversionError::TypeIdMismatch { expected, found }),
        }
    }
}

/// Errors preventing the conversion of to/from an [`UntypedAssetId`] and an [`AssetId`].
#[derive(Error, Debug, PartialEq, Clone)]
#[non_exhaustive]
pub enum ErasedAssetIdConversionError {
    /// Caused when trying to convert an [`UntypedAssetId`] into an [`AssetId`] of the wrong type.
    #[error("This UntypedAssetId is for {found:?} and cannot be converted into an AssetId<{expected:?}>")]
    TypeIdMismatch {
        /// The [`TypeId`] of the asset that we are trying to convert to.
        expected: TypeId,
        /// The [`TypeId`] of the asset that we are trying to convert from.
        found: TypeId,
    },
}

// XXX TODO: Documentation update.
/// An "untyped" / "generic-less" [`Asset`] identifier that behaves much like [`AssetId`], but stores the [`Asset`] type
/// information at runtime instead of compile-time. This increases the size of the type, but it enables storing asset ids
/// across asset types together and enables comparisons between them.
#[derive(Debug, Copy, Clone, Reflect)]
pub enum UntypedAssetId {
    /// The entity on which the data is stored. This is the "default" identifier used for assets.
    /// The alternative(s) (ex: [`UntypedAssetId::Uuid`]) will only be used if assets are explicitly
    /// registered that way.
    Entity(AssetEntity),
    /// A stable-across-runs / const asset identifier. This will only be used if an asset is
    /// explicitly registered with one.
    Uuid(Uuid),
}

impl UntypedAssetId {
    /// Returns the entity if this is an entity ID.
    #[inline]
    pub fn entity(&self) -> Option<AssetEntity> {
        match self {
            Self::Entity(entity) => Some(*entity),
            Self::Uuid(_) => None,
        }
    }

    /// Returns the UUID if this is a UUID ID.
    #[inline]
    pub fn uuid(&self) -> Option<Uuid> {
        match self {
            Self::Uuid(uuid) => Some(*uuid),
            Self::Entity(_) => None,
        }
    }

    /// Converts this to a "typed" [`AssetId`].
    // XXX TODO: Maybe shouldn't expose this now that the type can't be checked? Or call it `typed_unchecked`?
    #[inline]
    pub fn typed<A: Asset>(self) -> AssetId<A> {
        match self {
            UntypedAssetId::Entity(entity) => AssetId::Entity {
                entity,
                marker: PhantomData,
            },
            UntypedAssetId::Uuid(uuid) => AssetId::Uuid { uuid },
        }
    }

    #[inline]
    pub fn erased(self, type_id: TypeId) -> ErasedAssetId {
        match self {
            UntypedAssetId::Entity(entity) => ErasedAssetId::Entity { entity, type_id },
            UntypedAssetId::Uuid(uuid) => ErasedAssetId::Uuid { uuid, type_id },
        }
    }

    // XXX TODO: Might not be necessary any more since our type id was removed?
    // That might also mean `Eq/PartialEq/Ord` can be derived automatically.
    #[inline]
    fn internal(self) -> InternalAssetId {
        match self {
            UntypedAssetId::Entity(entity) => InternalAssetId::Entity(entity),
            UntypedAssetId::Uuid(uuid) => InternalAssetId::Uuid(uuid),
        }
    }
}

impl Display for UntypedAssetId {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        let mut writer = f.debug_struct("UntypedAssetId");
        match self {
            UntypedAssetId::Entity(entity) => {
                writer.field("entity", &entity);
            }
            UntypedAssetId::Uuid(uuid) => {
                writer.field("uuid", uuid);
            }
        }
        writer.finish()
    }
}

impl PartialEq for UntypedAssetId {
    #[inline]
    fn eq(&self, other: &Self) -> bool {
        self.internal().eq(&other.internal())
    }
}

impl Eq for UntypedAssetId {}

impl Hash for UntypedAssetId {
    #[inline]
    fn hash<H: core::hash::Hasher>(&self, state: &mut H) {
        self.internal().hash(state);
    }
}

impl Ord for UntypedAssetId {
    fn cmp(&self, other: &Self) -> core::cmp::Ordering {
        self.internal().cmp(&other.internal())
    }
}

impl PartialOrd for UntypedAssetId {
    fn partial_cmp(&self, other: &Self) -> Option<core::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl<A: Asset> From<AssetId<A>> for UntypedAssetId {
    #[inline]
    fn from(value: AssetId<A>) -> Self {
        match value {
            AssetId::Entity { entity, .. } => UntypedAssetId::Entity(entity),
            AssetId::Uuid { uuid } => UntypedAssetId::Uuid(uuid),
        }
    }
}

impl From<ErasedAssetId> for UntypedAssetId {
    #[inline]
    fn from(value: ErasedAssetId) -> Self {
        match value {
            ErasedAssetId::Entity { entity, .. } => UntypedAssetId::Entity(entity),
            ErasedAssetId::Uuid { uuid, .. } => UntypedAssetId::Uuid(uuid),
        }
    }
}

// XXX TODO: Tested for untype variants.
#[cfg(test)]
mod tests {
    use super::*;

    type TestAsset = ();

    const UUID_1: Uuid = Uuid::from_u128(123);
    const UUID_2: Uuid = Uuid::from_u128(456);

    /// Simple utility to directly hash a value using a fixed hasher
    fn hash<T: Hash>(data: &T) -> u64 {
        use core::hash::BuildHasher;

        bevy_platform::hash::FixedHasher.hash_one(data)
    }

    /// Typed and Erased `AssetIds` should be equivalent to each other and themselves
    #[test]
    fn equality() {
        let typed = AssetId::<TestAsset>::Uuid { uuid: UUID_1 };
        let erased = ErasedAssetId::Uuid {
            type_id: TypeId::of::<TestAsset>(),
            uuid: UUID_1,
        };

        assert_eq!(Ok(typed), AssetId::try_from(erased));
        assert_eq!(ErasedAssetId::from(typed), erased);
        assert_eq!(typed, erased);
    }

    /// Typed and Erased `AssetIds` should be orderable amongst each other and themselves
    #[test]
    fn ordering() {
        assert!(UUID_1 < UUID_2);

        let typed_1 = AssetId::<TestAsset>::Uuid { uuid: UUID_1 };
        let typed_2 = AssetId::<TestAsset>::Uuid { uuid: UUID_2 };
        let erased_1 = ErasedAssetId::Uuid {
            type_id: TypeId::of::<TestAsset>(),
            uuid: UUID_1,
        };
        let erased_2 = ErasedAssetId::Uuid {
            type_id: TypeId::of::<TestAsset>(),
            uuid: UUID_2,
        };

        assert!(typed_1 < typed_2);
        assert!(erased_1 < erased_2);

        assert!(ErasedAssetId::from(typed_1) < erased_2);
        assert!(erased_1 < ErasedAssetId::from(typed_2));

        assert!(AssetId::try_from(erased_1).unwrap() < typed_2);
        assert!(typed_1 < AssetId::try_from(erased_2).unwrap());

        assert!(typed_1 < erased_2);
        assert!(erased_1 < typed_2);
    }

    /// Typed and Erased `AssetIds` should be equivalently hashable to each other and themselves
    #[test]
    fn hashing() {
        let typed = AssetId::<TestAsset>::Uuid { uuid: UUID_1 };
        let erased = ErasedAssetId::Uuid {
            type_id: TypeId::of::<TestAsset>(),
            uuid: UUID_1,
        };

        assert_eq!(
            hash(&typed),
            hash(&AssetId::<TestAsset>::try_from(erased).unwrap())
        );
        assert_eq!(hash(&ErasedAssetId::from(typed)), hash(&erased));
        assert_eq!(hash(&typed), hash(&erased));
    }

    /// Typed and Erased `AssetIds` should be interchangeable
    #[test]
    fn conversion() {
        let typed = AssetId::<TestAsset>::Uuid { uuid: UUID_1 };
        let erased = ErasedAssetId::Uuid {
            type_id: TypeId::of::<TestAsset>(),
            uuid: UUID_1,
        };

        assert_eq!(Ok(typed), AssetId::try_from(erased));
        assert_eq!(ErasedAssetId::from(typed), erased);
    }
}
