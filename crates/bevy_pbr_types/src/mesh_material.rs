use bevy_asset::{AsAssetId, Asset, AssetId, Handle};
use bevy_derive::{Deref, DerefMut};
use bevy_ecs::{component::Component, reflect::ReflectComponent};
use bevy_reflect::{std_traits::ReflectDefault, Reflect};
use derive_more::derive::From;

/// A [material](Material) used for rendering a [`Mesh3d`].
///
/// See [`Material`] for general information about 3D materials and how to implement your own materials.
///
/// [`Mesh3d`]: bevy_render::mesh::Mesh3d
///
/// # Example
///
/// ```
/// # use bevy_pbr::{Material, MeshMaterial3d, StandardMaterial};
/// # use bevy_ecs::prelude::*;
/// # use bevy_render::mesh::{Mesh, Mesh3d};
/// # use bevy_color::palettes::basic::RED;
/// # use bevy_asset::Assets;
/// # use bevy_math::primitives::Capsule3d;
/// #
/// // Spawn an entity with a mesh using `StandardMaterial`.
/// fn setup(
///     mut commands: Commands,
///     mut meshes: ResMut<Assets<Mesh>>,
///     mut materials: ResMut<Assets<StandardMaterial>>,
/// ) {
///     commands.spawn((
///         Mesh3d(meshes.add(Capsule3d::default())),
///         MeshMaterial3d(materials.add(StandardMaterial {
///             base_color: RED.into(),
///             ..Default::default()
///         })),
///     ));
/// }
/// ```
#[derive(Component, Clone, Debug, Deref, DerefMut, Reflect, From)]
#[reflect(Component, Default, Clone, PartialEq)]
pub struct MeshMaterial3d<A: Asset + Clone>(pub Handle<A>);

impl<A: Asset + Clone> Default for MeshMaterial3d<A> {
    fn default() -> Self {
        Self(Handle::default())
    }
}

impl<A: Asset + Clone> PartialEq for MeshMaterial3d<A> {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}

impl<A: Asset + Clone> Eq for MeshMaterial3d<A> {}

impl<A: Asset + Clone> From<MeshMaterial3d<A>> for AssetId<A> {
    fn from(material: MeshMaterial3d<A>) -> Self {
        material.id()
    }
}

impl<A: Asset + Clone> From<&MeshMaterial3d<A>> for AssetId<A> {
    fn from(material: &MeshMaterial3d<A>) -> Self {
        material.id()
    }
}

impl<A: Asset + Clone> AsAssetId for MeshMaterial3d<A> {
    type Asset = A;

    fn as_asset_id(&self) -> AssetId<Self::Asset> {
        self.id()
    }
}
