//! Create invalid skinned meshes to test renderer behaviour.
//!
//! If working correctly, the user should see three meshes. The left
//! mesh should be animating. The middle and right meshes should be static.

use bevy::{
    core_pipeline::prepass::DepthPrepass,
    math::ops,
    prelude::*,
    render::{
        mesh::{
            skinning::{SkinnedMesh, SkinnedMeshInverseBindposes},
            Indices, PrimitiveTopology, VertexAttributeValues,
        },
        render_asset::RenderAssetUsages,
    },
};

fn main() {
    App::new()
        .add_plugins(DefaultPlugins)
        .add_systems(Startup, setup)
        .add_systems(Update, joint_animation)
        .run();
}

#[derive(Component)]
struct AnimatedJoint;

fn setup(
    mut commands: Commands,
    mut mesh_assets: ResMut<Assets<Mesh>>,
    mut material_assets: ResMut<Assets<StandardMaterial>>,
    mut inverse_bindposes_assets: ResMut<Assets<SkinnedMeshInverseBindposes>>,
) {
    commands.spawn((
        Camera3d::default(),
        Transform::from_xyz(0.0, 0.0, 4.0).looking_at(Vec3::ZERO, Vec3::Y),
        // Make sure we exercise the renderer's prepass path.
        DepthPrepass,
    ));

    // Add a directional light to make sure we exercise the renderer's lighting path.
    commands.spawn((
        Transform::IDENTITY,
        DirectionalLight {
            shadows_enabled: true,
            ..default()
        },
    ));

    let mesh = mesh_assets.add(
        Mesh::new(
            PrimitiveTopology::TriangleList,
            RenderAssetUsages::default(),
        )
        .with_inserted_attribute(
            Mesh::ATTRIBUTE_POSITION,
            vec![
                [-0.5, -0.5, 0.0],
                [0.5, -0.5, 0.0],
                [-0.5, 0.5, 0.0],
                [0.5, 0.5, 0.0],
            ],
        )
        .with_inserted_attribute(
            Mesh::ATTRIBUTE_JOINT_INDEX,
            VertexAttributeValues::Uint16x4(vec![
                [0, 0, 0, 0],
                [0, 0, 0, 0],
                [1, 0, 0, 0],
                [1, 0, 0, 0],
            ]),
        )
        .with_inserted_attribute(
            Mesh::ATTRIBUTE_JOINT_WEIGHT,
            vec![
                [1.00, 0.00, 0.0, 0.0],
                [1.00, 0.00, 0.0, 0.0],
                [1.00, 0.00, 0.0, 0.0],
                [1.00, 0.00, 0.0, 0.0],
            ],
        )
        .with_inserted_indices(Indices::U16(vec![0, 1, 3, 0, 3, 2])),
    );

    let inverse_bindposes = inverse_bindposes_assets.add(vec![Mat4::IDENTITY; 2]);

    let material = material_assets.add(StandardMaterial {
        cull_mode: None,
        ..default()
    });

    // Spawn three meshes, but deliberately break two of them.
    for mesh_index in 0..3 {
        let transform = Transform::from_xyz((mesh_index - 1) as f32 * 1.5, 0.0, 0.0);

        let joint_0 = commands.spawn(transform).id();

        let joint_1 = commands
            .spawn((ChildOf(joint_0), AnimatedJoint, Transform::IDENTITY))
            .id();

        let mesh_entity = commands
            .spawn((
                Mesh3d(mesh.clone()),
                MeshMaterial3d(material.clone()),
                SkinnedMesh {
                    inverse_bindposes: inverse_bindposes.clone(),
                    joints: vec![joint_0, joint_1],
                },
                transform,
            ))
            .id();

        match mesh_index {
            1 => {
                // Remove the `SkinnedMesh` component. The should make `extract_skins`
                // skip the mesh entirely and render it as unskinned.
                commands.entity(mesh_entity).remove::<SkinnedMesh>();
            }
            2 => {
                // Remove one joint entity. This should make `extract_skins`
                // try to gather the joints for this mesh, fail part way
                // through, then fall back to rendering it as unskinned.
                commands.entity(joint_1).despawn();
            }
            _ => {}
        }
    }
}

fn joint_animation(time: Res<Time>, mut query: Query<(&mut Transform, &AnimatedJoint)>) {
    for (mut transform, _) in &mut query {
        transform.rotation = Quat::from_rotation_y(ops::sin(time.elapsed_secs()));
    }
}
