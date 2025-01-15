//! Spawns a variety of skinned glTF meshes playing different animations.

use std::f32::consts::PI;

use bevy::{pbr::CascadeShadowConfigBuilder, prelude::*, scene::SceneInstanceReady};

fn main() {
    App::new()
        .insert_resource(AmbientLight {
            color: Color::WHITE,
            brightness: 2000.,
            ..default()
        })
        .add_plugins(DefaultPlugins)
        .add_systems(Startup, setup_meshes_and_animations)
        .add_systems(Startup, setup_camera_and_environment)
        .run();
}

#[derive(Component)]
struct AnimationToPlay {
    graph_handle: Handle<AnimationGraph>,
    index: AnimationNodeIndex,
}

// Create an animation graph and start loading the mesh and animation.
fn spawn_one_mesh_with_animation(
    commands: &mut Commands,
    mesh_handle: &Handle<Scene>,
    animation_handle: &Handle<AnimationClip>,
    transform: Transform,
    graphs: &mut ResMut<Assets<AnimationGraph>>,
) {
    // Build an animation graph containing a single animation.
    let (graph, index) = AnimationGraph::from_clip(animation_handle.clone());
    let graph_handle = graphs.add(graph);

    // Tell the engine to start loading our mesh and animation, and then spawn
    // them as a scene when ready.
    commands
        .spawn((
            transform,
            SceneRoot(mesh_handle.clone()),
            AnimationToPlay {
                graph_handle,
                index,
            },
        ))
        .observe(play_animation_once_loaded);
}

fn setup_meshes_and_animations(
    mut commands: Commands,
    asset_server: Res<AssetServer>,
    mut graphs: ResMut<Assets<AnimationGraph>>,
) {
    let simple_path = "models/SimpleSkin/SimpleSkin.gltf";
    let simple_mesh = asset_server.load(GltfAssetLabel::Scene(0).from_asset(simple_path));
    let simple_animation = asset_server.load(GltfAssetLabel::Animation(0).from_asset(simple_path));

    spawn_one_mesh_with_animation(
        &mut commands,
        &simple_mesh,
        &simple_animation,
        Transform::from_xyz(-140.0, 0.0, 0.0).with_scale(Vec3::splat(30.0)),
        &mut graphs,
    );

    let fox_path = "models/animated/Fox.glb";
    let fox_mesh = asset_server.load(GltfAssetLabel::Scene(0).from_asset(fox_path));
    let fox_survey_animation = asset_server.load(GltfAssetLabel::Animation(0).from_asset(fox_path));
    let fox_walk_animation = asset_server.load(GltfAssetLabel::Animation(1).from_asset(fox_path));
    let fox_run_animation = asset_server.load(GltfAssetLabel::Animation(2).from_asset(fox_path));

    spawn_one_mesh_with_animation(
        &mut commands,
        &fox_mesh,
        &fox_survey_animation,
        Transform::from_xyz(-40.0, 0.0, 0.0),
        &mut graphs,
    );

    spawn_one_mesh_with_animation(
        &mut commands,
        &fox_mesh,
        &fox_walk_animation,
        Transform::from_xyz(40.0, 0.0, 0.0),
        &mut graphs,
    );

    spawn_one_mesh_with_animation(
        &mut commands,
        &fox_mesh,
        &fox_run_animation,
        Transform::from_xyz(120.0, 0.0, 0.0),
        &mut graphs,
    );
}

// Detect that the scene is loaded and spawned, then play the animation.
fn play_animation_once_loaded(
    trigger: Trigger<SceneInstanceReady>,
    children: Query<&Children>,
    animation_to_play: Query<&AnimationToPlay>,
    mut commands: Commands,
    mut players: Query<&mut AnimationPlayer>,
) {
    if let Ok(animation) = animation_to_play.get(trigger.target()) {
        for child in children.iter_descendants(trigger.target()) {
            if let Ok(mut player) = players.get_mut(child) {
                player.play(animation.index).repeat();

                commands
                    .entity(child)
                    .insert(AnimationGraphHandle(animation.graph_handle.clone()));
            }
        }
    }
}

// Spawn a camera and a simple environment with a ground plane and light.
fn setup_camera_and_environment(
    mut commands: Commands,
    mut meshes: ResMut<Assets<Mesh>>,
    mut materials: ResMut<Assets<StandardMaterial>>,
) {
    // Camera
    commands.spawn((
        Camera3d::default(),
        Transform::from_xyz(0.0, 150.0, 250.0).looking_at(Vec3::new(0.0, 40.0, 0.0), Vec3::Y),
    ));

    // Plane
    commands.spawn((
        Mesh3d(meshes.add(Plane3d::default().mesh().size(500000.0, 500000.0))),
        MeshMaterial3d(materials.add(Color::srgb(0.3, 0.5, 0.3))),
    ));

    // Light
    commands.spawn((
        Transform::from_rotation(Quat::from_euler(EulerRot::ZYX, 0.0, PI * -0.25, PI * -0.25)),
        DirectionalLight {
            shadows_enabled: true,
            ..default()
        },
        CascadeShadowConfigBuilder {
            first_cascade_far_bound: 400.0,
            maximum_distance: 800.0,
            ..default()
        }
        .build(),
    ));
}
