//! Spawns a variety of skinned glTF meshes playing different animations.

use bevy::{pbr::CascadeShadowConfigBuilder, prelude::*, scene::SceneInstanceReady};
use std::f32::consts::PI;

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

// A component that records what animation we chose to play on a mesh instance.
// This is added to the mesh instance's entity when we start loading the assets,
// and then once they're loaded and spawned we use it to play the animation.
#[derive(Component)]
struct ChosenAnimation {
    graph_handle: Handle<AnimationGraph>,
    index: AnimationNodeIndex,
}

// Given a mesh and animation asset, spawn an entity that will load and spawn a
// an instance of that mesh along with an animation player.
fn spawn_mesh_with_animation(
    commands: &mut Commands,
    mesh_handle: &Handle<Scene>,
    animation_handle: &Handle<AnimationClip>,
    transform: Transform,
    graphs: &mut ResMut<Assets<AnimationGraph>>,
) {
    // Build an animation graph with our chosen animation.
    let (graph, index) = AnimationGraph::from_clip(animation_handle.clone());
    let graph_handle = graphs.add(graph);

    // Make a component with the data we need to play the chosen animation.
    let chosen_animation = ChosenAnimation {
        graph_handle,
        index,
    };

    // Make a component that will load and spawn an instance of our mesh.
    let mesh_scene = SceneRoot(mesh_handle.clone());

    // Spawn an entity with our components and connect it to play_animation_once_loaded.
    commands
        .spawn((mesh_scene, chosen_animation, transform))
        .observe(play_animation_once_loaded);
}

fn setup_meshes_and_animations(
    mut commands: Commands,
    asset_server: Res<AssetServer>,
    mut graphs: ResMut<Assets<AnimationGraph>>,
) {
    // Firstly, spawn a simple mesh with a single animation.
    let simple_path = "models/SimpleSkin/SimpleSkin.gltf";
    let simple_mesh = asset_server.load(GltfAssetLabel::Scene(0).from_asset(simple_path));
    let simple_animation = asset_server.load(GltfAssetLabel::Animation(0).from_asset(simple_path));

    spawn_mesh_with_animation(
        &mut commands,
        &simple_mesh,
        &simple_animation,
        // The simple mesh is bit small, so scale it up using the transform.
        Transform::from_xyz(-140.0, 0.0, 0.0).with_scale(Vec3::splat(30.0)),
        &mut graphs,
    );

    // Secondly, spawn three instances of a fox mesh, each playing a different animation.
    let fox_path = "models/animated/Fox.glb";
    let fox_mesh = asset_server.load(GltfAssetLabel::Scene(0).from_asset(fox_path));
    let fox_survey_animation = asset_server.load(GltfAssetLabel::Animation(0).from_asset(fox_path));
    let fox_walk_animation = asset_server.load(GltfAssetLabel::Animation(1).from_asset(fox_path));
    let fox_run_animation = asset_server.load(GltfAssetLabel::Animation(2).from_asset(fox_path));

    spawn_mesh_with_animation(
        &mut commands,
        &fox_mesh,
        &fox_survey_animation,
        Transform::from_xyz(-40.0, 0.0, 0.0),
        &mut graphs,
    );

    spawn_mesh_with_animation(
        &mut commands,
        &fox_mesh,
        &fox_walk_animation,
        Transform::from_xyz(40.0, 0.0, 0.0),
        &mut graphs,
    );

    spawn_mesh_with_animation(
        &mut commands,
        &fox_mesh,
        &fox_run_animation,
        Transform::from_xyz(120.0, 0.0, 0.0),
        &mut graphs,
    );
}

// Triggered when a mesh instance has loaded and spawned. Plays our chosen animation.
fn play_animation_once_loaded(
    trigger: Trigger<SceneInstanceReady>,
    mut commands: Commands,
    children: Query<&Children>,
    chosen_animations: Query<&ChosenAnimation>,
    mut players: Query<&mut AnimationPlayer>,
) {
    // Find the ChosenAnimation component that we added in spawn_mesh_with_animation.
    if let Ok(chosen_animation) = chosen_animations.get(trigger.target()) {
        // Find all the animation players that were spawned.
        for child in children.iter_descendants(trigger.target()) {
            if let Ok(mut player) = players.get_mut(child) {
                // Tell the player to start the animation and keep repeating it.
                player.play(chosen_animation.index).repeat();

                // Add the animation graph. This connects the player to the mesh.
                commands
                    .entity(child)
                    .insert(AnimationGraphHandle(chosen_animation.graph_handle.clone()));
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
