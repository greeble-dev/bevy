//! Meshlet rendering for dense high-poly scenes (experimental).

// Note: This example showcases the meshlet API, but is not the type of scene that would benefit from using meshlets.

use bevy::{
    asset::RenderAssetUsages,
    camera_controller::free_camera::{FreeCamera, FreeCameraPlugin},
    light::{CascadeShadowConfigBuilder, DirectionalLightShadowMap},
    pbr::experimental::meshlet::{MeshletMesh3d, MeshletPlugin},
    prelude::*,
    render::render_resource::{AsBindGroup, Extent3d, TextureDimension, TextureFormat},
};
use std::f32::consts::PI;

const ASSET_URL: &str =
    "https://github.com/bevyengine/bevy_asset_files/raw/6dccaef517bde74d1969734703709aead7211dbc/meshlet/bunny.meshlet_mesh";

fn main() {
    App::new()
        .insert_resource(DirectionalLightShadowMap { size: 4096 })
        .add_plugins((
            DefaultPlugins,
            MeshletPlugin {
                cluster_buffer_slots: 1 << 14,
            },
            MaterialPlugin::<MeshletDebugMaterial>::default(),
            FreeCameraPlugin,
        ))
        .add_systems(Startup, setup)
        .add_systems(Update, bunny_wiggler)
        .run();
}

fn setup(
    mut commands: Commands,
    asset_server: Res<AssetServer>,
    mut standard_materials: ResMut<Assets<StandardMaterial>>,
    mut debug_materials: ResMut<Assets<MeshletDebugMaterial>>,
    mut meshes: ResMut<Assets<Mesh>>,
    mut images: ResMut<Assets<Image>>,
) {
    commands.spawn((
        Camera3d::default(),
        Transform::from_translation(Vec3::new(1.8, 0.4, -0.1)).looking_at(Vec3::ZERO, Vec3::Y),
        Msaa::Off,
        EnvironmentMapLight {
            diffuse_map: asset_server.load("environment_maps/pisa_diffuse_rgb9e5_zstd.ktx2"),
            specular_map: asset_server.load("environment_maps/pisa_specular_rgb9e5_zstd.ktx2"),
            intensity: 150.0,
            ..default()
        },
        FreeCamera::default(),
    ));

    commands.spawn((
        DirectionalLight {
            illuminance: light_consts::lux::FULL_DAYLIGHT,
            shadow_maps_enabled: true,
            ..default()
        },
        CascadeShadowConfigBuilder {
            num_cascades: 1,
            maximum_distance: 15.0,
            ..default()
        }
        .build(),
        Transform::from_rotation(Quat::from_euler(EulerRot::ZYX, 0.0, PI * -0.15, PI * -0.15)),
    ));

    // A custom file format storing a [`bevy_mesh::Mesh`]
    // that has been converted to a [`bevy_pbr::meshlet::MeshletMesh`]
    // using [`bevy_pbr::meshlet::MeshletMesh::from_mesh`], which is
    // a function only available when the `meshlet_processor` cargo feature is enabled.
    let meshlet_mesh_handle = asset_server.load(ASSET_URL);
    let debug_material = debug_materials.add(MeshletDebugMaterial::default());

    for x in -2..=2 {
        let mut bunny = commands.spawn((
            MeshletMesh3d(meshlet_mesh_handle.clone()),
            MeshMaterial3d(standard_materials.add(StandardMaterial {
                base_color: match x {
                    -2 => Srgba::hex("#dc2626").unwrap().into(),
                    -1 => Srgba::hex("#ea580c").unwrap().into(),
                    0 => Srgba::hex("#facc15").unwrap().into(),
                    1 => Srgba::hex("#16a34a").unwrap().into(),
                    2 => Srgba::hex("#0284c7").unwrap().into(),
                    _ => unreachable!(),
                },
                perceptual_roughness: (x + 2) as f32 / 4.0,
                ..default()
            })),
            Transform::default()
                .with_scale(Vec3::splat(0.2))
                .with_translation(Vec3::new(x as f32 / 2.0, 0.0, -0.3)),
        ));
        if x == 1 {
            bunny.insert(BunnyWiggler);
        }
    }
    for x in -2..=2 {
        commands.spawn((
            MeshletMesh3d(meshlet_mesh_handle.clone()),
            MeshMaterial3d(debug_material.clone()),
            Transform::default()
                .with_scale(Vec3::splat(0.2))
                .with_rotation(Quat::from_rotation_y(PI))
                .with_translation(Vec3::new(x as f32 / 2.0, 0.0, 0.3)),
        ));
    }

    let mipmap_material = mipmap_material(&mut standard_materials, &mut images);

    for x in -2..=2 {
        commands.spawn((
            MeshletMesh3d(meshlet_mesh_handle.clone()),
            MeshMaterial3d(mipmap_material.clone()),
            Transform::default()
                .with_scale(Vec3::splat(0.2))
                .with_rotation(Quat::from_rotation_y(PI))
                .with_translation(Vec3::new(x as f32 / 2.0, 0.0, -0.9)),
        ));
    }

    commands.spawn((
        Mesh3d(meshes.add(Plane3d::default().mesh().size(5.0, 5.0))),
        MeshMaterial3d(standard_materials.add(StandardMaterial {
            base_color: Color::WHITE,
            perceptual_roughness: 1.0,
            ..default()
        })),
    ));
}

#[derive(Component)]
struct BunnyWiggler;

fn bunny_wiggler(mut bunny: Query<&mut Transform, With<BunnyWiggler>>, time: Res<Time>) {
    bunny.single_mut().as_deref_mut().unwrap().translation.z +=
        ops::cos(time.elapsed_secs() * 10.0) * 0.003;
}

#[derive(Asset, TypePath, AsBindGroup, Clone, Default)]
struct MeshletDebugMaterial {
    _dummy: (),
}

impl Material for MeshletDebugMaterial {}

fn checkerboard(size: usize, color: Srgba) -> Vec<u8> {
    let color = color.to_u8_array();
    let black = Srgba::BLACK.to_u8_array();
    let d = size.ilog2().saturating_sub(4);

    (0..(size * size))
        .into_iter()
        .flat_map(|i| {
            let x = i.rem_euclid(size) >> d;
            let y = (i / size) >> d;
            if ((x + y) & 1) == 0 {
                color
            } else {
                black
            }
        })
        .collect()
}

fn mipmap_material(
    standard_materials: &mut Assets<StandardMaterial>,
    images: &mut Assets<Image>,
) -> Handle<StandardMaterial> {
    let mip_colors = [
        Srgba::rgb(1.0, 0.0, 0.0),
        Srgba::rgb(0.0, 1.0, 0.0),
        Srgba::rgb(0.0, 0.0, 1.0),
        Srgba::rgb(1.0, 1.0, 0.0),
        Srgba::rgb(0.0, 1.0, 1.0),
        Srgba::rgb(1.0, 0.0, 1.0),
    ];

    let size = 1024;

    let mut image = Image::new_uninit(
        Extent3d {
            width: size as u32,
            height: size as u32,
            depth_or_array_layers: 1,
        },
        TextureDimension::D2,
        TextureFormat::Rgba8UnormSrgb,
        RenderAssetUsages::default(),
    );

    image.texture_descriptor.mip_level_count = mip_colors.len() as u32;

    image.data = Some(
        mip_colors
            .iter()
            .enumerate()
            .flat_map(|(level, color)| checkerboard(size / 2_usize.pow(level as u32), *color))
            .collect(),
    );

    standard_materials.add(StandardMaterial {
        base_color_texture: Some(images.add(image)),
        perceptual_roughness: 1.0,
        ..default()
    })
}
