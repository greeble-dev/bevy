//! Loads and renders a glTF file, but overrides the file's texture samplers.

use bevy::{gltf::GltfLoaderSettings, prelude::*};
use bevy_image::{ImageAddressMode, ImageFilterMode, ImageSamplerDescriptor};

fn main() {
    App::new()
        .insert_resource(VisibleModel(KeyCode::Digit1))
        .add_plugins(DefaultPlugins)
        .add_systems(Startup, setup)
        .add_systems(Update, (keyboard_control, update_visibility, update_camera))
        .run();
}

fn setup(mut commands: Commands, asset_server: Res<AssetServer>) {
    let default = ImageSamplerDescriptor {
        address_mode_u: ImageAddressMode::Repeat,
        address_mode_v: ImageAddressMode::Repeat,
        mag_filter: ImageFilterMode::Linear,
        min_filter: ImageFilterMode::Linear,
        mipmap_filter: ImageFilterMode::Nearest,
        ..Default::default()
    };

    let models: &[(KeyCode, &'static str, ImageSamplerDescriptor)] = &[
        (
            KeyCode::Digit1,
            "1: None",
            ImageSamplerDescriptor {
                lod_max_clamp: 0.0,
                ..default.clone()
            },
        ),
        (
            KeyCode::Digit2,
            "2: Nearest",
            ImageSamplerDescriptor { ..default.clone() },
        ),
        (
            KeyCode::Digit3,
            "3: Linear",
            ImageSamplerDescriptor {
                mipmap_filter: ImageFilterMode::Linear,
                ..default.clone()
            },
        ),
        (
            KeyCode::Digit4,
            "4: Linear, Anisotropic",
            ImageSamplerDescriptor {
                mipmap_filter: ImageFilterMode::Linear,
                anisotropy_clamp: 16,
                ..default.clone()
            },
        ),
    ];

    for (index, (key, label, sampler)) in models.iter().enumerate() {
        let asset = GltfAssetLabel::Scene(0)
            .from_asset(format!("models/checkerboard/checkerboard{index}.gltf"));

        let sampler = sampler.clone();

        let settings = move |settings: &mut GltfLoaderSettings| {
            settings.default_sampler = Some(sampler.clone());
            settings.override_sampler = true;
        };

        commands.spawn((
            SceneRoot(asset_server.load_with_settings(asset, settings)),
            Model { key: *key, label },
        ));
    }

    commands.spawn((
        Camera3d::default(),
        Projection::from(PerspectiveProjection {
            near: 0.001,
            ..Default::default()
        }),
    ));

    commands.spawn((
        Text::default(),
        Node {
            position_type: PositionType::Absolute,
            top: Val::Px(12.0),
            left: Val::Px(12.0),
            ..Default::default()
        },
    ));
}

#[derive(Component)]
struct Model {
    key: KeyCode,
    label: &'static str,
}

#[derive(Resource, PartialEq)]
struct VisibleModel(KeyCode);

fn keyboard_control(
    keyboard_input: Res<ButtonInput<KeyCode>>,
    mut time: ResMut<Time<Virtual>>,
    mut models: Query<&Model>,
    mut visible_model: ResMut<VisibleModel>,
) {
    if keyboard_input.just_pressed(KeyCode::Space) {
        if time.is_paused() {
            time.unpause();
        } else {
            time.pause();
        }
    }

    for model in &mut models {
        if keyboard_input.just_pressed(model.key) {
            *visible_model = VisibleModel(model.key);
        }
    }
}

fn update_visibility(
    mut text: Single<&mut Text>,
    mut models: Query<(&Model, &mut Visibility)>,
    visible_model: Res<VisibleModel>,
) {
    text.clear();
    text.push_str("  Space: Pause\n\n");

    for (model, mut visibility) in &mut models {
        let visible = *visible_model == VisibleModel(model.key);

        *visibility = if visible {
            Visibility::Visible
        } else {
            Visibility::Hidden
        };

        text.push_str(&format!(
            "{}{}{}\n",
            if visible { "> " } else { "  " },
            model.label,
            if visible { " <" } else { "  " },
        ));
    }
}

fn update_camera(time: Res<Time>, mut query: Query<&mut Transform, With<Camera3d>>) {
    for mut transform in &mut query {
        let height = (ops::sin(time.elapsed_secs()) * 0.07) + 0.08;

        *transform =
            Transform::from_xyz(0.2, height, 0.95).looking_at(Vec3::new(0.0, -0.1, 0.0), Vec3::Y);
    }
}
