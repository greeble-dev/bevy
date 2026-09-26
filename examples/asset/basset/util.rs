use bevy::{
    input::common_conditions::input_just_pressed, pbr::experimental::meshlet::*, prelude::*,
    reflect::TypePath, render::render_resource::AsBindGroup,
};

#[derive(Asset, TypePath, AsBindGroup, Clone, Default)]
pub struct MeshletDebugMaterial {
    _dummy: (),
}

impl Material for MeshletDebugMaterial {}

#[derive(Component, Clone)]
pub struct StashedStandardMaterial(pub Handle<StandardMaterial>);

#[derive(Resource)]
pub struct MeshletDebug {
    pub material: Handle<MeshletDebugMaterial>,
    enabled: bool,
}

impl MeshletDebug {
    fn new(material: Handle<MeshletDebugMaterial>) -> Self {
        Self {
            material,
            enabled: false,
        }
    }
}

pub fn toggle_meshlet_debug(mut debug: ResMut<MeshletDebug>) {
    debug.enabled = !debug.enabled;
}

pub fn update_meshlet_debug(
    mut commands: Commands,
    debug: Res<MeshletDebug>,
    enabled: Query<(Entity, &StashedStandardMaterial), With<MeshletMesh3d>>,
    disabled: Query<(Entity, &MeshMaterial3d<StandardMaterial>), With<MeshletMesh3d>>,
) {
    if debug.enabled {
        for (entity, unstashed) in disabled {
            commands
                .entity(entity)
                .remove::<MeshMaterial3d<StandardMaterial>>()
                .insert(MeshMaterial3d(debug.material.clone()))
                .insert(StashedStandardMaterial(unstashed.0.clone()));
        }
    } else {
        for (entity, stashed) in enabled {
            commands
                .entity(entity)
                .remove::<MeshMaterial3d<MeshletDebugMaterial>>()
                .remove::<StashedStandardMaterial>()
                .insert(MeshMaterial3d(stashed.0.clone()));
        }
    }
}

fn setup_meshlet_debug(mut commands: Commands, asset_server: Res<AssetServer>) {
    commands.insert_resource(MeshletDebug::new(
        asset_server.add(MeshletDebugMaterial::default()),
    ));

    // XXX TODO: Make this optional. Scene viewer shouldn't use it.
    commands.spawn((
        Text::new("M: Toggle meshlet debug"),
        TextFont::from_font_size(FontSize::Px(12.0)),
        Node {
            position_type: PositionType::Absolute,
            top: px(12),
            left: px(12),
            ..Default::default()
        },
    ));
}

pub struct MeshletDebugPlugin;

impl Plugin for MeshletDebugPlugin {
    fn build(&self, app: &mut App) {
        app.add_plugins(MaterialPlugin::<MeshletDebugMaterial>::default());

        app.add_systems(Startup, setup_meshlet_debug).add_systems(
            Update,
            (
                toggle_meshlet_debug.run_if(input_just_pressed(KeyCode::KeyM)),
                update_meshlet_debug,
            ),
        );
    }
}
