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
pub struct MeshletDebugMaterialHandle(pub Handle<MeshletDebugMaterial>);

pub fn toggle_meshlet_debug(
    mut commands: Commands,
    debug_material: Res<MeshletDebugMaterialHandle>,
    query: Query<
        (
            Entity,
            Option<&MeshMaterial3d<StandardMaterial>>,
            Option<&StashedStandardMaterial>,
        ),
        With<MeshletMesh3d>,
    >,
) {
    for (entity, standard, stashed) in query {
        if let Some(stashed) = stashed {
            commands
                .entity(entity)
                .remove::<MeshMaterial3d<MeshletDebugMaterial>>()
                .remove::<StashedStandardMaterial>()
                .insert(MeshMaterial3d(stashed.0.clone()));
        }

        if let Some(standard) = standard {
            commands
                .entity(entity)
                .remove::<MeshMaterial3d<StandardMaterial>>()
                .insert(MeshMaterial3d(debug_material.0.clone()))
                .insert(StashedStandardMaterial(standard.0.clone()));
        }
    }
}

fn setup_meshlet_debug(mut commands: Commands, asset_server: Res<AssetServer>) {
    commands.insert_resource(MeshletDebugMaterialHandle(
        asset_server.add(MeshletDebugMaterial::default()),
    ));

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
            toggle_meshlet_debug.run_if(input_just_pressed(KeyCode::KeyM)),
        );
    }
}
