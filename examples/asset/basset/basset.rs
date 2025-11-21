//! Basset proof of concept.

use bevy::{
    asset::{
        io::Reader, saver::AssetSaver, AssetLoader, AssetPath, ErasedLoadedAsset, LoadContext,
        LoadedAsset,
    },
    camera_controller::free_camera::{FreeCamera, FreeCameraPlugin},
    ecs::error::BevyError,
    light::CascadeShadowConfigBuilder,
    pbr::experimental::meshlet::{
        MeshletMesh, MeshletPlugin, MESHLET_DEFAULT_VERTEX_POSITION_QUANTIZATION_FACTOR,
    },
    prelude::*,
    reflect::TypePath,
    render::render_resource::AsBindGroup,
    time::common_conditions::on_timer,
};
use bevy_asset::{basset::*, AssetAction2, AssetRef};

use core::result::Result;
use serde::{Deserialize, Serialize};
use std::{boxed::Box, sync::Arc, time::Duration};

mod action {
    use super::*;

    pub struct LoadPath;

    #[derive(Serialize, Deserialize, Default)]
    pub struct LoadPathParams {
        pub path: String, // TODO: Should be AssetPath. Avoiding for now to simplify lifetimes.
        #[serde(default)]
        pub loader_settings: Option<Box<ron::value::RawValue>>,
        // TODO
        //loader_name: Option<String>,
    }

    impl BassetAction for LoadPath {
        type Params = LoadPathParams;
        type Error = BevyError;

        async fn apply(
            &self,
            context: &mut ApplyContext<'_>,
            params: &Self::Params,
        ) -> Result<ErasedLoadedAsset, Self::Error> {
            // XXX TODO: Review if treating the asset as a dependee is correct.
            context
                .erased_load_dependee_path(
                    // XXX TODO: Avoid `into_owned`?
                    &AssetPath::parse(&params.path).into_owned(),
                    &params.loader_settings,
                )
                .await
        }
    }

    pub struct JoinStrings;

    #[derive(Serialize, Deserialize, Default)]
    pub struct JoinStringsParams {
        separator: String,
        strings: Vec<AssetRef<'static>>,
    }

    impl BassetAction for JoinStrings {
        type Params = JoinStringsParams;
        type Error = BevyError;

        async fn apply(
            &self,
            context: &mut ApplyContext<'_>,
            params: &Self::Params,
        ) -> Result<ErasedLoadedAsset, Self::Error> {
            let mut strings = Vec::new();

            for path in &params.strings {
                strings.push(context.load_dependee::<demo::StringAsset>(path).await?.0);
            }

            let joined = strings
                .into_iter()
                .reduce(|l, r| l + &params.separator + &r)
                .unwrap_or("".to_owned());

            Ok(LoadedAsset::new_with_dependencies(demo::StringAsset(joined)).into())
        }
    }

    pub struct UppercaseString;

    #[derive(Serialize, Deserialize, Default)]
    pub struct UppercaseStringParams {
        string: AssetRef<'static>,
    }

    impl BassetAction for UppercaseString {
        type Params = UppercaseStringParams;
        type Error = BevyError;

        async fn apply(
            &self,
            context: &mut ApplyContext<'_>,
            params: &Self::Params,
        ) -> Result<ErasedLoadedAsset, Self::Error> {
            let string = demo::StringAsset(
                context
                    .load_dependee::<demo::StringAsset>(&params.string)
                    .await?
                    .0
                    .to_uppercase(),
            );

            Ok(LoadedAsset::new_with_dependencies(string).into())
        }
    }

    /// Creates an `AcmeScene` from a `Gltf`. This does not respect the glTF's
    /// scenes list - it just takes every node.
    pub struct AcmeSceneFromGltf;

    #[derive(Serialize, Deserialize, Default)]
    pub struct AcmeSceneFromGltfParams {
        gltf: AssetRef<'static>,
    }

    impl BassetAction for AcmeSceneFromGltf {
        type Params = AcmeSceneFromGltfParams;
        type Error = BevyError;

        async fn apply(
            &self,
            context: &mut ApplyContext<'_>,
            params: &Self::Params,
        ) -> Result<ErasedLoadedAsset, Self::Error> {
            let gltf = context.erased_load_dependee(&params.gltf).await?;

            let scene = acme::from_gltf(&gltf);

            // XXX TODO: What about dependencies?

            Ok(LoadedAsset::new_with_dependencies(scene).into())
        }
    }

    pub struct MeshletFromMesh;

    #[derive(Serialize, Deserialize, Default)]
    pub struct MeshletFromMeshParams {
        mesh: AssetRef<'static>,
        #[serde(default)]
        vertex_position_quantization_factor: Option<u8>,
    }

    impl MeshletFromMeshParams {
        fn vertex_position_quantization_factor(&self) -> u8 {
            self.vertex_position_quantization_factor
                .unwrap_or(MESHLET_DEFAULT_VERTEX_POSITION_QUANTIZATION_FACTOR)
        }
    }

    impl BassetAction for MeshletFromMesh {
        type Params = MeshletFromMeshParams;
        type Error = BevyError;

        async fn apply(
            &self,
            context: &mut ApplyContext<'_>,
            params: &Self::Params,
        ) -> Result<ErasedLoadedAsset, Self::Error> {
            // TODO: Should we check if `MeshletPlugin` is registered so we can
            // return a sensible error?

            let mesh = context.load_dependee::<Mesh>(&params.mesh).await?;

            let meshlet =
                MeshletMesh::from_mesh(&mesh, params.vertex_position_quantization_factor())?;

            Ok(LoadedAsset::new_with_dependencies(meshlet).into())
        }
    }

    pub struct ConvertAcmeSceneMeshesToMeshlets;

    #[derive(Serialize, Deserialize, Default)]
    pub struct ConvertAcmeSceneMeshesToMeshletsParams {
        scene: AssetRef<'static>,
        #[serde(default)]
        vertex_position_quantization_factor: Option<u8>,
    }

    impl BassetAction for ConvertAcmeSceneMeshesToMeshlets {
        type Params = ConvertAcmeSceneMeshesToMeshletsParams;
        type Error = BevyError;

        async fn apply(
            &self,
            context: &mut ApplyContext<'_>,
            params: &Self::Params,
        ) -> Result<ErasedLoadedAsset, Self::Error> {
            // TODO: Should we check if `MeshletPlugin` is registered so we can
            // return a sensible error?

            let mut scene = context
                .load_dependee::<acme::AcmeScene>(&params.scene)
                .await?;

            for entity in &mut scene.entities {
                if let Some(mesh) = entity.mesh.take() {
                    entity.meshlet_mesh = Some(acme::AcmeMeshletMesh {
                        asset: AssetRef::Action(make_action::<MeshletFromMesh>(
                            &MeshletFromMeshParams {
                                mesh: mesh.asset,
                                vertex_position_quantization_factor: params
                                    .vertex_position_quantization_factor,
                            },
                        )),
                    });
                }
            }

            Ok(LoadedAsset::new_with_dependencies(scene).into())
        }
    }
}

mod demo {
    use bevy_asset::{io::Writer, saver::SavedAsset, AsyncWriteExt};

    use super::*;

    #[derive(Asset, TypePath, Debug)]
    pub struct StringAsset(pub String);

    // TODO: Delete? Originally used to confirm that loader settings work. Probably
    // redundant now?
    #[derive(Serialize, Deserialize, Default)]
    pub struct StringAssetSettings {
        uppercase: bool,
    }

    #[derive(Default)]
    pub struct StringAssetLoader;

    impl AssetLoader for StringAssetLoader {
        type Asset = StringAsset;
        type Settings = StringAssetSettings;
        type Error = std::io::Error;

        async fn load(
            &self,
            reader: &mut dyn Reader,
            settings: &Self::Settings,
            _load_context: &mut LoadContext<'_>,
        ) -> Result<StringAsset, Self::Error> {
            let mut bytes = Vec::new();
            reader.read_to_end(&mut bytes).await?;

            let mut string = String::from_utf8(bytes).expect("TODO");

            if settings.uppercase {
                string = string.to_uppercase();
            }

            Ok(StringAsset(string))
        }

        fn extensions(&self) -> &[&str] {
            &["string"]
        }
    }

    #[derive(Default)]
    pub struct StringAssetSaver;

    impl AssetSaver for StringAssetSaver {
        type Asset = StringAsset;
        type Settings = ();
        type OutputLoader = StringAssetLoader;
        type Error = std::io::Error;

        async fn save(
            &self,
            writer: &mut Writer,
            asset: SavedAsset<'_, Self::Asset>,
            _settings: &Self::Settings,
        ) -> Result<StringAssetSettings, Self::Error> {
            writer.write_all(asset.0.as_bytes()).await?;

            Ok(StringAssetSettings::default())
        }
    }

    #[derive(Asset, TypePath, Debug)]
    pub struct IntAsset(pub i64);

    #[derive(Default)]
    pub struct IntAssetLoader;

    impl AssetLoader for IntAssetLoader {
        type Asset = IntAsset;
        type Settings = ();
        type Error = std::io::Error;

        async fn load(
            &self,
            reader: &mut dyn Reader,
            _: &Self::Settings,
            _load_context: &mut LoadContext<'_>,
        ) -> Result<IntAsset, Self::Error> {
            let mut bytes = Vec::new();
            reader.read_to_end(&mut bytes).await?;
            // TODO: Error handling.
            Ok(IntAsset(
                String::from_utf8(bytes).unwrap().parse::<i64>().unwrap(),
            ))
        }

        fn extensions(&self) -> &[&str] {
            &["int"]
        }
    }

    #[derive(Default)]
    pub struct IntAssetSaver;

    impl AssetSaver for IntAssetSaver {
        type Asset = IntAsset;
        type Settings = ();
        type OutputLoader = IntAssetLoader;
        type Error = std::io::Error;

        async fn save(
            &self,
            writer: &mut Writer,
            asset: SavedAsset<'_, Self::Asset>,
            _settings: &Self::Settings,
        ) -> Result<(), Self::Error> {
            writer.write_all(asset.0.to_string().as_bytes()).await?;

            Ok(())
        }
    }
}

mod acme {
    use super::*;
    use bevy::pbr::experimental::meshlet::MeshletMesh3d;

    #[derive(Serialize, Deserialize, Debug)]
    pub struct AcmeMesh {
        pub asset: AssetRef<'static>,
    }

    #[derive(Serialize, Deserialize, Debug)]
    pub struct AcmeMeshletMesh {
        pub asset: AssetRef<'static>,
    }

    #[derive(Serialize, Deserialize, Debug)]
    pub struct AcmeMaterial {
        pub base_color_texture: Option<AssetRef<'static>>,
    }

    #[derive(Serialize, Deserialize, Default, Debug)]
    pub struct AcmeEntity {
        #[serde(default)]
        pub transform: Transform,

        #[serde(default)]
        pub mesh: Option<AcmeMesh>,

        #[serde(default)]
        pub meshlet_mesh: Option<AcmeMeshletMesh>,

        #[serde(default)]
        pub material: Option<AcmeMaterial>,
    }

    #[derive(Asset, TypePath, Serialize, Deserialize, Default, Debug)]
    pub struct AcmeScene {
        pub entities: Vec<AcmeEntity>,
    }

    fn get_sub_asset<'a, T: Asset>(
        asset: &'a ErasedLoadedAsset,
        sub_asset_handle: &Handle<T>,
    ) -> &'a T {
        asset
            .get_labeled_by_id(sub_asset_handle.into())
            .expect("TODO")
            .get::<T>()
            .expect("TODO")
    }

    pub fn from_gltf(asset: &ErasedLoadedAsset) -> AcmeScene {
        let mut entities = Vec::<AcmeEntity>::new();

        let gltf = asset.get::<Gltf>().expect("TODO");

        let mut stack = gltf
            .nodes
            .iter()
            .map(|node_handle| {
                let node = get_sub_asset(asset, node_handle);

                (node, node.transform)
            })
            .collect::<Vec<_>>();

        loop {
            let Some((node, transform)) = stack.pop() else {
                break;
            };

            if let Some(mesh) = node
                .mesh
                .as_ref()
                .map(|mesh_handle| get_sub_asset(asset, mesh_handle))
            {
                for primitive in mesh.primitives.iter() {
                    let mesh = Some(AcmeMesh {
                        asset: primitive.mesh.path().expect("TODO").clone(),
                    });

                    let standard_material =
                        get_sub_asset(asset, primitive.material.as_ref().expect("TODO"));

                    let material = Some(AcmeMaterial {
                        base_color_texture: standard_material
                            .base_color_texture
                            .clone()
                            .map(|p| p.path().expect("TODO").clone()),
                    });

                    entities.push(AcmeEntity {
                        transform,
                        mesh,
                        material,
                        ..Default::default()
                    });
                }
            }

            for child in node
                .children
                .iter()
                .map(|child_handle| get_sub_asset(asset, child_handle))
            {
                stack.push((child, transform * child.transform));
            }
        }

        AcmeScene { entities }
    }

    pub fn spawn(
        commands: &mut Commands,
        scene: &AcmeScene,
        parent_entity: Option<Entity>,
        asset_server: &AssetServer,
        standard_material_assets: &mut Assets<StandardMaterial>,
        meshlet_debug_material_assets: &mut Assets<MeshletDebugMaterial>,
    ) {
        for scene_entity in &scene.entities {
            let mut world_entity = commands.spawn(scene_entity.transform);

            // XXX TODO: Currently this forces the debug material for meshlets.
            // Should change that to be a scene conversion action. AcmeMaterial
            // will become an enum of standard/debug materials.

            if scene_entity.meshlet_mesh.is_some() {
                world_entity.insert(MeshMaterial3d(
                    meshlet_debug_material_assets.add(MeshletDebugMaterial::default()),
                ));
            } else if let Some(material) = &scene_entity.material {
                let standard_material = StandardMaterial {
                    base_color_texture: material
                        .base_color_texture
                        .as_ref()
                        .map(|p| asset_server.load::<Image>(p)),
                    ..Default::default()
                };

                world_entity.insert(MeshMaterial3d(
                    standard_material_assets.add(standard_material),
                ));
            }

            if let Some(mesh) = &scene_entity.mesh {
                world_entity.insert(Mesh3d(asset_server.load::<Mesh>(&mesh.asset)));
            }

            if let Some(meshlet_mesh) = &scene_entity.meshlet_mesh {
                world_entity.insert(MeshletMesh3d(
                    asset_server.load::<MeshletMesh>(&meshlet_mesh.asset),
                ));
            }

            if let Some(parent_entity) = parent_entity {
                world_entity.insert(ChildOf(parent_entity));
            }
        }
    }

    #[derive(Component)]
    pub struct AcmeSceneSpawner(pub Handle<AcmeScene>);

    // XXX TODO: This is currently used to keep the handle alive long enough that
    // the asset loaded events can be printed. Rethink?
    #[derive(Component)]
    #[expect(dead_code, reason = "TODO")]
    pub struct AcmeSceneInstance(pub Handle<AcmeScene>);

    pub fn tick_scene_spawners(
        mut commands: Commands,
        spawners: Query<(Entity, &AcmeSceneSpawner)>,
        scene_assets: Res<Assets<AcmeScene>>,
        asset_server: Res<AssetServer>,
        mut standard_material_assets: ResMut<Assets<StandardMaterial>>,
        mut meshlet_debug_material_assets: ResMut<Assets<MeshletDebugMaterial>>,
    ) {
        for (entity, spawner) in spawners {
            let Some(scene_asset) = scene_assets.get(&spawner.0) else {
                continue;
            };

            commands
                .entity(entity)
                .insert(AcmeSceneInstance(spawner.0.clone()));

            commands.entity(entity).remove::<AcmeSceneSpawner>();

            spawn(
                &mut commands,
                scene_asset,
                Some(entity),
                &asset_server,
                &mut standard_material_assets,
                &mut meshlet_debug_material_assets,
            );
        }
    }
}

#[derive(Asset, TypePath, AsBindGroup, Clone, Default)]
struct MeshletDebugMaterial {
    _dummy: (),
}

impl Material for MeshletDebugMaterial {}

#[derive(Resource)]
struct Handles(Vec<UntypedHandle>);

fn setup(
    mut commands: Commands,
    asset_server: Res<AssetServer>,
    mut meshes: ResMut<Assets<Mesh>>,
    mut materials: ResMut<Assets<StandardMaterial>>,
) {
    // XXX TODO
    //let _inline_path = BassetPathSerializable::Path("1234.int".into()).to_asset_path();

    commands.insert_resource(Handles(vec![
        asset_server
            .load::<demo::StringAsset>("hello.string")
            .untyped(),
        asset_server
            .load::<demo::StringAsset>("world.string")
            .untyped(),
        asset_server.load::<demo::IntAsset>("1234.int").untyped(),
        asset_server.load::<demo::IntAsset>("int.basset").untyped(),
        asset_server
            .load::<demo::StringAsset>("string.basset")
            .untyped(),
        // Disabled until we can work out how apply_settings works with dynamic types.
        /*
        asset_server
            .load::<demo::StringAsset>("string_loader_uppercase.basset")
            .untyped(),
            */
        asset_server
            .load::<demo::StringAsset>("join_strings.basset")
            .untyped(),
        //asset_server.load::<demo::IntAsset>(inline_path).untyped(),
    ]));

    commands.spawn((
        acme::AcmeSceneSpawner(asset_server.load::<acme::AcmeScene>("scene_from_gltf.basset")),
        Transform::from_xyz(-100.0, 0.0, 0.0)
            .looking_to(Dir3::new(vec3(1.0, 0.0, 2.0)).unwrap(), Vec3::Y),
    ));

    commands.spawn((
        acme::AcmeSceneSpawner(asset_server.load::<acme::AcmeScene>("meshlet_scene.basset")),
        Transform::from_xyz(100.0, 0.0, 0.0)
            .looking_to(Dir3::new(vec3(1.0, 0.0, 2.0)).unwrap(), Vec3::Y),
    ));

    commands.spawn((
        Camera3d::default(),
        Transform::from_xyz(-30.0, 200.0, 300.0).looking_at(Vec3::new(-30.0, 75.0, 0.0), Vec3::Y),
        FreeCamera {
            walk_speed: 200.0,
            ..Default::default()
        },
        // Meshlets are incompatible with MSAA.
        #[cfg(feature = "meshlet")]
        Msaa::Off,
    ));

    commands.spawn((
        Mesh3d(meshes.add(Plane3d::default().mesh().size(500000.0, 500000.0))),
        MeshMaterial3d(materials.add(Color::srgb(0.3, 0.5, 0.3))),
    ));

    commands.spawn((
        Transform::from_xyz(100.0, 200.0, 200.0).looking_at(Vec3::new(0.0, 0.0, 0.0), Vec3::Y),
        DirectionalLight {
            shadows_enabled: true,
            ..default()
        },
        CascadeShadowConfigBuilder {
            num_cascades: 1,
            maximum_distance: 600.0,
            ..default()
        }
        .build(),
    ));
}

fn print_events<T: Asset + std::fmt::Debug>(
    asset_server: &AssetServer,
    assets: &Assets<T>,
    events: &mut MessageReader<AssetEvent<T>>,
) {
    for event in events.read() {
        match *event {
            AssetEvent::Added { id } | AssetEvent::Modified { id } => {
                info!(
                    "{:?}: Value = {:?}",
                    asset_server.get_path(id).unwrap(),
                    assets.get(id).unwrap()
                );
            }
            _ => (),
        }
    }
}

fn print(
    asset_server: Res<AssetServer>,
    string_assets: Res<Assets<demo::StringAsset>>,
    int_assets: Res<Assets<demo::IntAsset>>,
    scene_assets: Res<Assets<acme::AcmeScene>>,
    mut string_events: MessageReader<AssetEvent<demo::StringAsset>>,
    mut int_events: MessageReader<AssetEvent<demo::IntAsset>>,
    mut scene_events: MessageReader<AssetEvent<acme::AcmeScene>>,
) {
    print_events(&asset_server, &string_assets, &mut string_events);
    print_events(&asset_server, &int_assets, &mut int_events);
    print_events(&asset_server, &scene_assets, &mut scene_events);
}

fn reload(asset_server: Res<AssetServer>, handles: Res<Handles>, mut done: Local<bool>) {
    if *done {
        return;
    }

    *done = true;

    info!("RELOADING");

    for handle in &handles.0 {
        asset_server.reload(handle.path().expect("TODO"));
    }
}

fn make_action<A: BassetAction>(params: &<A as BassetAction>::Params) -> AssetAction2<'static> {
    AssetAction2::new(
        core::any::type_name::<A>().into(),
        ron::value::RawValue::from_rust(params).expect("TODO"),
        None,
    )
}

fn main() {
    dbg!(ron::ser::to_string(&AssetRef::Path("asdf.txt".into())).expect("TODO"));
    dbg!(
        ron::ser::to_string(&AssetRef::Action(make_action::<action::LoadPath>(
            &action::LoadPathParams {
                path: "asdf.txt".into(),
                ..Default::default()
            }
        )))
        .expect("TODO")
    );
    dbg!(ron::de::from_str::<AssetRef>("Path(\"asdf.txt\")").expect("TODO"));
    dbg!(ron::de::from_str::<AssetRef>("Action((name: \"foo\", params: ()))").expect("TODO"));

    dbg!(serde_json::ser::to_string(&AssetRef::Path("asdf.txt".into())).expect("TODO"));
    dbg!(
        serde_json::ser::to_string(&AssetRef::Action(make_action::<action::LoadPath>(
            &action::LoadPathParams {
                path: "asdf.txt".into(),
                ..Default::default()
            }
        )))
        .expect("TODO")
    );

    let basset_shared = Arc::new(
        BassetShared::new(Some("examples/asset/basset/cache".into()))
            .with_action(action::LoadPath)
            .with_action(action::JoinStrings)
            .with_action(action::UppercaseString)
            .with_action(action::AcmeSceneFromGltf)
            .with_action(action::MeshletFromMesh)
            .with_action(action::ConvertAcmeSceneMeshesToMeshlets)
            .with_saver(demo::StringAssetSaver)
            .with_saver(demo::IntAssetSaver),
    );

    App::new()
        .add_plugins((
            DefaultPlugins.set(AssetPlugin {
                file_path: "examples/asset/basset/assets".to_string(),
                basset_shared,
                ..default()
            }),
            BassetPlugin,
            MaterialPlugin::<MeshletDebugMaterial>::default(),
            FreeCameraPlugin,
            MeshletPlugin {
                cluster_buffer_slots: 1 << 14,
            },
        ))
        .init_asset::<demo::StringAsset>()
        .init_asset::<demo::IntAsset>()
        .init_asset::<acme::AcmeScene>()
        .register_asset_loader(demo::StringAssetLoader)
        .register_asset_loader(demo::IntAssetLoader)
        .add_systems(Startup, setup)
        .add_systems(Update, print)
        .add_systems(Update, reload.run_if(on_timer(Duration::from_secs(2))))
        .add_systems(Update, acme::tick_scene_spawners)
        .run();
}
