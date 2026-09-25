//! Basset proof of concept.

// XXX TODO: Remove when no longer needed.
#![allow(clippy::allow_attributes, reason = "XXX TODO")]

#[path = "action.rs"]
mod action;
use self::action::*;

#[path = "asset.rs"]
mod asset;
use self::asset::*;

#[path = "util.rs"]
mod util;
use self::util::*;

use argh::FromArgs;
use bevy::{
    asset::{
        asset_template,
        basset::{
            publisher::{published_asset_source, read_pack_file, PublishDependency, PublishInput},
            *,
        },
        io::AssetSourceId,
        AssetRef,
    },
    camera_controller::free_camera::{FreeCamera, FreeCameraPlugin},
    ecs::template::TemplateAssetDependencies,
    light::CascadeShadowConfigBuilder,
    log::LogPlugin,
    pbr::{experimental::meshlet::*, StandardMaterialTemplate},
    prelude::*,
    reflect::TypeRegistryArc,
    scene::{ResolvedSceneListRoot, SceneDependencies, ScenePatch},
    tasks::block_on,
    time::common_conditions::on_timer,
};
use core::{any::TypeId, ops::Deref, result::Result};
use std::{path::PathBuf, str::FromStr, sync::Arc, time::Duration};

#[derive(Resource)]
struct AssetHandles(Vec<UntypedHandle>);

#[derive(Resource)]
struct AssetPaths {
    regular: Vec<(TypeId, AssetRef<'static>)>,
    scenes: Vec<(AssetRef<'static>, Transform)>,
    dynamic_scenes: Vec<(AssetRef<'static>, Transform)>,
    // XXX TODO: Maybe better to store as functions to scenes? Then we can don't
    // have to consume them (since `spawn_scene` consumes the `Scene`).
    bsns: Vec<Box<dyn SceneList>>,
}

impl AssetPaths {
    fn publish(self, asset_server: &AssetServer) -> Vec<PublishDependency> {
        let bsns = self
            .bsns
            .into_iter()
            .flat_map(|bsn| bsn_dependencies(bsn, asset_server))
            .collect::<Vec<_>>();

        self.regular
            .into_iter()
            .map(|(_, path)| path)
            .chain(self.scenes.into_iter().map(|(path, _)| path))
            .chain(self.dynamic_scenes.into_iter().map(|(path, _)| path))
            .chain(bsns)
            .map(|path| PublishDependency::Load(RootAssetRef::without_label(path)))
            .collect()
    }
}

fn bsn_dependencies(
    scene: Box<dyn SceneList>,
    asset_server: &AssetServer,
) -> Vec<AssetRef<'static>> {
    let mut dependencies = Vec::<AssetRef<'static>>::new();

    let mut scene_dependencies = SceneDependencies::default();

    scene.register_dependencies(&mut scene_dependencies);

    dependencies.extend(
        scene_dependencies
            .iter()
            .map(|scene_dependency| scene_dependency.path.clone()),
    );

    // XXX TODO: Review scene patch parameter.
    let resolved_root =
        ResolvedSceneListRoot::resolve(scene, asset_server, &Assets::<ScenePatch>::default())
            .expect("XXX TODO");

    let mut template_dependencies = TemplateAssetDependencies::new();

    for resolved_scene in resolved_root.scenes.iter() {
        // XXX TODO: We need to check more than `ResolvedScene::component_templates`.
        // There's various other templates in `ResolvedScene`.
        for component_template in resolved_scene.component_templates.iter() {
            (&**component_template).asset_dependencies(&mut template_dependencies);
        }
    }

    dependencies.extend(template_dependencies.into_iter().map(|d| {
        if let Some(path) = d.downcast_ref::<AssetRef<'static>>() {
            path.clone()
        } else {
            // XXX TODO: Proper error handling.
            panic!("Unexpected dependency type");
        }
    }));

    dependencies
}

#[allow(unused, reason = "XXX TODO")]
const INLINE_JOIN_STRINGS_RON: &str = r#"
(
    separator: ", ",
    strings: [
        Action((
            name: "basset::UppercaseString",
            action: (
                string: Path("hello.string"),
            )
        )),
        Path("world.string"),
    ],
)
"#;

fn setup(
    mut commands: Commands,
    asset_server: Res<AssetServer>,
    mut asset_paths: ResMut<AssetPaths>,
    mut meshes: ResMut<Assets<Mesh>>,
    mut materials: ResMut<Assets<StandardMaterial>>,
) {
    commands.insert_resource(AssetHandles(
        asset_paths
            .regular
            .iter()
            .map(|(type_id, path)| asset_server.load_builder().load_erased(*type_id, path))
            .collect(),
    ));

    for (path, transform) in &asset_paths.scenes {
        commands.spawn((
            WorldAssetRoot(asset_server.load::<WorldAsset>(path.clone())),
            *transform,
        ));
    }

    for (path, transform) in &asset_paths.dynamic_scenes {
        commands.spawn((
            DynamicWorldRoot(asset_server.load::<DynamicWorld>(path.clone())),
            *transform,
        ));
    }

    for scene in std::mem::take(&mut asset_paths.bsns) {
        commands.spawn_scene_list(scene);
    }

    commands.spawn((
        Camera3d::default(),
        Transform::from_xyz(-0.4, 3.5, 5.0).looking_at(Vec3::new(-0.3, 0.0, 0.0), Vec3::Y),
        FreeCamera {
            walk_speed: 2.0,
            ..Default::default()
        },
        // Meshlets are incompatible with MSAA.
        #[cfg(feature = "meshlet")]
        Msaa::Off,
    ));

    commands.spawn((
        Mesh3d(meshes.add(Plane3d::default().mesh().size(5000.0, 5000.0))),
        MeshMaterial3d(materials.add(Color::srgb(0.3, 0.5, 0.3))),
    ));

    commands.spawn((
        Transform::from_xyz(1.0, 2.0, 2.0).looking_at(Vec3::new(0.0, 0.0, 0.0), Vec3::Y),
        DirectionalLight {
            shadow_maps_enabled: true,
            ..default()
        },
        CascadeShadowConfigBuilder {
            num_cascades: 1,
            maximum_distance: 20.0,
            ..default()
        }
        .build(),
    ));
}

fn print_events<T: Asset + std::fmt::Debug>(
    asset_server: &AssetServer,
    assets: &Assets<T>,
    events: &mut MessageReader<AssetEvent<T>>,
    print_value: bool,
) {
    for event in events.read() {
        match *event {
            AssetEvent::Added { id } | AssetEvent::Modified { id } => {
                // XXX TODO: Do something if path is `None`?
                if let Some(path) = asset_server.get_path(id)
                    && !path.to_string().starts_with("embedded://")
                {
                    let value = assets.get(id).unwrap();

                    if print_value {
                        info!(target: "load_events", ?path, ?value, "Loaded");
                    } else {
                        info!(target: "load_events", ?path, "Loaded");
                    }
                }
            }
            _ => (),
        }
    }
}

// XXX TODO: Annoying boilerplate. Isn't there an easier way to track all asset
// loads?
fn print(
    asset_server: Res<AssetServer>,
    string_assets: Res<Assets<StringAsset>>,
    int_assets: Res<Assets<IntAsset>>,
    image_assets: Res<Assets<Image>>,
    gltf_assets: Res<Assets<Gltf>>,
    mut string_events: MessageReader<AssetEvent<StringAsset>>,
    mut int_events: MessageReader<AssetEvent<IntAsset>>,
    mut image_events: MessageReader<AssetEvent<Image>>,
    mut gltf_events: MessageReader<AssetEvent<Gltf>>,
) {
    print_events(&asset_server, &string_assets, &mut string_events, true);
    print_events(&asset_server, &int_assets, &mut int_events, true);
    print_events(&asset_server, &image_assets, &mut image_events, false);
    print_events(&asset_server, &gltf_assets, &mut gltf_events, false);
}

// XXX TODO: The `done` is annoying. Better way to run once?
fn reload(
    mut done: Local<bool>,
    args: Res<Args>,
    asset_server: Res<AssetServer>,
    handles: Res<AssetHandles>,
) {
    if *done {
        return;
    }

    *done = true;

    if args.reload {
        info!("RELOADING");

        for handle in &handles.0 {
            asset_server.reload(handle.path().expect("TODO"));
        }
    }
}

// XXX TODO: The `done` is annoying. Better way to run once?
fn dump(mut done: Local<bool>, args: Res<Args>, asset_server: Res<AssetServer>) {
    if *done {
        return;
    }

    *done = true;

    if args.dump_dependency_graph {
        asset_server.basset_action_source().dump_dependency_graph();
    }
}

#[derive(PartialEq)]
enum ArgMode {
    Development,
    Publish,
    Published,
}

impl FromStr for ArgMode {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "development" => Ok(Self::Development),
            "publish" => Ok(Self::Publish),
            "published" => Ok(Self::Published),
            _ => Err("must be 'development', 'publish', or 'published'".into()),
        }
    }
}

/// XXX TODO
#[derive(FromArgs, Resource)]
struct Args {
    /// XXX TODO
    #[argh(switch)]
    validate_dependency_cache: bool,

    /// XXX TODO
    #[argh(switch)]
    validate_action_cache: bool,

    /// XXX TODO
    #[argh(switch)]
    dump_dependency_graph: bool,

    /// XXX TODO
    #[argh(switch)]
    reload: bool,

    /// XXX TODO
    #[argh(option, default = "ArgMode::Development")]
    mode: ArgMode,
}

fn test_serialization() {
    // let mut registry = TypeRegistry::default();
    // registry.register::<AssetRef<'static>>();
    // registry.register::<LoadPath>();

    // {
    //     use ron::{de, ser};

    //     let a = dbg!(ser::to_string(&ReflectSerializer::new(
    //         &AssetRef::from(AssetPath::parse("asdf.txt")),
    //         &registry
    //     ))
    //     .expect("TODO"));

    //     dbg!(ReflectDeserializer::new(&registry)
    //         .deserialize(&mut de::Deserializer::from_str(&a).expect("XXX TODO"))
    //         .expect("XXX TODO")
    //         .try_take::<AssetRef>()
    //         .expect("XXX TODO"));

    //     let b = dbg!(ser::to_string(&ReflectSerializer::new(
    //         &AssetRef::new_with_label(
    //             LoadPath {
    //                 path: "asdf.txt".try_into().expect("XXX TODO"),
    //                 ..Default::default()
    //             },
    //             Some("subasset".into()),
    //         ),
    //         &registry
    //     ))
    //     .expect("TODO"));

    //     dbg!(ReflectDeserializer::new(&registry)
    //         .deserialize(&mut de::Deserializer::from_str(&b).expect("XXX TODO"))
    //         .expect("XXX TODO")
    //         .try_take::<AssetRef>()
    //         .expect("XXX TODO"));
    // }

    // #[derive(Serialize, Deserialize)]
    // struct Foo {
    //     i: usize,
    //     s: Box<ron::value::RawValue>,
    // }

    // {
    //     use serde_json::ser;

    //     let _ = dbg!(ser::to_string(&Foo {
    //         i: 123,
    //         s: ron::value::RawValue::from_boxed_ron("456".into()).unwrap()
    //     }));
    // }
}

fn test_reflect_serialization(_registry: TypeRegistryArc, _asset_server: AssetServer) {
    // use io::{VecReader};
    // use tasks::{IoTaskPool};

    // let task = IoTaskPool::get().spawn(async move {
    //     let saver = RonAssetSaver::new(registry.clone());
    //     let loader = RonAssetLoader::new(registry.clone());

    //     let material_original = StandardMaterial::from(Color::srgba(1.0, 2.0, 3.0, 1.0));

    //     let mut material_ron = Vec::<u8>::new();
    //     saver
    //         .save(
    //             &mut material_ron,
    //             &SavedAsset::from_asset(&material_original).upcast(),
    //             &(),
    //             AssetPath::default(),
    //         )
    //         .await
    //         .unwrap();

    //     std::dbg!(String::from_utf8(material_ron.clone()).unwrap());

    //     let material_loaded = loader
    //         .load(
    //             &mut VecReader::new(material_ron),
    //             &(),
    //             LoadContext::new(&asset_server, AssetPath::default(), true, false, None),
    //         )
    //         .await
    //         .unwrap()
    //         .take::<StandardMaterial>()
    //         .unwrap();

    //     std::dbg!(&material_loaded);
    // });

    // task.detach();
}

fn main() {
    #[cfg(not(target_arch = "wasm32"))]
    let args: Args = argh::from_env();
    #[cfg(target_arch = "wasm32")]
    let args = Args::from_args(&[], &[]).unwrap();

    test_serialization();

    let asset_paths = AssetPaths {
        regular: vec![
            // (TypeId::of::<StringAsset>(), "hello.string".into()),
            // (TypeId::of::<StringAsset>(), "world.string".into()),
            // (TypeId::of::<IntAsset>(), "1234.int".into()),
            // (TypeId::of::<IntAsset>(), "int.basset".into()),
            // (TypeId::of::<StringAsset>(), "string.basset".into()),
            // (
            //     TypeId::of::<StringAsset>(),
            //     "string_loader_uppercase.basset".into(),
            // ),
            // (
            //     TypeId::of::<StringAsset>(),
            //     "join_strings.basset".into(),
            // ),
            // (
            //     TypeId::of::<StringAsset>(),
            //     AssetAction2::new(
            //         "basset::JoinStrings".into(),
            //         ron::value::RawValue::from_boxed_ron(INLINE_JOIN_STRINGS_RON.into()).unwrap(),
            //         None,
            //     )
            //     .into(),
            // ),
        ],
        scenes: vec![
            // (
            //     "scene_from_gltf.basset".into(),
            //     Transform::IDENTITY.looking_to(Dir3::new(vec3(1.0, 0.0, 2.0)).unwrap(), Vec3::Y),
            // ),
            (
                "Duck.glb#Scene0".into(),
                Transform::from_xyz(-2.0, 0.0, 0.0)
                    .looking_to(Dir3::new(vec3(1.0, 0.0, 2.0)).unwrap(), Vec3::Y),
            ),
        ],
        dynamic_scenes: vec![
            (
                OptimizeScene {
                    scene: "Duck.glb#Scene0".into(),
                    convert_meshes_to_meshlets: true,
                    compress_textures: true,
                    ..Default::default()
                }
                .into(),
                Transform::IDENTITY.looking_to(Dir3::new(vec3(1.0, 0.0, 2.0)).unwrap(), Vec3::Y),
            ),
            // (
            //     "meshlet_scene.basset".into(),
            //     Transform::from_xyz(2.0, 0.0, 0.0)
            //         .looking_to(Dir3::new(vec3(1.0, 0.0, 2.0)).unwrap(), Vec3::Y),
            // ),
            (
                "gltf_scene_with_external_textures.basset".into(),
                Transform::from_xyz(1.8, 0.75 * 0.5, 1.5)
                    //                    .looking_to(Dir3::new(vec3(1.0, 0.0, 2.0)).unwrap(), Vec3::Y)
                    .with_scale(vec3(0.75, 0.75, 0.75)),
            ),
        ],
        bsns: vec![
            Box::new(bsn! {
                MeshletMesh3d(MeshletFromMesh::new(
                    MeshFromHeightmap::new(
                        ResizeImage::new("heightmaps/Heightmap_08_Island_512.png", 0.5)
                    )
                ))
                MeshMaterial3d<StandardMaterial>(asset_template(StandardMaterialTemplate {
                    base_color_texture: Some(CompressImage::new(
                        ColorizeHeightmap::new("heightmaps/Heightmap_08_Island_512.png")
                    ).into()).into(),
                    perceptual_roughness: 0.9,
                    ..Default::default()
                }))
                Transform::from_xyz(-2.0, 0.1, 1.5).with_scale(vec3(0.75, 1.0, 0.75))
            }),
            Box::new(bsn! {
                MeshletMesh3d(MeshletFromMesh::new("Duck.glb#Mesh0/Primitive0"))
                MeshMaterial3d<StandardMaterial>("Duck.glb#Material0/std")
                Transform::from_xyz(2.0, 0.0, 0.0).looking_to(Dir3::new(vec3(1.0, 0.0, 2.0)).unwrap(), Vec3::Y).with_scale(Vec3::splat(0.01))
            }),
        ],
    };

    let mut app = App::new();

    let registry = app
        .world()
        .get_resource::<AppTypeRegistry>()
        .expect("XXX TODO")
        .deref()
        .clone();

    // XXX TODO: Consider `.bpack` as a more precise extension.
    let pack_file_path = PathBuf::from("target/basset/published.pack");

    let asset_plugin = if args.mode == ArgMode::Published {
        // XXX TODO: We shouldn't be blocking or doing direct FS access. Long-term
        // goal is to read the pack file through regular asset sources.
        let pack_file = Arc::new(block_on(read_pack_file(&pack_file_path)));

        app.register_asset_source(
            AssetSourceId::default(),
            published_asset_source(AssetSourceId::default(), pack_file.clone()),
        );

        AssetPlugin {
            basset_action_source_builder: Some(Arc::new(PublishedActionSourceBuilder::new(
                pack_file.clone(),
            ))),
            ..Default::default()
        }
    } else {
        AssetPlugin {
            file_path: "examples/asset/basset/assets".to_string(),
            basset_action_source_builder: Some(Arc::new(DevelopmentActionSourceBuilder::new(
                development_action_source_settings(&app)
                    .with_default_poly_saver(RonAssetSaver::new(registry.clone()))
                    .with_validate_dependency_cache(args.validate_dependency_cache)
                    .with_validate_action_cache(args.validate_action_cache),
            ))),
            ..Default::default()
        }
    };

    app.add_plugins((
        DefaultPlugins.set(asset_plugin).set(LogPlugin {
            filter: bevy::log::DEFAULT_FILTER.to_string() + "bevy_asset::basset=debug",
            ..Default::default()
        }),
        BassetPlugin,
        FreeCameraPlugin,
        MeshletPlugin::default(),
    ))
    .init_asset::<StringAsset>()
    .init_asset::<IntAsset>()
    .register_asset_loader(StringAssetLoader)
    .register_asset_loader(IntAssetLoader)
    .register_asset_loader(MeshAssetLoader)
    .register_poly_asset_loader(RonAssetLoader::new(registry.clone()));

    test_reflect_serialization(
        registry.clone(),
        app.world().resource::<AssetServer>().clone(),
    );

    match args.mode {
        ArgMode::Development | ArgMode::Published => {
            app.insert_resource(asset_paths)
                .add_systems(Startup, setup)
                .add_systems(Update, print)
                .add_systems(Update, reload.run_if(on_timer(Duration::from_secs(2))));

            app.add_plugins(MeshletDebugPlugin);

            if args.mode == ArgMode::Development {
                app.add_systems(Update, dump.run_if(on_timer(Duration::from_secs(4))));
            }

            app.insert_resource(args);
            app.run();
        }

        ArgMode::Publish => {
            app.finish();

            let asset_server = app.world().resource::<AssetServer>();

            let input = PublishInput {
                paths: asset_paths.publish(asset_server),
            };

            block_on(
                asset_server
                    .basset_action_source()
                    .publish(input, asset_server, &pack_file_path)
                    .expect(
                        "XXX TODO: This is just checking that publishing was actually implemented.",
                    ),
            )
            .expect("XXX TODO");

            if args.dump_dependency_graph {
                asset_server.basset_action_source().dump_dependency_graph();
            }
        }
    }
}
