//! Basset proof of concept.

use bevy::{
    asset::{io::Reader, AssetLoader, LoadContext},
    ecs::error::BevyError,
    light::CascadeShadowConfigBuilder,
    platform::collections::HashMap,
    prelude::*,
    reflect::TypePath,
    tasks::{BoxedFuture, ConditionalSendFuture},
};
use bevy_asset::{
    meta::{AssetAction, AssetHash, AssetMeta, AssetMetaDyn, Settings},
    AssetPath, DeserializeMetaError, ErasedAssetLoader, ErasedLoadedAsset, LoadedAsset,
};
use core::result::Result;
use serde::{Deserialize, Serialize};
use std::{
    boxed::Box,
    sync::{Arc, Mutex},
};

struct BassetPlugin;

impl Plugin for BassetPlugin {
    fn build(&self, app: &mut App) {
        app.init_asset::<StringAsset>()
            .init_asset::<IntAsset>()
            .init_asset::<acme::AcmeScene>()
            .register_asset_loader(StringAssetLoader)
            .register_asset_loader(IntAssetLoader)
            .register_erased_asset_loader(Box::new(
                BassetLoader::default()
                    .with_action(LoadPathAction)
                    .with_action(JoinStringsAction)
                    .with_action(UppercaseStringAction)
                    .with_action(SceneFromGltfAction),
            ));
    }
}

struct BassetActionContext<'a> {
    loader: &'a BassetLoader,
    asset_server: &'a AssetServer,
    loader_dependencies: HashMap<AssetPath<'static>, AssetHash>,
}

trait BassetAction: Send + Sync + 'static {
    // TODO: Should this use `Settings`?
    type Params: Settings + Default + Serialize + for<'a> Deserialize<'a>;
    type Error: Into<BevyError>;

    fn apply(
        &self,
        context: &mut BassetActionContext<'_>,
        params: &Self::Params,
    ) -> impl ConditionalSendFuture<Output = Result<ErasedLoadedAsset, Self::Error>>;
}

impl BassetActionContext<'_> {
    async fn apply<T: Asset>(&mut self, action: &SerializableAction) -> Result<T, BevyError> {
        Ok(self.erased_apply(action).await?.take::<T>().expect("TODO"))
    }

    async fn erased_apply(
        &mut self,
        action: &SerializableAction,
    ) -> Result<ErasedLoadedAsset, BevyError> {
        self.loader
            .action(&action.name)
            .expect("TODO")
            .apply(self, &action.params)
            .await
    }

    async fn erased_load(
        &mut self,
        path: AssetPath<'static>,
        settings: &Option<Box<ron::value::RawValue>>,
    ) -> Result<ErasedLoadedAsset, BevyError> {
        let (asset, hash) = load_direct(self.asset_server, &path, settings).await?;

        self.loader_dependencies.insert(path, hash);

        Ok(asset)
    }
}

trait ErasedBassetAction: Send + Sync + 'static {
    fn apply<'a>(
        &'a self,
        context: &'a mut BassetActionContext,
        params: &'a ron::value::RawValue,
    ) -> BoxedFuture<'a, Result<ErasedLoadedAsset, BevyError>>;
}

impl<T> ErasedBassetAction for T
where
    T: BassetAction + Send + Sync,
{
    fn apply<'a>(
        &'a self,
        context: &'a mut BassetActionContext,
        params: &'a ron::value::RawValue,
    ) -> BoxedFuture<'a, Result<ErasedLoadedAsset, BevyError>> {
        // TODO: Check that we're correctly using BoxedFuture and Box::pin.
        Box::pin(async move {
            let params = params.into_rust::<T::Params>().expect("TODO");

            T::apply(self, context, &params).await.map_err(Into::into)
        })
    }
}

#[derive(Serialize, Deserialize, Debug)]
struct SerializableAction {
    name: String,
    // TODO: If we split this into SerializableAction and DeserializableAction
    // then the DeserializableAction can replace the box with a reference. See
    // serde_json RawValue example.
    params: Box<ron::value::RawValue>,
}

impl SerializableAction {
    #[expect(dead_code, reason = "TODO")]
    fn new<Type, ParamsType: Serialize>(params: &ParamsType) -> Self {
        Self {
            name: core::any::type_name::<Type>().into(),
            params: ron::value::RawValue::from_rust(params).expect("TODO"),
        }
    }
}

impl Default for SerializableAction {
    fn default() -> Self {
        // TODO: This is ugly. Can't see an efficient way to support Default for
        // a Box<ron::RawValue>.
        //
        // Could make `SerializableAction::params` an `Option<RawValue>`. But
        // that makes the RON messy unless we allow implicit Some.
        //
        // Maybe we can make a new type that contains an optional RawValue but
        // still serializes as if it's non-optional?
        //
        // Also consider splitting off a DeserializableAction (which doesn't
        // need a boxed ron), and then SerializableAction doesn't need default.
        let empty_struct =
            ron::value::RawValue::from_boxed_ron(Box::<str>::from("()")).expect("TODO");

        Self {
            name: Default::default(),

            params: empty_struct,
        }
    }
}

#[derive(Serialize, Deserialize, Debug)]
struct Basset {
    root: SerializableAction,
}

#[derive(Default)]
struct BassetLoader {
    type_name_to_action: HashMap<&'static str, Arc<dyn ErasedBassetAction>>,
}

impl BassetLoader {
    fn with_action<T: BassetAction>(mut self, action: T) -> Self {
        let type_name = core::any::type_name::<T>();

        // XXX TODO: This conversion from action to Box and later to Arc feels
        // too boilerplate-y. Simpler way?
        let action: Box<dyn ErasedBassetAction> = Box::new(action);

        self.type_name_to_action
            .insert(type_name, Arc::from(action));

        self
    }

    fn action<'a>(&'a self, type_name: &str) -> Option<&'a dyn ErasedBassetAction> {
        // TODO: Review this. Can't decide if we should return Arc here or avoid
        // storing Arc in the first place.
        self.type_name_to_action.get(type_name).map(move |a| &**a)
    }
}

impl ErasedAssetLoader for BassetLoader {
    fn load<'a>(
        &'a self,
        reader: &'a mut dyn Reader,
        _meta: &'a dyn AssetMetaDyn,
        load_context: LoadContext<'a>,
    ) -> BoxedFuture<'a, Result<ErasedLoadedAsset, BevyError>> {
        // TODO: Check that we're correctly using BoxedFuture and Box::pin.
        Box::pin(async move {
            let mut bytes = Vec::new();
            reader.read_to_end(&mut bytes).await?;

            let basset = ron::de::from_bytes::<Basset>(&bytes)?;

            let mut context = BassetActionContext {
                loader: self,
                asset_server: load_context.server(),
                loader_dependencies: HashMap::default(),
            };

            let asset = context.erased_apply(&basset.root).await?;

            // XXX TODO: At this point we should replace `asset.loader_dependencies`
            // with our own `context.loader_dependencies`.

            // TODO: Better way to dump temporary debug without it getting mixed
            // up due to threading.
            {
                static MUTEX: Mutex<()> = Mutex::new(());
                let _lock = MUTEX.lock();

                dbg!(load_context.path());
                dbg!(ron::ser::to_string(&basset)?);

                for (dependency, _) in context.loader_dependencies {
                    dbg!(dependency);
                }
            }

            Ok(asset)
        })
    }

    fn extensions(&self) -> &[&str] {
        &["basset"]
    }

    fn deserialize_meta(&self, meta: &[u8]) -> Result<Box<dyn AssetMetaDyn>, DeserializeMetaError> {
        // XXX TODO: Review. Is this going to be problematic since we don't know the loader type?
        let meta = AssetMeta::<FakeAssetLoader, ()>::deserialize(meta)?;
        Ok(Box::new(meta))
    }

    fn default_meta(&self) -> Box<dyn AssetMetaDyn> {
        // XXX TODO: Review. Is this going to be problematic since we don't know the loader type?
        Box::new(AssetMeta::<FakeAssetLoader, ()>::new(AssetAction::Load {
            loader: self.type_name().to_string(),
            settings: <FakeAssetLoader as AssetLoader>::Settings::default(),
        }))
    }

    fn type_name(&self) -> &'static str {
        core::any::type_name::<BassetLoader>()
    }

    fn type_id(&self) -> std::any::TypeId {
        core::any::TypeId::of::<BassetLoader>()
    }

    fn asset_type_name(&self) -> Option<&'static str> {
        None
    }

    fn asset_type_id(&self) -> Option<std::any::TypeId> {
        None
    }
}

fn apply_settings(settings: Option<&mut dyn Settings>, ron: &Option<Box<ron::value::RawValue>>) {
    let Some(settings) = settings else {
        return;
    };

    let Some(ron) = ron else {
        return;
    };

    if let Some(settings) = settings.downcast_mut::<StringAssetSettings>() {
        *settings = ron
            .clone()
            .into_rust::<StringAssetSettings>()
            .expect("TODO");
    }
}

async fn load_direct(
    asset_server: &AssetServer,
    path: &AssetPath<'static>,
    settings: &Option<Box<ron::value::RawValue>>,
) -> Result<(ErasedLoadedAsset, AssetHash), BevyError> {
    let (mut meta, loader, mut reader) = asset_server
        .get_meta_loader_and_reader(path, None)
        .await
        .map_err(Into::<BevyError>::into)?;

    apply_settings(meta.loader_settings_mut(), settings);

    // Roughly the same as LoadContext::load_direct_internal.

    let load_dependencies = false;
    let populate_hashes = false;

    let asset = asset_server
        .load_with_meta_loader_and_reader(
            path,
            &*meta,
            &*loader,
            &mut *reader,
            load_dependencies,
            populate_hashes,
        )
        .await
        .map_err(Into::<BevyError>::into)?;

    let info = meta.processed_info().as_ref();
    let hash = info.map(|i| i.full_hash).unwrap_or_default();

    Ok((asset, hash))
}

struct LoadPathAction;

#[derive(Serialize, Deserialize, Default)]
struct LoadPathActionParams {
    path: String, // TODO: Should be AssetPath. Avoiding for now to simplify lifetimes.
    #[serde(default)]
    loader_settings: Option<Box<ron::value::RawValue>>,
    // TODO
    //loader_name: Option<String>,
}

impl BassetAction for LoadPathAction {
    type Params = LoadPathActionParams;
    type Error = BevyError;

    async fn apply(
        &self,
        context: &mut BassetActionContext<'_>,
        params: &Self::Params,
    ) -> Result<ErasedLoadedAsset, Self::Error> {
        let path = AssetPath::parse(&params.path).into_owned();

        context.erased_load(path, &params.loader_settings).await
    }
}

struct JoinStringsAction;

#[derive(Serialize, Deserialize, Default)]
struct JoinStringsActionParams {
    separator: String,
    strings: Vec<SerializableAction>,
}

impl BassetAction for JoinStringsAction {
    type Params = JoinStringsActionParams;
    type Error = BevyError;

    async fn apply(
        &self,
        context: &mut BassetActionContext<'_>,
        params: &Self::Params,
    ) -> Result<ErasedLoadedAsset, Self::Error> {
        let mut strings = Vec::new();

        for action in &params.strings {
            strings.push(context.apply::<StringAsset>(action).await?.0);
        }

        let joined = strings
            .into_iter()
            .reduce(|l, r| l + &params.separator + &r)
            .unwrap_or("".to_owned());

        Ok(LoadedAsset::new_with_dependencies(StringAsset(joined)).into())
    }
}

struct UppercaseStringAction;

#[derive(Serialize, Deserialize, Default)]
struct UppercaseStringActionParams {
    string: SerializableAction,
}

impl BassetAction for UppercaseStringAction {
    type Params = UppercaseStringActionParams;
    type Error = BevyError;

    async fn apply(
        &self,
        context: &mut BassetActionContext<'_>,
        params: &Self::Params,
    ) -> Result<ErasedLoadedAsset, Self::Error> {
        let string = StringAsset(
            context
                .apply::<StringAsset>(&params.string)
                .await?
                .0
                .to_uppercase(),
        );

        Ok(LoadedAsset::new_with_dependencies(string).into())
    }
}

struct SceneFromGltfAction;

#[derive(Serialize, Deserialize, Default)]
struct SceneFromGltfActionParams {
    path: String,
}

impl BassetAction for SceneFromGltfAction {
    type Params = SceneFromGltfActionParams;
    type Error = BevyError;

    async fn apply(
        &self,
        context: &mut BassetActionContext<'_>,
        params: &Self::Params,
    ) -> Result<ErasedLoadedAsset, Self::Error> {
        let gltf = load_direct(
            context.asset_server,
            &AssetPath::parse(&params.path).into_owned(),
            &None,
        )
        .await?
        .0;

        let scene = acme::from_gltf(&gltf, context.asset_server);

        // XXX TODO: What about dependencies?

        Ok(LoadedAsset::new_with_dependencies(scene).into())
    }
}

#[derive(Asset, TypePath, Debug)]
struct StringAsset(String);

#[derive(Serialize, Deserialize, Default)]
struct StringAssetSettings {
    uppercase: bool,
}

#[derive(Default)]
struct StringAssetLoader;

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

        // TODO: Error handling.
        let mut string = String::from_utf8(bytes).unwrap();

        if settings.uppercase {
            string = string.to_uppercase();
        }

        Ok(StringAsset(string))
    }

    fn extensions(&self) -> &[&str] {
        &["string"]
    }
}

#[derive(Asset, TypePath, Debug)]
#[expect(dead_code, reason = "TODO")]
struct IntAsset(i64);

#[derive(Default)]
struct IntAssetLoader;

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

// TODO: Review this. Awkward hack so that `BassetLoader::default_meta` and
// `BassetLoader::deserialize_meta` can return a meta even if it's wrong.
struct FakeAssetLoader;

impl AssetLoader for FakeAssetLoader {
    type Asset = ();
    type Settings = ();
    type Error = std::io::Error;

    async fn load(
        &self,
        _reader: &mut dyn Reader,
        _settings: &Self::Settings,
        _load_context: &mut LoadContext<'_>,
    ) -> Result<Self::Asset, Self::Error> {
        Ok(())
    }
}

mod acme {
    use super::*;

    #[derive(Serialize, Deserialize, Default, Debug)]
    pub struct AcmeHandle {
        path: String,
    }

    impl AcmeHandle {
        fn new(asset_server: &AssetServer, handle: &UntypedHandle) -> Self {
            Self {
                path: asset_server
                    .get_path(handle.id())
                    .expect("TODO")
                    .to_string(),
            }
        }
    }

    impl<'a> From<&'a AcmeHandle> for AssetPath<'a> {
        fn from(handle: &'a AcmeHandle) -> Self {
            AssetPath::parse(&handle.path)
        }
    }

    #[derive(Serialize, Deserialize, Debug)]
    pub struct AcmeMesh {
        asset: AcmeHandle,
    }

    #[derive(Serialize, Deserialize, Debug)]
    pub struct AcmeMaterial {
        base_color_texture: Option<AcmeHandle>,
    }

    #[derive(Serialize, Deserialize, Default, Debug)]
    pub struct AcmeEntity {
        #[serde(default)]
        transform: Transform,

        #[serde(default)]
        mesh: Option<AcmeMesh>,

        #[serde(default)]
        material: Option<AcmeMaterial>,
    }

    #[derive(Asset, TypePath, Serialize, Deserialize, Default, Debug)]
    pub struct AcmeScene {
        entities: Vec<AcmeEntity>,
    }

    fn get_sub_asset<'a, T: Asset>(
        asset: &'a ErasedLoadedAsset,
        sub_asset_handle: &Handle<T>,
        asset_server: &'a AssetServer,
    ) -> &'a T {
        let label = asset_server
            .get_path(sub_asset_handle.id().untyped())
            .expect("TODO")
            .label_cow()
            .expect("TODO");

        asset
            .get_labeled(label.into_owned())
            .expect("TODO")
            .get::<T>()
            .expect("TODO")
    }

    pub fn from_gltf(asset: &ErasedLoadedAsset, asset_server: &AssetServer) -> AcmeScene {
        let mut entities = Vec::<AcmeEntity>::new();

        let gltf = asset.get::<Gltf>().expect("TODO");

        let mut stack = gltf
            .nodes
            .iter()
            .map(|node_handle| {
                let node = get_sub_asset(asset, node_handle, asset_server);

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
                .map(|mesh_handle| get_sub_asset(asset, mesh_handle, asset_server))
            {
                for primitive in mesh.primitives.iter() {
                    let mesh = Some(AcmeMesh {
                        asset: AcmeHandle::new(asset_server, &primitive.mesh.clone().untyped()),
                    });

                    let standard_material = get_sub_asset(
                        asset,
                        primitive.material.as_ref().expect("TODO"),
                        asset_server,
                    );

                    let material = Some(AcmeMaterial {
                        base_color_texture: standard_material
                            .base_color_texture
                            .clone()
                            .map(|p| AcmeHandle::new(asset_server, &p.untyped())),
                    });

                    entities.push(AcmeEntity {
                        transform,
                        mesh,
                        material,
                    });
                }
            }

            for child in node
                .children
                .iter()
                .map(|child_handle| get_sub_asset(asset, child_handle, asset_server))
            {
                stack.push((child, transform * child.transform));
            }
        }

        AcmeScene { entities }
    }

    pub fn spawn(
        commands: &mut Commands,
        scene: &AcmeScene,
        asset_server: &AssetServer,
        material_assets: &mut Assets<StandardMaterial>,
    ) {
        for scene_entity in &scene.entities {
            let mut world_entity = commands.spawn(scene_entity.transform);

            if let Some(material) = &scene_entity.material {
                let standard_material = StandardMaterial {
                    base_color_texture: material
                        .base_color_texture
                        .as_ref()
                        .map(|p| asset_server.load::<Image>(p)),
                    ..Default::default()
                };

                world_entity.insert(MeshMaterial3d(material_assets.add(standard_material)));
            }

            if let Some(mesh) = &scene_entity.mesh {
                world_entity.insert(Mesh3d(asset_server.load::<Mesh>(&mesh.asset)));
            }
        }
    }
}

#[derive(Resource)]
#[expect(dead_code, reason = "Needed to keep handles live")]
struct Handles(Vec<UntypedHandle>);

fn setup(
    mut commands: Commands,
    asset_server: Res<AssetServer>,
    mut meshes: ResMut<Assets<Mesh>>,
    mut materials: ResMut<Assets<StandardMaterial>>,
) {
    let inline_path = AssetPath::from_basset(
        "(root: (name: \"basset::LoadPathAction\", params: (path: \"1234.int\")))",
    );

    commands.insert_resource(Handles(vec![
        asset_server.load::<StringAsset>("hello.string").untyped(),
        asset_server.load::<StringAsset>("world.string").untyped(),
        asset_server.load::<IntAsset>("1234.int").untyped(),
        asset_server.load::<IntAsset>("int.basset").untyped(),
        asset_server.load::<StringAsset>("string.basset").untyped(),
        asset_server
            .load::<StringAsset>("string_loader_uppercase.basset")
            .untyped(),
        asset_server
            .load::<StringAsset>("join_strings.basset")
            .untyped(),
        asset_server
            .load::<acme::AcmeScene>("scene_from_gltf.basset")
            .untyped(),
        asset_server.load::<IntAsset>(inline_path).untyped(),
    ]));

    commands.spawn((
        Camera3d::default(),
        Transform::from_xyz(200.0, 200.0, 200.0).looking_at(Vec3::new(0.0, 75.0, 0.0), Vec3::Y),
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
        if let AssetEvent::Added { id } = *event {
            println!(
                "{:?}: {:?}",
                asset_server.get_path(id).unwrap(),
                assets.get(id).unwrap()
            );
        }
    }
}

fn print(
    asset_server: Res<AssetServer>,
    string_assets: Res<Assets<StringAsset>>,
    int_assets: Res<Assets<IntAsset>>,
    scene_assets: Res<Assets<acme::AcmeScene>>,
    mut string_events: MessageReader<AssetEvent<StringAsset>>,
    mut int_events: MessageReader<AssetEvent<IntAsset>>,
    mut scene_events: MessageReader<AssetEvent<acme::AcmeScene>>,
) {
    print_events(&asset_server, &string_assets, &mut string_events);
    print_events(&asset_server, &int_assets, &mut int_events);
    print_events(&asset_server, &scene_assets, &mut scene_events);
}

fn spawn(
    mut commands: Commands,
    scene_assets: Res<Assets<acme::AcmeScene>>,
    mut scene_events: MessageReader<AssetEvent<acme::AcmeScene>>,
    asset_server: Res<AssetServer>,
    mut material_assets: ResMut<Assets<StandardMaterial>>,
) {
    for event in scene_events.read() {
        if let AssetEvent::Added { id } = *event {
            let scene = scene_assets.get(id).unwrap();

            acme::spawn(&mut commands, scene, &asset_server, &mut material_assets);
        }
    }
}

fn main() {
    App::new()
        .add_plugins((
            DefaultPlugins.set(AssetPlugin {
                file_path: "examples/asset/basset/assets".to_string(),
                ..default()
            }),
            BassetPlugin,
        ))
        .add_systems(Startup, setup)
        .add_systems(Update, (print, spawn))
        .run();
}
