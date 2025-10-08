//! Basset proof of concept.

use bevy::{
    asset::{io::Reader, AssetLoader, LoadContext},
    ecs::error::BevyError,
    platform::collections::HashMap,
    prelude::*,
    reflect::TypePath,
    tasks::{BoxedFuture, ConditionalSendFuture},
};
use bevy_asset::{
    meta::{AssetAction, AssetMeta, AssetMetaDyn, Settings},
    AssetPath, DeserializeMetaError, ErasedAssetLoader, ErasedLoadedAsset,
};
use core::result::Result;
use serde::{Deserialize, Serialize};
use std::{boxed::Box, sync::Arc};

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
        .add_systems(Update, print)
        .run();
}

struct BassetPlugin;

impl Plugin for BassetPlugin {
    fn build(&self, app: &mut App) {
        app.init_asset::<StringAsset>()
            .init_asset::<IntAsset>()
            .register_asset_loader(StringAssetLoader)
            .register_asset_loader(IntAssetLoader)
            .register_erased_asset_loader(Box::new(
                BassetLoader::default().with_action(LoadPathAction),
            ));
    }
}

#[derive(Asset, TypePath, Debug)]
#[expect(dead_code, reason = "TODO")]
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

trait BassetAction: Send + Sync + 'static {
    // TODO: Should this use `Settings`?
    type Params: Settings + Default + Serialize + for<'a> Deserialize<'a>;
    type Error: Into<BevyError>;

    fn apply(
        &self,
        params: Self::Params,
        load_context: &mut LoadContext,
    ) -> impl ConditionalSendFuture<Output = Result<ErasedLoadedAsset, Self::Error>>;
}

trait ErasedBassetAction: Send + Sync + 'static {
    fn apply<'a>(
        &'a self,
        params: &'a Option<Box<ron::value::RawValue>>,
        load_context: &'a mut LoadContext<'a>,
    ) -> BoxedFuture<'a, Result<ErasedLoadedAsset, BevyError>>;
}

impl<T> ErasedBassetAction for T
where
    T: BassetAction + Send + Sync,
{
    fn apply<'a>(
        &'a self,
        params: &'a Option<Box<ron::value::RawValue>>,
        load_context: &'a mut LoadContext<'a>,
    ) -> BoxedFuture<'a, Result<ErasedLoadedAsset, BevyError>> {
        // TODO: Check that we're correctly using BoxedFuture and Box::pin.
        Box::pin(async move {
            let params = params
                .as_ref()
                .map(|p| p.into_rust::<T::Params>().expect("TODO"))
                .unwrap_or_default();

            <T as BassetAction>::apply(self, params, load_context)
                .await
                .map_err(Into::into)
        })
    }
}

#[derive(Serialize, Deserialize)]
struct SerializableAction {
    name: String,
    params: Option<Box<ron::value::RawValue>>,
}

#[derive(Serialize, Deserialize)]
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

        // XXX TODO: Clean up.
        let action: Box<dyn ErasedBassetAction> = Box::new(action);
        let action: Arc<dyn ErasedBassetAction> = action.into();

        self.type_name_to_action.insert(type_name, action);

        self
    }

    fn action(&self, type_name: &str) -> Option<Arc<dyn ErasedBassetAction>> {
        self.type_name_to_action.get(type_name).cloned()
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

impl ErasedAssetLoader for BassetLoader {
    fn load<'a>(
        &'a self,
        reader: &'a mut dyn Reader,
        _meta: &'a dyn AssetMetaDyn,
        load_context: LoadContext<'a>,
    ) -> BoxedFuture<'a, Result<ErasedLoadedAsset, BevyError>> {
        // TODO: Check that we're correctly using BoxedFuture and Box::pin.
        Box::pin(async move {
            // Move load_context so that it can be referenced as &mut later.
            let mut load_context = load_context;

            let mut bytes = Vec::new();
            reader.read_to_end(&mut bytes).await?;

            let basset = ron::de::from_bytes::<Basset>(&bytes)?;
            dbg!(ron::ser::to_string(&basset)?);

            let action = self.action(&basset.root.name).expect("TODO");

            action.apply(&basset.root.params, &mut load_context).await
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

struct LoadPathAction;

#[derive(Serialize, Deserialize, Default)]
struct LoadPathActionParams {
    path: String, // TODO: Should be AssetPath. Avoiding for now to simplify lifetimes.
    #[serde(default)]
    loader_settings: Option<Box<ron::value::RawValue>>,
    // TODO: Requires some faff to implement. NestedLoader doesn't have a way
    // to choose the loader by name, although it does have a way to choose by type
    // id. So we should either extend NestedLoader, or get the type id from AssetServer.
    //loader_name: Option<String>,
}

impl BassetAction for LoadPathAction {
    type Params = LoadPathActionParams;
    type Error = BevyError; // XXX TODO: What should this be?

    async fn apply(
        &self,
        params: Self::Params,
        load_context: &mut LoadContext<'_>,
    ) -> Result<ErasedLoadedAsset, Self::Error> {
        let path = AssetPath::parse(&params.path);

        Ok(load_context
            .loader()
            .with_unknown_type()
            .with_transform(move |meta| {
                apply_settings(meta.loader_settings_mut(), &params.loader_settings);
            })
            .immediate()
            .load(path)
            .await?)
    }
}

#[derive(Resource)]
#[expect(dead_code, reason = "Needed to keep handles live")]
struct Handles(Vec<UntypedHandle>);

fn setup(mut commands: Commands, assets: Res<AssetServer>) {
    commands.insert_resource(Handles(vec![
        assets.load::<StringAsset>("asdf.string").untyped(),
        assets.load::<IntAsset>("1234.int").untyped(),
        assets.load::<IntAsset>("int.basset").untyped(),
        assets.load::<StringAsset>("string.basset").untyped(),
        assets
            .load::<StringAsset>("string_loader_uppercase.basset")
            .untyped(),
    ]));
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
    mut string_events: MessageReader<AssetEvent<StringAsset>>,
    mut int_events: MessageReader<AssetEvent<IntAsset>>,
) {
    print_events(&asset_server, &string_assets, &mut string_events);
    print_events(&asset_server, &int_assets, &mut int_events);
}
