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
        let basset_loader = BassetLoader::default().with_transformer(LoadBassetTransformer);

        app.init_asset::<StringAsset>()
            .init_asset::<IntAsset>()
            .register_asset_loader(StringAssetLoader)
            .register_asset_loader(IntAssetLoader)
            .register_erased_asset_loader(Box::new(basset_loader));
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

#[derive(Serialize, Deserialize)]
enum SerializableInput {
    Path {
        path: String,

        #[serde(default)]
        loader: SerializableLoader,
    },
}

#[derive(Serialize, Deserialize, Default)]
struct SerializableLoader {
    #[serde(default)]
    name: Option<String>,

    #[serde(default)]
    settings: Option<Box<ron::value::RawValue>>,
}

#[derive(Serialize, Deserialize)]
struct SerializableTransformer {
    name: String,

    #[serde(default)]
    settings: Option<Box<ron::value::RawValue>>,
}

#[derive(Serialize, Deserialize)]
struct SerializableSaver {
    name: String,
    settings: Box<ron::value::RawValue>,
}

#[derive(Serialize, Deserialize)]
struct SerializableOutput {
    saver: SerializableSaver,

    // TODO: Decide if specifying the output's loader is good.
    loader: Option<SerializableLoader>,
}

#[derive(Serialize, Deserialize)]
struct Basset {
    input: SerializableInput,

    #[serde(default)]
    transformers: Vec<SerializableTransformer>,

    #[serde(default)]
    output: Option<SerializableOutput>,
}

trait BassetTransformer: Send + Sync + 'static {
    type Settings: Settings + Default + Serialize + for<'a> Deserialize<'a>;
    type Error: Into<BevyError>;

    #[expect(dead_code, reason = "TODO")]
    fn transform(
        &self,
        input: Option<ErasedLoadedAsset>,
        settings: Self::Settings,
        load_context: &mut LoadContext,
    ) -> impl ConditionalSendFuture<Output = Result<ErasedLoadedAsset, Self::Error>>;
}

trait ErasedBassetTransformer: Send + Sync + 'static {
    #[expect(dead_code, reason = "TODO")]
    fn transform<'a>(
        &'a self,
        input: Option<ErasedLoadedAsset>,
        settings: Option<Box<ron::value::RawValue>>,
        load_context: LoadContext<'a>,
    ) -> BoxedFuture<'a, Result<ErasedLoadedAsset, BevyError>>;
}

impl<T> ErasedBassetTransformer for T
where
    T: BassetTransformer + Send + Sync,
{
    fn transform<'a>(
        &'a self,
        input: Option<ErasedLoadedAsset>,
        settings: Option<Box<ron::value::RawValue>>,
        mut load_context: LoadContext<'a>,
    ) -> BoxedFuture<'a, Result<ErasedLoadedAsset, BevyError>> {
        Box::pin(async move {
            let settings = settings
                .map(|settings| settings.into_rust::<T::Settings>().expect("TODO"))
                .unwrap_or_default();

            <T as BassetTransformer>::transform(self, input, settings, &mut load_context)
                .await
                .map_err(Into::into)
        })
    }
}

#[derive(Default)]
struct BassetLoader {
    type_name_to_transformer: HashMap<&'static str, Arc<dyn ErasedBassetTransformer>>,
}

impl BassetLoader {
    fn with_transformer<T: BassetTransformer>(mut self, transformer: T) -> Self {
        let type_name = core::any::type_name::<T>();

        // XXX TODO: Clean up.
        let transformer: Box<dyn ErasedBassetTransformer> = Box::new(transformer);
        let transformer: Arc<dyn ErasedBassetTransformer> = transformer.into();

        self.type_name_to_transformer.insert(type_name, transformer);

        self
    }

    #[expect(dead_code, reason = "TODO")]
    fn transformer(&self, type_name: &str) -> Option<Arc<dyn ErasedBassetTransformer>> {
        self.type_name_to_transformer.get(type_name).cloned()
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
        mut load_context: LoadContext<'a>,
    ) -> BoxedFuture<'a, Result<ErasedLoadedAsset, BevyError>> {
        Box::pin(async move {
            let mut bytes = Vec::new();
            reader.read_to_end(&mut bytes).await?;

            let basset = ron::de::from_bytes::<Basset>(&bytes)?;
            dbg!(ron::ser::to_string(&basset)?);

            match basset.input {
                SerializableInput::Path { path, loader } => {
                    let path = AssetPath::try_parse(&path).expect("TODO");

                    // TODO: Find loader by name.

                    Ok(load_context
                        .loader()
                        .with_unknown_type()
                        .with_transform(move |meta| {
                            apply_settings(meta.loader_settings_mut(), &loader.settings);
                        })
                        .immediate()
                        .load(path)
                        .await?)
                }
            }

            // TODO: Needs work. load_context can't be moved.
            /*
            for transformer in basset.transformers {
                let transformer_instance = self.transformer(&transformer.name).expect("TODO");

                let transformer_settings = transformer.settings.clone();

                asset = transformer_instance
                    .transform(None, transformer_settings, load_context)
                    .await
            }
            */
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

struct LoadBassetTransformer;

#[derive(Serialize, Deserialize, Default)]
struct LoadBassetTransformerSettings {
    path: String, // TODO: Should be AssetPath, avoiding for now to simplify lifetimes.
    #[serde(default)]
    loader_settings: Option<Box<ron::value::RawValue>>,
    // XXX TODO: Loader type?
}

impl BassetTransformer for LoadBassetTransformer {
    type Settings = LoadBassetTransformerSettings;
    type Error = BevyError; // XXX TODO: What should this be?

    async fn transform(
        &self,
        input: Option<ErasedLoadedAsset>,
        settings: Self::Settings,
        load_context: &mut LoadContext<'_>,
    ) -> Result<ErasedLoadedAsset, Self::Error> {
        assert!(input.is_none());

        let path = AssetPath::parse(&settings.path);

        Ok(load_context
            .loader()
            .with_unknown_type()
            .with_transform(move |meta| {
                apply_settings(meta.loader_settings_mut(), &settings.loader_settings);
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
        /*
        assets.load::<StringAsset>("string.basset").untyped(),
        assets
            .load::<StringAsset>("string_loader_uppercase.basset")
            .untyped(),
        */
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
