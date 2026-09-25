// XXX TODO: Remove when no longer needed.
#![allow(clippy::allow_attributes, reason = "XXX TODO")]

use bevy::{
    asset::{
        io::{Reader, Writer},
        meta::Settings,
        saver::{AssetSaver, ErasedSavedAsset, PolyAssetSaver, SavedAsset},
        AssetLoader, AssetPath, AsyncWriteExt, EphemeralHandleBehavior, ErasedLoadedAsset,
        HandleDeserializeProcessor, HandleSerializeProcessor, LoadContext, PolyAssetLoader,
    },
    ecs::error::BevyError,
    mesh::SerializedMesh,
    prelude::*,
    reflect::{
        serde::{ReflectDeserializer, ReflectSerializer},
        TypePath, TypeRegistryArc,
    },
    world_serialization::WorldAssetLoader,
};
use core::{ops::Deref, result::Result};
use serde::{de::DeserializeSeed, Deserialize, Serialize};

#[derive(Asset, TypePath, Debug)]
pub struct StringAsset(pub String);

// TODO: Delete? Originally used to confirm that loader settings work. Probably
// redundant now?
#[derive(Serialize, Deserialize, Default)]
pub struct StringAssetSettings {
    uppercase: bool,
}

#[derive(Default, TypePath)]
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

#[derive(Default, TypePath)]
pub struct StringAssetSaver;

impl AssetSaver for StringAssetSaver {
    type Asset = StringAsset;
    type Settings = ();
    type OutputLoader = StringAssetLoader;
    type Error = std::io::Error;

    async fn save(
        &self,
        writer: &mut Writer,
        asset: SavedAsset<'_, '_, Self::Asset>,
        _settings: &Self::Settings,
        _asset_path: AssetPath<'_>,
    ) -> Result<StringAssetSettings, Self::Error> {
        writer.write_all(asset.0.as_bytes()).await?;

        Ok(StringAssetSettings::default())
    }
}

#[derive(Asset, TypePath, Debug)]
pub struct IntAsset(pub i64);

#[derive(Default, TypePath)]
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

#[derive(Default, TypePath)]
pub struct IntAssetSaver;

impl AssetSaver for IntAssetSaver {
    type Asset = IntAsset;
    type Settings = ();
    type OutputLoader = IntAssetLoader;
    type Error = std::io::Error;

    async fn save(
        &self,
        writer: &mut Writer,
        asset: SavedAsset<'_, '_, Self::Asset>,
        _settings: &Self::Settings,
        _asset_path: AssetPath<'_>,
    ) -> Result<(), Self::Error> {
        writer.write_all(asset.0.to_string().as_bytes()).await?;

        Ok(())
    }
}

#[derive(TypePath)]
#[allow(dead_code, reason = "XXX TODO")]
pub struct RonAssetLoader {
    registry: TypeRegistryArc,
}

impl RonAssetLoader {
    #[allow(dead_code, reason = "XXX TODO")]
    pub fn new(registry: TypeRegistryArc) -> Self {
        Self { registry }
    }
}

impl PolyAssetLoader for RonAssetLoader {
    type Settings = ();
    type Error = BevyError;

    async fn load(
        &self,
        reader: &mut dyn Reader,
        _settings: &Self::Settings,
        mut load_context: LoadContext<'_>,
    ) -> Result<ErasedLoadedAsset, Self::Error> {
        // XXX TODO: Check if we can use `ron::de::from_reader`.
        let mut bytes = Vec::new();
        reader.read_to_end(&mut bytes).await?;

        let registry = self.registry.read();

        let mut processor = HandleDeserializeProcessor {
            load_from_path: &mut load_context,
        };

        let deserializer = ReflectDeserializer::with_processor(&registry, &mut processor);

        let reflected = deserializer
            .deserialize(&mut ron::de::Deserializer::from_bytes(&bytes).expect("XXX TODO"))
            .expect("XXX TODO");

        load_context.finish_reflect(reflected)
    }

    fn extensions(&self) -> &[&str] {
        &["ron"]
    }
}

#[derive(TypePath)]
#[allow(dead_code, reason = "XXX TODO")]
pub struct RonAssetSaver {
    registry: TypeRegistryArc,
}

impl RonAssetSaver {
    #[allow(dead_code, reason = "XXX TODO")]
    pub fn new(registry: TypeRegistryArc) -> Self {
        Self { registry }
    }
}

impl PolyAssetSaver for RonAssetSaver {
    type Settings = ();
    type OutputLoader = RonAssetLoader;
    type Error = BevyError;

    async fn save(
        &self,
        writer: &mut Writer,
        asset: &ErasedSavedAsset<'_, '_>,
        _settings: &Self::Settings,
        _asset_path: AssetPath<'_>,
    ) -> Result<Box<dyn Settings>, Self::Error> {
        let string = {
            let registry = self.registry.read();

            let reflected_asset = asset.as_partial_reflect(&registry).expect("XXX TODO");

            let processor = HandleSerializeProcessor {
                ephemeral_handle_behavior: EphemeralHandleBehavior::Error,
            };

            // XXX TODO: Check if we can use `ron::de::to_writer_pretty`.
            ron::ser::to_string_pretty(
                &ReflectSerializer::with_processor(reflected_asset, &registry, &processor),
                ron::ser::PrettyConfig::default(),
            )?
        };

        writer.write_all(string.as_bytes()).await?;

        Ok(Box::new(()))
    }
}

#[derive(TypePath)]
pub struct MeshAssetLoader;

impl AssetLoader for MeshAssetLoader {
    type Asset = Mesh;
    type Settings = ();
    type Error = BevyError;

    async fn load(
        &self,
        reader: &mut dyn Reader,
        _settings: &Self::Settings,
        _load_context: &mut LoadContext<'_>,
    ) -> Result<Self::Asset, Self::Error> {
        // XXX TODO: Avoid boilerplate?
        let mut bytes = Vec::new();
        reader.read_to_end(&mut bytes).await?;

        Ok(ron::de::from_bytes::<SerializedMesh>(&bytes)
            .expect("XXX TODO")
            .into_mesh())
    }
}

#[derive(TypePath)]
pub struct MeshAssetSaver;

impl AssetSaver for MeshAssetSaver {
    type Asset = Mesh;
    type Settings = ();
    type OutputLoader = MeshAssetLoader;
    type Error = BevyError;

    async fn save(
        &self,
        writer: &mut Writer,
        asset: SavedAsset<'_, '_, Self::Asset>,
        _settings: &Self::Settings,
        _asset_path: AssetPath<'_>,
    ) -> Result<<Self::OutputLoader as AssetLoader>::Settings, Self::Error> {
        let mesh = SerializedMesh::from_mesh(asset.deref().clone());
        let string = ron::ser::to_string(&mesh).expect("XXX TODO");

        writer.write_all(string.as_bytes()).await?;

        Ok(())
    }
}

// XXX TODO: Should this go in `bevy_world_serialization`? Seems odd that it
// provides a loader but not a saver. Note that `WorldAssetLoader` loads a
// `DynamicWorld`, not a `WorldAsset`.
#[derive(TypePath)]
pub struct DynamicWorldAssetSaver {
    registry: TypeRegistryArc,
}

impl DynamicWorldAssetSaver {
    pub fn new(registry: TypeRegistryArc) -> Self {
        Self { registry }
    }
}

impl AssetSaver for DynamicWorldAssetSaver {
    type Asset = DynamicWorld;
    type Settings = ();
    type OutputLoader = WorldAssetLoader;
    type Error = BevyError;

    async fn save(
        &self,
        writer: &mut Writer,
        asset: SavedAsset<'_, '_, Self::Asset>,
        _settings: &Self::Settings,
        _asset_path: AssetPath<'_>,
    ) -> Result<<WorldAssetLoader as AssetLoader>::Settings, Self::Error> {
        let string = {
            let registry = self.registry.read();

            asset.serialize(&registry).expect("XXX TODO")
        };

        writer.write_all(string.as_bytes()).await?;

        Ok(())
    }
}
