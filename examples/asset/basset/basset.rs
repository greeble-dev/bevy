//! Basset proof of concept.

// XXX TODO: Remove when no longer needed.
#![allow(clippy::allow_attributes, reason = "XXX TODO")]

use argh::FromArgs;
use bevy::{
    asset::{
        basset::*, io::Reader, saver::AssetSaver, AssetLoader, AssetRef, ErasedLoadedAsset,
        HandleDeserializeProcessor, LoadContext, PolyAssetLoader,
    },
    camera_controller::free_camera::{FreeCamera, FreeCameraPlugin},
    ecs::error::BevyError,
    light::CascadeShadowConfigBuilder,
    log::LogPlugin,
    mesh::SerializedMesh,
    pbr::experimental::meshlet::*,
    prelude::*,
    reflect::{
        serde::{ReflectDeserializer, ReflectSerializer},
        TypePath,
    },
    render::render_resource::AsBindGroup,
    tasks::block_on,
    time::common_conditions::on_timer,
};
// XXX TODO: Should be in `use bevy` above?
use bevy_asset::{
    basset::{
        action::LoadPath,
        publisher::{published_asset_source, read_pack_file, PublishDependency, PublishInput},
    },
    basset_action_version,
    io::{AssetSourceId, Writer},
    meta::Settings,
    saver::{ErasedSavedAsset, PolyAssetSaver, SavedAsset},
    AssetPath, AsyncWriteExt, EphemeralHandleBehavior, HandleSerializeProcessor,
};
use bevy_image::{ImageSaver, ImageSaverSettings};
use bevy_reflect::{TypeRegistry, TypeRegistryArc};
use bevy_scene::SceneDependencies;
use core::{
    hash::{Hash, Hasher},
    ops::Deref,
    result::Result,
};
use serde::{de::DeserializeSeed, Deserialize, Serialize};
use std::{any::TypeId, path::PathBuf, str::FromStr, sync::Arc, time::Duration};

mod action {
    use bevy::{math::FloatOrd, mesh::Indices};
    use core::ops::Mul;
    // XXX TODO: Should be in `use bevy` above?
    use bevy_asset::RenderAssetUsages;
    #[cfg(feature = "compressed_image_saver_universal")]
    use bevy_image::universal::CompressedImageSaverUniversal;
    use bevy_image::{
        ctt::{CompressedImageSaverCtt, CompressedImageSaverCttFormat},
        CompressedImageSaverSettings,
    };
    use fast_image_resize::{FilterType, ResizeAlg, ResizeOptions, Resizer};
    use image::{DynamicImage, Rgb, RgbImage};

    use super::*;

    #[derive(Default, Debug, PartialEq, Hash, Reflect)]
    #[reflect(BassetAction, PartialEq, Hash)]
    pub struct JoinStrings {
        separator: String,
        strings: Vec<AssetRef<'static>>,
    }

    impl BassetAction for JoinStrings {
        basset_action_version!(crate);
    }

    #[derive(TypePath)]
    pub struct JoinStringsFunction;

    impl BassetActionFunction for JoinStringsFunction {
        type Action = JoinStrings;
        type Error = BevyError;

        async fn apply(
            &self,
            mut context: ApplyContext<'_>,
            action: &Self::Action,
        ) -> Result<BassetActionOutput, Self::Error> {
            let mut strings = Vec::new();

            for path in &action.strings {
                strings.push(context.load_dependee::<demo::StringAsset>(path).await?.0);
            }

            let joined = strings
                .into_iter()
                .reduce(|l, r| l + &action.separator + &r)
                .unwrap_or_else(|| "".to_owned());

            Ok(context.finish(demo::StringAsset(joined)))
        }
    }

    #[derive(Default, Debug, PartialEq, Hash, Reflect)]
    #[reflect(BassetAction, PartialEq, Hash)]
    pub struct UppercaseString {
        string: AssetRef<'static>,
    }

    impl BassetAction for UppercaseString {
        basset_action_version!(crate);
    }

    #[derive(TypePath)]
    pub struct UppercaseStringFunction;

    impl BassetActionFunction for UppercaseStringFunction {
        type Action = UppercaseString;
        type Error = BevyError;

        async fn apply(
            &self,
            mut context: ApplyContext<'_>,
            action: &Self::Action,
        ) -> Result<BassetActionOutput, Self::Error> {
            let string = demo::StringAsset(
                context
                    .load_dependee::<demo::StringAsset>(&action.string)
                    .await?
                    .0
                    .to_uppercase(),
            );

            Ok(context.finish(string))
        }
    }

    /// Creates an `AcmeScene` from a `Gltf`. This does not respect the glTF's
    /// scenes list - it just takes every node.
    #[derive(Default, Debug, PartialEq, Hash, Reflect)]
    #[reflect(BassetAction, PartialEq, Hash)]
    pub struct AcmeSceneFromGltf {
        gltf: AssetRef<'static>,
        // XXX TODO: Would be nice to support selecting a scene. but that's
        // awkward to do - we'd have to dig around `Gltf::scenes` and extract
        // everything from components.
        //#[serde(default)]
        //scene: Option<String>,
    }

    impl BassetAction for AcmeSceneFromGltf {
        basset_action_version!(crate);
    }

    #[derive(TypePath)]
    pub struct AcmeSceneFromGltfFunction;

    impl BassetActionFunction for AcmeSceneFromGltfFunction {
        type Action = AcmeSceneFromGltf;
        type Error = BevyError;

        async fn apply(
            &self,
            mut context: ApplyContext<'_>,
            action: &Self::Action,
        ) -> Result<BassetActionOutput, Self::Error> {
            let gltf = context.erased_load_dependee(&action.gltf).await?;

            let scene = acme::from_gltf(&gltf)?;

            // XXX TODO: What about dependencies?

            Ok(context.finish(scene))
        }
    }

    #[derive(Default, Debug, PartialEq, Hash, Reflect)]
    #[reflect(BassetAction, PartialEq, Hash)]
    pub struct MeshletFromMesh {
        pub mesh: AssetRef<'static>, // XXX TODO: Better if we had a typed asset ref?
        pub vertex_position_quantization_factor: Option<u8>,
    }

    impl MeshletFromMesh {
        pub fn new(mesh: impl Into<AssetRef<'static>>) -> Self {
            Self {
                mesh: mesh.into(),
                ..Default::default()
            }
        }

        fn vertex_position_quantization_factor(&self) -> u8 {
            self.vertex_position_quantization_factor
                .unwrap_or(MESHLET_DEFAULT_VERTEX_POSITION_QUANTIZATION_FACTOR)
        }
    }

    impl BassetAction for MeshletFromMesh {
        basset_action_version!(crate);
    }

    impl From<MeshletFromMesh> for AssetRef<'static> {
        fn from(value: MeshletFromMesh) -> Self {
            AssetRef::new(value)
        }
    }

    #[derive(TypePath)]
    pub struct MeshletFromMeshFunction;

    impl BassetActionFunction for MeshletFromMeshFunction {
        type Action = MeshletFromMesh;
        type Error = BevyError;

        async fn apply(
            &self,
            mut context: ApplyContext<'_>,
            action: &Self::Action,
        ) -> Result<BassetActionOutput, Self::Error> {
            // TODO: Should we check if `MeshletPlugin` is registered so we can
            // return a sensible error?

            let mesh = context.load_dependee::<Mesh>(&action.mesh).await?;

            let meshlet =
                MeshletMesh::from_mesh(&mesh, action.vertex_position_quantization_factor())?;

            Ok(context.finish(meshlet))
        }
    }

    #[derive(Default, Debug, PartialEq, Hash, Reflect)]
    #[reflect(BassetAction, PartialEq, Hash)]
    pub struct ConvertAcmeSceneMeshesToMeshlets {
        scene: AssetRef<'static>,
        #[reflect(default)]
        vertex_position_quantization_factor: Option<u8>,
    }

    impl BassetAction for ConvertAcmeSceneMeshesToMeshlets {
        basset_action_version!(crate);
    }

    #[derive(TypePath)]
    pub struct ConvertAcmeSceneMeshesToMeshletsFunction;

    impl BassetActionFunction for ConvertAcmeSceneMeshesToMeshletsFunction {
        type Action = ConvertAcmeSceneMeshesToMeshlets;
        type Error = BevyError;

        async fn apply(
            &self,
            mut context: ApplyContext<'_>,
            action: &Self::Action,
        ) -> Result<BassetActionOutput, Self::Error> {
            // TODO: Should we check if `MeshletPlugin` is registered so we can
            // return a sensible error?

            let mut scene = context
                .load_dependee::<acme::AcmeScene>(&action.scene)
                .await?;

            for entity in &mut scene.entities {
                if let Some(mesh) = entity.mesh.take() {
                    entity.meshlet_mesh = Some(context.make_handle(MeshletFromMesh {
                        mesh: mesh.path().expect("XXX TODO").clone(),
                        vertex_position_quantization_factor:
                            action.vertex_position_quantization_factor,
                    }));
                }
            }

            Ok(context.finish(scene))
        }
    }

    #[derive(Default, Debug, PartialEq, Hash, Reflect)]
    #[reflect(BassetAction, PartialEq, Hash)]
    pub struct CompressImage {
        pub image: AssetRef<'static>,
    }

    impl BassetAction for CompressImage {
        basset_action_version!(crate);

        fn env<'a>(&'a self) -> EnvironmentSchema<'a> {
            EnvironmentSchema(&[EnvironmentSchemaKey {
                name: "compressed_texture_format",
                required: EnvironmentRequiredKey::Yes,
            }])
        }
    }

    impl CompressImage {
        pub fn new(image: impl Into<AssetRef<'static>>) -> Self {
            Self {
                image: image.into(),
            }
        }
    }

    impl From<CompressImage> for AssetRef<'static> {
        fn from(value: CompressImage) -> Self {
            AssetRef::new(value)
        }
    }

    #[derive(TypePath)]
    pub struct CompressImageFunction;

    impl BassetActionFunction for CompressImageFunction {
        type Action = CompressImage;
        type Error = BevyError; // XXX: Or use `CompressedImageSaverError`?

        async fn apply(
            &self,
            mut context: ApplyContext<'_>,
            action: &Self::Action,
        ) -> Result<BassetActionOutput, Self::Error> {
            // XXX TODO: See what can be refactored out of here.
            let uncompressed_asset = context
                .erased_load_dependee(&action.image)
                .await?
                .take::<Image>()
                .expect("XXX TODO");

            let uncompressed_size = uncompressed_asset.size();

            // XXX TODO: Review. `CompressedImageSaver` can fail if the compressed
            // size is not pow2. Unclear if we should be doing this automatically
            // or if it should be done by `CompressedImageSaver`.
            let compressed_size = UVec2::new(
                uncompressed_size.x.next_power_of_two().max(4),
                uncompressed_size.y.next_power_of_two().max(4),
            );

            let mut resized_asset = if uncompressed_size != compressed_size {
                resize_image(uncompressed_asset, compressed_size)
            } else {
                uncompressed_asset
            };

            // XXX TODO: Verify we're correctly handling dependencies. Currently
            // `finished_erased_saved` overwrites the loader dependencies we
            // passed in.

            // XXX TODO: Review if we should be customizing these settings.
            let settings = CompressedImageSaverSettings::default();

            match context
                .env()
                .get("compressed_texture_format")
                .expect("XXX TODO?")
            {
                #[cfg(feature = "compressed_image_saver")]
                "bcn" => {
                    context
                        .finish_saved::<CompressedImageSaverCtt>(
                            &mut resized_asset,
                            &CompressedImageSaverCtt(CompressedImageSaverCttFormat::Bcn),
                            &settings,
                        )
                        .await
                }
                #[cfg(feature = "compressed_image_saver")]
                "astc" => {
                    context
                        .finish_saved::<CompressedImageSaverCtt>(
                            &mut resized_asset,
                            &CompressedImageSaverCtt(CompressedImageSaverCttFormat::Astc {
                                // XXX TODO: Should be configurable? Maybe needs to go in the
                                // environment.
                                block_width: 4,
                                block_height: 4,
                            }),
                            &settings,
                        )
                        .await
                }
                #[cfg(feature = "compressed_image_saver_universal")]
                "universal" => {
                    context
                        .finish_saved::<CompressedImageSaverUniversal>(
                            &mut resized_asset,
                            &CompressedImageSaverUniversal,
                            &settings,
                        )
                        .await
                }
                unrecognized => {
                    // XXX TODO: This reports a misleading error if the format is
                    // recognized but the relevant feature is not enabled.
                    return Err(BevyError::from(format!(
                        "Unrecognized compressed texture format \"{unrecognized}\"."
                    )));
                }
            }
            .map_err(BevyError::from)
        }
    }

    #[derive(Default, Debug, PartialEq, Reflect)]
    #[reflect(BassetAction, PartialEq, Hash)]
    pub struct ResizeImage {
        pub image: AssetRef<'static>,
        pub scale: f32,
    }

    impl BassetAction for ResizeImage {
        basset_action_version!(crate);
    }

    impl Hash for ResizeImage {
        fn hash<H: Hasher>(&self, state: &mut H) {
            self.image.hash(state);
            // XXX TODO: Hashing f32 action parameters is going to be common.
            // Should we add a wrapper or using something like `ordered_float`?
            self.scale.to_le_bytes().hash(state);
        }
    }

    impl From<ResizeImage> for AssetRef<'static> {
        fn from(value: ResizeImage) -> Self {
            AssetRef::new(value)
        }
    }

    #[derive(TypePath)]
    pub struct ResizeImageFunction;

    fn resize_dynamic_image(image: &DynamicImage, size: UVec2) -> DynamicImage {
        let mut resizer = Resizer::new();

        let resize_alg =
            ResizeOptions::new().resize_alg(ResizeAlg::Convolution(FilterType::Gaussian));

        let mut resized_image = DynamicImage::new(size.x, size.y, image.color());

        resizer
            .resize(image, &mut resized_image, &resize_alg)
            .expect("XXX TODO");

        resized_image
    }

    fn resize_image(image: Image, size: UVec2) -> Image {
        // XXX TODO: Implement these properly.
        let is_srgb = true;
        let render_asset_usages = RenderAssetUsages::default();

        Image::from_dynamic(
            resize_dynamic_image(&image.try_into_dynamic().expect("XXX TODO"), size),
            is_srgb,
            render_asset_usages,
        )
    }

    impl BassetActionFunction for ResizeImageFunction {
        type Action = ResizeImage;
        type Error = BevyError;

        async fn apply(
            &self,
            mut context: ApplyContext<'_>,
            action: &Self::Action,
        ) -> Result<BassetActionOutput, Self::Error> {
            let original_image = context
                .erased_load_dependee(&action.image)
                .await?
                .take::<Image>()
                .ok_or_else(|| BevyError::from("XXX TODO"))?;

            let target_size = original_image.size().as_vec2().mul(action.scale).as_uvec2();

            let resized_image = resize_image(original_image, target_size);

            Ok(context.finish(resized_image))
        }
    }

    #[derive(Debug, PartialEq, Reflect)]
    #[reflect(BassetAction, PartialEq, Hash)]
    pub struct MeshFromHeightmap {
        pub heightmap: AssetRef<'static>,
        pub extents: Aabb3d,
    }

    impl Default for MeshFromHeightmap {
        fn default() -> Self {
            Self {
                heightmap: Default::default(),
                extents: Aabb3d::from_min_max(vec3(-1.0, 0.0, -1.0), Vec3::ONE),
            }
        }
    }

    impl BassetAction for MeshFromHeightmap {
        basset_action_version!(crate);
    }

    impl From<MeshFromHeightmap> for AssetRef<'static> {
        fn from(value: MeshFromHeightmap) -> Self {
            AssetRef::new(value)
        }
    }

    impl MeshFromHeightmap {
        pub fn new(heightmap: impl Into<AssetRef<'static>>) -> Self {
            Self::default().with_heightmap(heightmap)
        }

        pub fn with_heightmap(self, heightmap: impl Into<AssetRef<'static>>) -> Self {
            Self {
                heightmap: heightmap.into(),
                ..self
            }
        }

        pub fn with_extents(self, extents: Aabb3d) -> Self {
            Self { extents, ..self }
        }
    }

    // XXX TODO: Could avoid this if we made an `Aabb3d` with `FloatOrd`.
    impl Hash for MeshFromHeightmap {
        fn hash<H: Hasher>(&self, state: &mut H) {
            self.heightmap.hash(state);
            for f in self
                .extents
                .min
                .to_array()
                .into_iter()
                .chain(self.extents.max.to_array())
            {
                FloatOrd(f).hash(state);
            }
        }
    }

    #[derive(TypePath)]
    pub struct MeshFromHeightmapFunction;

    fn normalize(value: u16) -> f32 {
        (value as f32) * (1.0 / (u16::MAX as f32))
    }

    fn within_extents(extents: Aabb3d, value: Vec3) -> Vec3 {
        let bias = extents.min.to_vec3();
        let scale = extents.max.to_vec3() - extents.min.to_vec3();

        (value * scale) + bias
    }

    impl BassetActionFunction for MeshFromHeightmapFunction {
        type Action = MeshFromHeightmap;
        type Error = BevyError;

        async fn apply(
            &self,
            mut context: ApplyContext<'_>,
            action: &Self::Action,
        ) -> Result<BassetActionOutput, Self::Error> {
            let heightmap = context
                .erased_load_dependee(&action.heightmap)
                .await?
                .take::<Image>()
                .ok_or_else(|| BevyError::from("XXX TODO"))?
                .try_into_dynamic()?
                .to_luma16();

            let vertex_count_x = heightmap.width();
            let vertex_count_y = heightmap.height();

            if (vertex_count_x <= 1) || (vertex_count_y <= 1) {
                return Err(BevyError::from(format!(
                    "XXX TODO: {vertex_count_x} {vertex_count_y}"
                )));
            }

            // Create a vertex for each pixel in the heightmap.
            let vertex_count = (vertex_count_x * vertex_count_y) as usize;
            let mut positions = vec![Vec3::ZERO; vertex_count];
            let mut uvs = vec![Vec2::ZERO; vertex_count];

            for x in 0..vertex_count_x {
                for y in 0..vertex_count_y {
                    let height = normalize(heightmap.get_pixel(x, y)[0]);
                    let u = (x as f32) * ((vertex_count_x - 1) as f32).recip();
                    let v = (y as f32) * ((vertex_count_y - 1) as f32).recip();
                    let position = within_extents(action.extents, Vec3::new(u, height, v));
                    let uv = Vec2::new(u, v);

                    let vertex_index = (x + (y * vertex_count_x)) as usize;
                    positions[vertex_index] = position;
                    uvs[vertex_index] = uv;
                }
            }

            // Create indices so that a square joins each group of four
            // heightmap pixels.
            let square_count_x = vertex_count_x - 1;
            let square_count_y = vertex_count_y - 1;
            let square_count = (square_count_x * square_count_y) as usize;
            let mut squares = vec![[0u32; 6]; square_count];

            for x in 0..square_count_x {
                for y in 0..square_count_y {
                    let i0 = x + (y * vertex_count_x);
                    let i1 = i0 + 1;
                    let i2 = i0 + vertex_count_x;
                    let i3 = i2 + 1;

                    let square_index = (x + (y * square_count_x)) as usize;
                    squares[square_index] = [i0, i2, i1, i1, i2, i3];
                }
            }

            let indices = squares.into_flattened();

            let mesh = Mesh::new(
                bevy::mesh::PrimitiveTopology::TriangleList,
                RenderAssetUsages::default(),
            )
            .with_inserted_attribute(Mesh::ATTRIBUTE_POSITION, positions)
            .with_inserted_attribute(Mesh::ATTRIBUTE_UV_0, uvs)
            .with_inserted_indices(Indices::U32(indices))
            .with_computed_area_weighted_normals();

            Ok(context.finish(mesh))
        }
    }

    #[derive(Debug, PartialEq, Reflect, Default, Hash)]
    #[reflect(BassetAction, PartialEq, Hash)]
    pub struct ColorizeHeightmap {
        pub heightmap: AssetRef<'static>,
    }

    impl BassetAction for ColorizeHeightmap {
        basset_action_version!(crate);
    }

    impl From<ColorizeHeightmap> for AssetRef<'static> {
        fn from(value: ColorizeHeightmap) -> Self {
            AssetRef::new(value)
        }
    }

    impl ColorizeHeightmap {
        pub fn new(heightmap: impl Into<AssetRef<'static>>) -> Self {
            Self {
                heightmap: heightmap.into(),
            }
        }
    }

    #[derive(TypePath)]
    pub struct ColorizeHeightmapFunction;

    impl BassetActionFunction for ColorizeHeightmapFunction {
        type Action = ColorizeHeightmap;
        type Error = BevyError;

        async fn apply(
            &self,
            mut context: ApplyContext<'_>,
            action: &Self::Action,
        ) -> Result<BassetActionOutput, Self::Error> {
            let heightmap = context
                .erased_load_dependee(&action.heightmap)
                .await?
                .take::<Image>()
                .ok_or_else(|| BevyError::from("XXX TODO"))?
                .try_into_dynamic()?
                .to_luma16();

            let w = heightmap.width();
            let h = heightmap.height();

            let mut output = RgbImage::new(w, h);

            let mapping: &[(u16, Rgb<u8>)] = &[
                (10, Rgb([20, 10, 127])),
                (5000, Rgb([60, 100, 60])),
                (6000, Rgb([127, 80, 40])),
                (8000, Rgb([50, 40, 60])),
                (u16::MAX, Rgb([210, 210, 230])),
            ];

            for x in 0..w {
                for y in 0..h {
                    let height = heightmap.get_pixel(x, y)[0];

                    let color = mapping
                        .iter()
                        .find(|(threshold, _)| height <= *threshold)
                        .map(|(_, color)| *color)
                        .unwrap_or(Rgb([255, 0, 255]));

                    output.put_pixel(x, y, color);
                }
            }

            Ok(context.finish(Image::from_dynamic(
                output.into(),
                true,
                RenderAssetUsages::default(),
            )))
        }
    }
}

mod demo {
    use bevy_asset::{io::Writer, saver::SavedAsset, AssetPath, AsyncWriteExt};

    use super::*;

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
}

#[derive(TypePath)]
struct RonAssetLoader {
    registry: TypeRegistryArc,
}

impl RonAssetLoader {
    fn new(registry: TypeRegistryArc) -> Self {
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

        load_context.finish_reflect(reflected, &registry)
    }

    fn extensions(&self) -> &[&str] {
        &["ron"]
    }
}

#[derive(TypePath)]
struct RonAssetSaver {
    registry: TypeRegistryArc,
}

impl RonAssetSaver {
    fn new(registry: TypeRegistryArc) -> Self {
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
struct MeshAssetLoader;

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
struct MeshAssetSaver;

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

mod acme {
    use super::*;
    use bevy::pbr::experimental::meshlet::MeshletMesh3d;
    use bevy_asset::VisitAssetDependencies;

    #[derive(Default, Debug, VisitAssetDependencies, Reflect)]
    pub struct AcmeEntity {
        pub transform: Transform,

        #[dependency]
        pub mesh: Option<Handle<Mesh>>,

        #[dependency]
        pub meshlet_mesh: Option<Handle<MeshletMesh>>,

        // XXX TODO: Think through what it would take to make this `Handle<StandardMaterial>`.
        // Problematic because we're currently converting from `GltfMaterial` to `StandardMaterial`
        // in the action, so we need to make the material a sub-asset. Or maybe that's the wrong
        // approach and we should leave it as `GltfMaterial` until later. Or maybe this is all
        // a red herring - real question is what we do for BSN.
        #[dependency]
        pub material: Option<StandardMaterial>,
    }

    #[derive(Asset, Default, Debug, Reflect)]
    pub struct AcmeScene {
        #[dependency]
        pub entities: Vec<AcmeEntity>,
    }

    fn get_sub_asset<'a, T: Asset>(
        asset: &'a ErasedLoadedAsset,
        sub_asset_handle: &Handle<T>,
    ) -> Result<&'a T, BevyError> {
        asset
            .get_labeled_by_id(sub_asset_handle.id().untyped())
            // XXX TODO: Don't use handle debug?
            .ok_or_else(|| {
                BevyError::from(format!("Couldn't find sub-asset {sub_asset_handle:?}"))
            })?
            .get::<T>()
            // XXX TODO: Better error.
            .ok_or_else(|| {
                BevyError::from(format!("Sub-asset was wrong type {sub_asset_handle:?}"))
            })
    }

    pub fn from_gltf(asset: &ErasedLoadedAsset) -> Result<AcmeScene, BevyError> {
        let mut entities = Vec::<AcmeEntity>::new();

        let gltf = asset.get::<Gltf>().expect("XXX TODO");

        // Add all the root nodes to the stack.
        let mut stack = gltf
            .nodes
            .iter()
            .filter_map(|node_handle| {
                let node = get_sub_asset(asset, node_handle).expect("XXX TODO");

                if node.children.is_empty() {
                    None
                } else {
                    Some((node, node.transform))
                }
            })
            .collect::<Vec<_>>();

        while let Some((node, transform)) = stack.pop() {
            if let Some(mesh_handle) = &node.mesh {
                let mesh = get_sub_asset(asset, mesh_handle)?;

                for primitive in mesh.primitives.iter() {
                    let primitive_mesh = Some(primitive.mesh.clone());
                    let material = if let Some(gltf_material) = &primitive.material {
                        Some(
                            get_sub_asset(asset, gltf_material)
                                .map(bevy::pbr::gltf::standard_material_from_gltf_material)?,
                        )
                    } else {
                        None
                    };

                    entities.push(AcmeEntity {
                        transform,
                        mesh: primitive_mesh,
                        material,
                        ..Default::default()
                    });
                }
            }

            // Push children onto the stack.
            for child_handle in node.children.iter() {
                let child = get_sub_asset(asset, child_handle)?;

                stack.push((child, transform * child.transform));
            }
        }

        Ok(AcmeScene { entities })
    }

    pub fn spawn(
        commands: &mut Commands,
        scene: &AcmeScene,
        parent_entity: Option<Entity>,
        standard_material_assets: &mut Assets<StandardMaterial>,
        meshlet_debug_material_assets: &mut Assets<MeshletDebugMaterial>,
    ) {
        for scene_entity in &scene.entities {
            let mut world_entity = commands.spawn(scene_entity.transform);

            // XXX TODO: Currently this forces the debug #material for meshlets.
            // Should change that to be a scene conversion action. AcmeMaterial
            // will become an enum of standard/debug materials.

            if scene_entity.meshlet_mesh.is_some() {
                world_entity.insert(MeshMaterial3d(
                    meshlet_debug_material_assets.add(MeshletDebugMaterial::default()),
                ));
            } else if let Some(material) = &scene_entity.material {
                world_entity.insert(MeshMaterial3d(
                    standard_material_assets.add(material.clone()),
                ));
            }

            if let Some(mesh) = &scene_entity.mesh {
                world_entity.insert(Mesh3d(mesh.clone()));
            } else if let Some(meshlet_mesh) = &scene_entity.meshlet_mesh {
                world_entity.insert(MeshletMesh3d(meshlet_mesh.clone()));
            } else {
                panic!("Expected mesh or meshlet");
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
struct AssetHandles(Vec<UntypedHandle>);

#[derive(Resource)]
struct AssetPaths {
    regular: Vec<(TypeId, AssetRef<'static>)>,
    scenes: Vec<(AssetRef<'static>, Transform)>,
    // XXX TODO: Maybe better to store as functions to scenes? Then we can don't
    // have to consume them (since `spawn_scene` consumes the `Scene`).
    bsns: Vec<Box<dyn SceneList>>,
}

impl AssetPaths {
    fn publish(&self) -> Vec<PublishDependency> {
        let bsns = self
            .bsns
            .iter()
            .flat_map(|bsn| bsn_dependencies(bsn.as_ref()))
            .collect::<Vec<_>>();

        self.regular
            .iter()
            .map(|(_, path)| path)
            .chain(self.scenes.iter().map(|(path, _)| path))
            .chain(bsns.iter())
            .map(|path| PublishDependency::Load(RootAssetRef::without_label(path.clone())))
            .collect()
    }
}

fn bsn_dependencies(scene: &dyn SceneList) -> Vec<AssetRef<'static>> {
    let mut scene_dependencies = SceneDependencies::default();

    // XXX TODO: This doesn't do anything useful since `HandleTemplate`
    // doesn't register dependencies. Seems to be planned for the future:
    // https://discord.com/channels/691052431525675048/1264881140007702558/1483190850644082759
    scene.register_dependencies(&mut scene_dependencies);

    scene_dependencies
        .iter()
        .map(|scene_dependency| scene_dependency.path.clone())
        .collect()
}

#[allow(unused, reason = "XXX TODO")]
const INLINE_JOIN_STRINGS_RON: &str = r#"
(
    separator: ", ",
    strings: [
        Action((
            name: "basset::action::UppercaseString",
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
            acme::AcmeSceneSpawner(asset_server.load::<acme::AcmeScene>(path.clone())),
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
    string_assets: Res<Assets<demo::StringAsset>>,
    int_assets: Res<Assets<demo::IntAsset>>,
    scene_assets: Res<Assets<acme::AcmeScene>>,
    image_assets: Res<Assets<Image>>,
    gltf_assets: Res<Assets<Gltf>>,
    mut string_events: MessageReader<AssetEvent<demo::StringAsset>>,
    mut int_events: MessageReader<AssetEvent<demo::IntAsset>>,
    mut scene_events: MessageReader<AssetEvent<acme::AcmeScene>>,
    mut image_events: MessageReader<AssetEvent<Image>>,
    mut gltf_events: MessageReader<AssetEvent<Gltf>>,
) {
    print_events(&asset_server, &string_assets, &mut string_events, true);
    print_events(&asset_server, &int_assets, &mut int_events, true);
    print_events(&asset_server, &scene_assets, &mut scene_events, true);
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
    let mut registry = TypeRegistry::default();
    registry.register::<AssetRef<'static>>();
    registry.register::<LoadPath>();

    {
        use ron::{de, ser};

        let a = dbg!(ser::to_string(&ReflectSerializer::new(
            &AssetRef::from(AssetPath::parse("asdf.txt")),
            &registry
        ))
        .expect("TODO"));

        dbg!(ReflectDeserializer::new(&registry)
            .deserialize(&mut de::Deserializer::from_str(&a).expect("XXX TODO"))
            .expect("XXX TODO")
            .try_take::<AssetRef>()
            .expect("XXX TODO"));

        let b = dbg!(ser::to_string(&ReflectSerializer::new(
            &AssetRef::new_with_label(
                LoadPath {
                    path: "asdf.txt".try_into().expect("XXX TODO"),
                    ..Default::default()
                },
                Some("subasset".into()),
            ),
            &registry
        ))
        .expect("TODO"));

        dbg!(ReflectDeserializer::new(&registry)
            .deserialize(&mut de::Deserializer::from_str(&b).expect("XXX TODO"))
            .expect("XXX TODO")
            .try_take::<AssetRef>()
            .expect("XXX TODO"));
    }

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
            // (TypeId::of::<demo::StringAsset>(), "hello.string".into()),
            // (TypeId::of::<demo::StringAsset>(), "world.string".into()),
            // (TypeId::of::<demo::IntAsset>(), "1234.int".into()),
            // (TypeId::of::<demo::IntAsset>(), "int.basset".into()),
            // (TypeId::of::<demo::StringAsset>(), "string.basset".into()),
            // (
            //     TypeId::of::<demo::StringAsset>(),
            //     "string_loader_uppercase.basset".into(),
            // ),
            // (
            //     TypeId::of::<demo::StringAsset>(),
            //     "join_strings.basset".into(),
            // ),
            // (
            //     TypeId::of::<demo::StringAsset>(),
            //     AssetAction2::new(
            //         "basset::action::JoinStrings".into(),
            //         ron::value::RawValue::from_boxed_ron(INLINE_JOIN_STRINGS_RON.into()).unwrap(),
            //         None,
            //     )
            //     .into(),
            // ),
        ],
        scenes: vec![
            (
                "scene_from_gltf_with_dependencies.basset".into(),
                Transform::from_xyz(-2.0, 1.0, 0.0)
                    .looking_to(Dir3::new(vec3(1.0, 0.0, 2.0)).unwrap(), Vec3::Y),
            ),
            (
                "scene_from_gltf.basset".into(),
                Transform::IDENTITY.looking_to(Dir3::new(vec3(1.0, 0.0, 2.0)).unwrap(), Vec3::Y),
            ),
            (
                "meshlet_scene.basset".into(),
                Transform::from_xyz(2.0, 0.0, 0.0)
                    .looking_to(Dir3::new(vec3(1.0, 0.0, 2.0)).unwrap(), Vec3::Y),
            ),
        ],
        bsns: vec![
            Box::new(bsn! {
                MeshletMesh3d(action::MeshletFromMesh::new(
                    action::MeshFromHeightmap::new(
                        action::ResizeImage { image: "heightmaps/Heightmap_08_Island_512.png".into(), scale: 0.5 })
                    )
                )
                template(|context| {
                    let s = context.resource::<AssetServer>();
                    Ok(MeshMaterial3d::<StandardMaterial>(s.add(StandardMaterial {
                        base_color_texture: Some(s.load(action::CompressImage::new(
                            action::ColorizeHeightmap::new("heightmaps/Heightmap_08_Island_512.png")
                        ))),
                        perceptual_roughness: 0.9,
                        ..Default::default()
                    })))
                })
                Transform::from_xyz(-2.0, 0.1, 1.5).with_scale(vec3(0.75, 1.0, 0.75))
            }),
            Box::new(bsn! {
                MeshletMesh3d(action::MeshletFromMesh::new(
                    action::MeshFromHeightmap::new(
                        action::ResizeImage { image: "heightmaps/Heightmap_08_Island_512.png".into(), scale: 0.5 })
                    )
                )
                MeshMaterial3d<MeshletDebugMaterial>(asset_value(MeshletDebugMaterial::default()))
                Transform::from_xyz(0.0, 0.1, 1.5).with_scale(vec3(0.75, 1.0, 0.75))
            }),
            Box::new(bsn! {
                MeshletMesh3d(action::MeshletFromMesh::new("Duck.glb#Mesh0/Primitive0"))
                MeshMaterial3d<StandardMaterial>("Duck.glb#Material0/std")
                Transform::from_xyz(2.0, 0.0, 1.5).looking_to(Dir3::new(vec3(1.0, 0.0, 2.0)).unwrap(), Vec3::Y).with_scale(Vec3::splat(0.01))
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
        // XXX TODO: Avoid `block_on`.
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
        let mut env = FullEnvironment::default();
        env.set("compressed_texture_format", "bcn")
            .expect("XXX TODO");

        AssetPlugin {
            file_path: "examples/asset/basset/assets".to_string(),
            basset_action_source_builder: Some(Arc::new(DevelopmentActionSourceBuilder::new(
                DevelopmentActionSourceSettings::default()
                    .with_file_cache_path("target/basset/cache".into())
                    .with_validate_dependency_cache(args.validate_dependency_cache)
                    .with_validate_action_cache(args.validate_action_cache)
                    .with_action(action::JoinStringsFunction)
                    .with_action(action::UppercaseStringFunction)
                    .with_action(action::AcmeSceneFromGltfFunction)
                    .with_action(action::MeshletFromMeshFunction)
                    .with_action(action::ConvertAcmeSceneMeshesToMeshletsFunction)
                    .with_action(action::CompressImageFunction)
                    .with_action(action::ResizeImageFunction)
                    .with_action(action::MeshFromHeightmapFunction)
                    .with_action(action::ColorizeHeightmapFunction)
                    .with_saver(demo::StringAssetSaver)
                    .with_saver(demo::IntAssetSaver)
                    .with_saver(MeshletMeshSaver)
                    .with_saver(MeshAssetSaver)
                    .with_saver_and_settings(
                        ImageSaver,
                        ImageSaverSettings {
                            // XXX TODO: Review. Not sure if this will be a problem.
                            // It's only for cache action values, so we really want
                            // some default "use the most appropriate format". Maybe
                            // PNG is good enough.
                            format: bevy_image::SaveImageFormatSetting::Format(ImageFormat::Png),
                        },
                    )
                    .with_default_poly_saver(RonAssetSaver::new(registry.clone()))
                    .with_env(env),
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
        MaterialPlugin::<MeshletDebugMaterial>::default(),
        FreeCameraPlugin,
        MeshletPlugin {
            cluster_buffer_slots: 1 << 14,
        },
    ))
    .init_asset::<demo::StringAsset>()
    .init_asset::<demo::IntAsset>()
    .init_asset::<acme::AcmeScene>()
    .register_asset_reflect::<acme::AcmeScene>()
    .register_asset_loader(demo::StringAssetLoader)
    .register_asset_loader(demo::IntAssetLoader)
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
                .add_systems(Update, reload.run_if(on_timer(Duration::from_secs(2))))
                .add_systems(Update, acme::tick_scene_spawners);

            if args.mode == ArgMode::Development {
                app.add_systems(Update, dump.run_if(on_timer(Duration::from_secs(4))));
            }

            app.insert_resource(args).run();
        }

        ArgMode::Publish => {
            app.finish();

            let asset_server = app.world().resource::<AssetServer>();

            let input = PublishInput {
                paths: asset_paths.publish(),
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
