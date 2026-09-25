// XXX TODO: Remove when no longer needed.
#![allow(clippy::allow_attributes, reason = "XXX TODO")]

use super::asset::*;

#[cfg(feature = "compressed_image_saver_universal")]
use bevy::image::universal::CompressedImageSaverUniversal;
use bevy::{
    asset::RenderAssetUsages,
    asset::{basset::*, basset_action_version, AssetRef},
    ecs::{error::BevyError, system::RunSystemOnce},
    image::{
        ctt::{CompressedImageSaverCtt, CompressedImageSaverCttFormat},
        CompressedImageSaverSettings,
    },
    math::FloatOrd,
    mesh::Indices,
    pbr::experimental::meshlet::*,
    prelude::*,
    reflect::TypePath,
};
use bevy_image::{ImageSaver, ImageSaverSettings};
use core::ops::Mul;
use core::{
    hash::{Hash, Hasher},
    result::Result,
};
use fast_image_resize::{FilterType, ResizeAlg, ResizeOptions, Resizer};
use image::{DynamicImage, Rgb, RgbImage};

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
            strings.push(context.load_value::<StringAsset>(path).await?.0);
        }

        let joined = strings
            .into_iter()
            .reduce(|l, r| l + &action.separator + &r)
            .unwrap_or_else(|| "".to_owned());

        Ok(context.finish(StringAsset(joined)))
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
        let string = StringAsset(
            context
                .load_value::<StringAsset>(&action.string)
                .await?
                .0
                .to_uppercase(),
        );

        Ok(context.finish(string))
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
        // XXX TODO: Should we check if `MeshletPlugin` is registered so we can
        // return a sensible error?

        let mut mesh = context.load_value::<Mesh>(&action.mesh).await?;

        // XXX TODO: Cut and pasted from `meshlet::from_mesh::validate_input_mesh`.
        // Should be exposed in the meshlet module?
        let required_attributes = [
            Mesh::ATTRIBUTE_POSITION,
            Mesh::ATTRIBUTE_NORMAL,
            Mesh::ATTRIBUTE_UV_0,
        ];

        let unwanted_attributes = mesh
            .attributes()
            .map(|(attribute, _)| *attribute)
            .filter(|attribute| !required_attributes.contains(attribute))
            .collect::<Vec<_>>();

        for unwanted_attribute in unwanted_attributes {
            mesh.remove_attribute(unwanted_attribute.id);
        }

        let meshlet = MeshletMesh::from_mesh(&mesh, action.vertex_position_quantization_factor())?;

        Ok(context.finish(meshlet))
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
            .erased_load_value(&action.image)
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

// XXX TODO: Should this be called `ScaleImage`? Maybe resize is more explicit...
// scale could mean something like changing dynamic range.
#[derive(Default, Debug, PartialEq, Reflect, Hash)]
#[reflect(BassetAction, PartialEq, Hash)]
pub struct ResizeImage {
    pub image: AssetRef<'static>,
    // XXX TODO: Decide whether we should be encouraging `FloatOrd` or not.
    // Avoids having to manually implement hashing, but leaks into serialiation
    // and users initializing the struct directly.
    pub scale: FloatOrd,
}

impl BassetAction for ResizeImage {
    basset_action_version!(crate);
}

impl ResizeImage {
    pub fn new(image: impl Into<AssetRef<'static>>, scale: f32) -> Self {
        Self {
            image: image.into(),
            scale: FloatOrd(scale),
        }
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

    let resize_alg = ResizeOptions::new().resize_alg(ResizeAlg::Convolution(FilterType::Gaussian));

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
            .erased_load_value(&action.image)
            .await?
            .take::<Image>()
            .ok_or_else(|| BevyError::from("XXX TODO"))?;

        let target_size = std::dbg!(original_image
            .size()
            .as_vec2()
            .mul(action.scale.0)
            .as_uvec2());

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
            .erased_load_value(&action.heightmap)
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
            .erased_load_value(&action.heightmap)
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

// XXX TODO: Investigate making this more generic for all materials. Either
// we extend `VisitAssetDependencies` to allow mutation (but how do we get
// from an unknown asset type to that trait?), or use reflection to find
// image handles.
#[derive(Debug, PartialEq, Hash, Reflect)]
#[reflect(BassetAction, PartialEq, Hash)]
pub struct OptimizeStandardMaterial {
    pub material: AssetRef<'static>,
    pub compress_textures: bool,
    pub scale_textures: Option<FloatOrd>,
}

impl BassetAction for OptimizeStandardMaterial {
    basset_action_version!(crate);
}

impl Default for OptimizeStandardMaterial {
    fn default() -> Self {
        Self {
            material: Default::default(),
            compress_textures: true,
            scale_textures: None,
        }
    }
}

impl From<OptimizeStandardMaterial> for AssetRef<'static> {
    fn from(value: OptimizeStandardMaterial) -> Self {
        AssetRef::new(value)
    }
}

#[derive(TypePath)]
pub struct OptimizeStandardMaterialFunction;

fn optimize_texture(
    context: &mut ApplyContext<'_>,
    options: &OptimizeStandardMaterial,
    handle: &mut Option<Handle<Image>>,
) {
    if let Some(handle) = handle {
        if let Some(scale) = options.scale_textures {
            *handle =
                context.load_handle(ResizeImage::new(handle.path().expect("XXX TODO"), scale.0));
        }

        if options.compress_textures {
            *handle = context.load_handle(CompressImage::new(handle.path().expect("XXX TODO")));
        }
    }
}

impl BassetActionFunction for OptimizeStandardMaterialFunction {
    type Action = OptimizeStandardMaterial;
    type Error = BevyError;

    async fn apply(
        &self,
        mut context: ApplyContext<'_>,
        action: &Self::Action,
    ) -> Result<BassetActionOutput, Self::Error> {
        let mut material = context
            .erased_load_value(&action.material)
            .await?
            .take::<StandardMaterial>()
            .expect("XXX TODO");

        // XXX TODO: This misses some textures because I didn't want to
        // faff around with feature flags. In future this should be done
        // in a generic way through `VisitAssetDependencies` or reflection.
        // See comment on `OptimizeStandardMaterial`.
        optimize_texture(&mut context, action, &mut material.base_color_texture);
        optimize_texture(&mut context, action, &mut material.emissive_texture);
        optimize_texture(
            &mut context,
            action,
            &mut material.metallic_roughness_texture,
        );
        optimize_texture(&mut context, action, &mut material.normal_map_texture);
        optimize_texture(&mut context, action, &mut material.occlusion_texture);
        optimize_texture(&mut context, action, &mut material.depth_map);

        Ok(context.finish(material))
    }
}

#[derive(Default, Clone, Debug, PartialEq, Hash, Reflect)]
#[reflect(BassetAction, Default, PartialEq, Hash)]
pub struct OptimizeScene {
    // XXX TODO: Maybe want typed `AssetRef` here? But gets weird because
    // in theory we could support both `WorldAsset` and `DynamicWorld`.
    pub scene: AssetRef<'static>,
    pub convert_meshes_to_meshlets: bool,
    pub compress_textures: bool,
    pub scale_textures: Option<FloatOrd>,
}

impl BassetAction for OptimizeScene {
    basset_action_version!(crate);
}

impl OptimizeScene {
    pub fn with_scene(mut self, scene: impl Into<AssetRef<'static>>) -> Self {
        self.scene = scene.into();
        self
    }
}

impl From<OptimizeScene> for AssetRef<'static> {
    fn from(value: OptimizeScene) -> Self {
        AssetRef::new(value)
    }
}

#[derive(TypePath)]
pub struct OptimizeSceneFunction;

impl BassetActionFunction for OptimizeSceneFunction {
    type Action = OptimizeScene;
    type Error = BevyError;

    async fn apply(
        &self,
        mut context: ApplyContext<'_>,
        action: &Self::Action,
    ) -> Result<BassetActionOutput, Self::Error> {
        // XXX TODO: Consider supporting `DynamicWorld` as well? That would
        // mean we can load `.scn`/`.scn.ron`.
        let mut scene = context
            .erased_load_value(&action.scene)
            .await?
            .take::<WorldAsset>()
            .expect("XXX TODO")
            .world;

        // XXX TODO: There's some lifetime issues below due to using queries.
        // Given that we have to convert `World` to `DynamicWorld` at the end
        // for serialization, maybe we should use `DynamicWorld` directly?
        // That would avoid the lifetime issues... but on the other hand
        // using queries is quite elegant?

        if action.convert_meshes_to_meshlets {
            let asset_server = context.asset_server().clone();

            scene
                .run_system_once(
                    move |mut commands: Commands, mut query: Query<(Entity, &Mesh3d)>| {
                        for (entity, mesh) in query.iter_mut() {
                            // XXX TODO: We should be using `ApplyContext::load_handle`,
                            // not `AssetServer::load`. But I couldn't work out how to
                            // borrow `ApplyContext` correctly - keeps complaining that
                            // the closure can outlive this function. Try again?
                            let meshlet = MeshletMesh3d(asset_server.load(MeshletFromMesh::new(
                                mesh.0.path().expect("XXX TODO").clone(),
                            )));

                            commands.entity(entity).remove::<Mesh3d>().insert(meshlet);
                        }
                    },
                )
                .expect("XXX TODO");
        }

        if action.compress_textures || action.scale_textures.is_some() {
            // XXX TODO: As above, annoying borrow issues.
            let asset_server = context.asset_server().clone();
            let action = action.clone();

            // XXX TODO: Hard-coding this to `StandardMaterial` is inadequate.
            // See comment on `OptimizeStandardMaterial`.
            scene
                .run_system_once(
                    move |mut query: Query<&mut MeshMaterial3d<StandardMaterial>>| {
                        for mut material in query.iter_mut() {
                            // XXX TODO: As above, should be avoiding `AssetServer::load`.
                            material.0 = asset_server.load(OptimizeStandardMaterial {
                                material: material.0.path().expect("XXX TODO").into(),
                                compress_textures: action.compress_textures,
                                scale_textures: action.scale_textures,
                            });
                        }
                    },
                )
                .expect("XXX TODO");
        }

        scene.flush();

        let asset =
            DynamicWorld::from_world_with(&scene, &context.asset_server().type_registry().read());

        Ok(context.finish(asset))
    }
}

pub fn development_action_source_settings(app: &App) -> DevelopmentActionSourceSettings {
    let registry = app
        .world()
        .get_resource::<AppTypeRegistry>()
        .cloned()
        .expect("XXX TODO")
        .0;

    let env = FullEnvironment::new([("compressed_texture_format", "bcn")]).expect("XXX TODO?");

    DevelopmentActionSourceSettings::default()
        .with_file_cache_path("target/basset/cache".into())
        .with_action(JoinStringsFunction)
        .with_action(UppercaseStringFunction)
        .with_action(MeshletFromMeshFunction)
        .with_action(CompressImageFunction)
        .with_action(ResizeImageFunction)
        .with_action(MeshFromHeightmapFunction)
        .with_action(ColorizeHeightmapFunction)
        .with_action(OptimizeStandardMaterialFunction)
        .with_action(OptimizeSceneFunction)
        .with_saver(StringAssetSaver)
        .with_saver(IntAssetSaver)
        .with_saver(MeshletMeshSaver)
        .with_saver(MeshAssetSaver)
        .with_saver(DynamicWorldAssetSaver::new(registry))
        .with_saver_and_settings(
            ImageSaver,
            ImageSaverSettings {
                // XXX TODO: Review. Not sure if this will be a problem.
                // It's only for cache action values, so we really want
                // some default "use the most appropriate format".
                format: bevy_image::SaveImageFormatSetting::Format(ImageFormat::Png),
            },
        )
        .with_env(env)
}
