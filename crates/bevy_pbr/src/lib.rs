#![expect(missing_docs, reason = "Not all docs are written yet, see #3492.")]
#![cfg_attr(docsrs, feature(doc_auto_cfg))]
#![forbid(unsafe_code)]
#![doc(
    html_logo_url = "https://bevyengine.org/assets/icon.png",
    html_favicon_url = "https://bevyengine.org/assets/icon.png"
)]

pub use bevy_pbr_internal::{
    decal, irradiance_volume, light_consts, queue_material_meshes, wireframe, Atmosphere,
    AtmosphereSettings, CascadeShadowConfigBuilder, CubemapVisibleEntities,
    DefaultOpaqueRendererMethod, DirectionalLight, DirectionalLightShadowMap, DrawMesh,
    ExtendedMaterial, ExtractedPointLight, FogVolume, GlobalClusterableObjectMeta, Lightmap,
    MaterialExtension, MaterialPipeline, MaterialPipelineKey, MeshInputUniform, MeshMaterial3d,
    MeshPipeline, MeshPipelineKey, MeshPipelineViewLayoutKey, MeshUniform, NotShadowCaster,
    NotShadowReceiver, OpaqueRendererMethod, PbrPlugin, PointLight, PointLightShadowMap,
    RenderMeshInstances, ScreenSpaceAmbientOcclusion, ScreenSpaceAmbientOcclusionQualityLevel,
    ScreenSpaceReflections, SetMeshBindGroup, SetMeshViewBindGroup, ShadowFilteringMethod,
    SpotLight, StandardMaterial, TransmittedShadowReceiver, VisibleMeshEntities, VolumetricFog,
    VolumetricLight, MAX_JOINTS,
};

pub use bevy_pbr_interface::UvChannel;

pub mod prelude {
    #[doc(hidden)]
    pub use bevy_pbr_internal::{
        environment_map::EnvironmentMapLight, light_consts, AmbientLight, Atmosphere,
        CascadeShadowConfigBuilder, DirectionalLight, DistanceFog, FogFalloff, LightProbe,
        Material, MaterialPlugin, MeshMaterial3d, ParallaxMappingMethod, PointLight, SpotLight,
        StandardMaterial,
    };
}

/// Experimental features that are not yet finished. Please report any issues you encounter!
///
/// Expect bugs, missing features, compatibility issues, low performance, and/or future breaking changes.
#[cfg(feature = "meshlet")]
pub mod experimental {
    /// Render high-poly 3d meshes using an efficient GPU-driven method.
    /// See [`MeshletPlugin`](meshlet::MeshletPlugin) and [`MeshletMesh`](meshlet::MeshletMesh) for details.
    pub use bevy_pbr_internal::experimental::meshlet;
}
