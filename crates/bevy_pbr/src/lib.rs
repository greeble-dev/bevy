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
    SpotLight, StandardMaterial, TransmittedShadowReceiver, UvChannel, VisibleMeshEntities,
    VolumetricFog, VolumetricLight, MAX_JOINTS,
};

pub mod prelude {
    #[doc(hidden)]
    pub use bevy_pbr_internal::{
        environment_map::EnvironmentMapLight, light_consts, AmbientLight, Atmosphere,
        CascadeShadowConfigBuilder, DirectionalLight, DistanceFog, FogFalloff, LightProbe,
        Material, MaterialPlugin, MeshMaterial3d, ParallaxMappingMethod, PointLight, SpotLight,
        StandardMaterial,
    };
}
