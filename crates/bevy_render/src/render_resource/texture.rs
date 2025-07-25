use crate::define_atomic_id;
use crate::WgpuWrapper;
use bevy_derive::{Deref, DerefMut};
use bevy_ecs::resource::Resource;
use core::ops::Deref;

define_atomic_id!(TextureId);

/// A GPU-accessible texture.
///
/// May be converted from and dereferences to a wgpu [`Texture`](wgpu::Texture).
/// Can be created via [`RenderDevice::create_texture`](crate::renderer::RenderDevice::create_texture).
///
/// Other options for storing GPU-accessible data are:
/// * [`BufferVec`](crate::render_resource::BufferVec)
/// * [`DynamicStorageBuffer`](crate::render_resource::DynamicStorageBuffer)
/// * [`DynamicUniformBuffer`](crate::render_resource::DynamicUniformBuffer)
/// * [`GpuArrayBuffer`](crate::render_resource::GpuArrayBuffer)
/// * [`RawBufferVec`](crate::render_resource::RawBufferVec)
/// * [`StorageBuffer`](crate::render_resource::StorageBuffer)
/// * [`UniformBuffer`](crate::render_resource::UniformBuffer)
#[derive(Clone, Debug)]
pub struct Texture {
    id: TextureId,
    value: WgpuWrapper<wgpu::Texture>,
}

impl Texture {
    /// Returns the [`TextureId`].
    #[inline]
    pub fn id(&self) -> TextureId {
        self.id
    }

    /// Creates a view of this texture.
    pub fn create_view(&self, desc: &wgpu::TextureViewDescriptor) -> TextureView {
        TextureView::from(self.value.create_view(desc))
    }
}

impl From<wgpu::Texture> for Texture {
    fn from(value: wgpu::Texture) -> Self {
        Texture {
            id: TextureId::new(),
            value: WgpuWrapper::new(value),
        }
    }
}

impl Deref for Texture {
    type Target = wgpu::Texture;

    #[inline]
    fn deref(&self) -> &Self::Target {
        &self.value
    }
}

define_atomic_id!(TextureViewId);

/// Describes a [`Texture`] with its associated metadata required by a pipeline or [`BindGroup`](super::BindGroup).
#[derive(Clone, Debug)]
pub struct TextureView {
    id: TextureViewId,
    value: WgpuWrapper<wgpu::TextureView>,
}

pub struct SurfaceTexture {
    value: WgpuWrapper<wgpu::SurfaceTexture>,
}

impl SurfaceTexture {
    pub fn present(self) {
        self.value.into_inner().present();
    }
}

impl TextureView {
    /// Returns the [`TextureViewId`].
    #[inline]
    pub fn id(&self) -> TextureViewId {
        self.id
    }
}

impl From<wgpu::TextureView> for TextureView {
    fn from(value: wgpu::TextureView) -> Self {
        TextureView {
            id: TextureViewId::new(),
            value: WgpuWrapper::new(value),
        }
    }
}

impl From<wgpu::SurfaceTexture> for SurfaceTexture {
    fn from(value: wgpu::SurfaceTexture) -> Self {
        SurfaceTexture {
            value: WgpuWrapper::new(value),
        }
    }
}

impl Deref for TextureView {
    type Target = wgpu::TextureView;

    #[inline]
    fn deref(&self) -> &Self::Target {
        &self.value
    }
}

impl Deref for SurfaceTexture {
    type Target = wgpu::SurfaceTexture;

    #[inline]
    fn deref(&self) -> &Self::Target {
        &self.value
    }
}

define_atomic_id!(SamplerId);

/// A Sampler defines how a pipeline will sample from a [`TextureView`].
/// They define image filters (including anisotropy) and address (wrapping) modes, among other things.
///
/// May be converted from and dereferences to a wgpu [`Sampler`](wgpu::Sampler).
/// Can be created via [`RenderDevice::create_sampler`](crate::renderer::RenderDevice::create_sampler).
#[derive(Clone, Debug)]
pub struct Sampler {
    id: SamplerId,
    value: WgpuWrapper<wgpu::Sampler>,
}

impl Sampler {
    /// Returns the [`SamplerId`].
    #[inline]
    pub fn id(&self) -> SamplerId {
        self.id
    }
}

impl From<wgpu::Sampler> for Sampler {
    fn from(value: wgpu::Sampler) -> Self {
        Sampler {
            id: SamplerId::new(),
            value: WgpuWrapper::new(value),
        }
    }
}

impl Deref for Sampler {
    type Target = wgpu::Sampler;

    #[inline]
    fn deref(&self) -> &Self::Target {
        &self.value
    }
}

/// A rendering resource for the default image sampler which is set during renderer
/// initialization.
///
/// The [`ImagePlugin`](crate::texture::ImagePlugin) can be set during app initialization to change the default
/// image sampler.
#[derive(Resource, Debug, Clone, Deref, DerefMut)]
pub struct DefaultImageSampler(pub(crate) Sampler);
