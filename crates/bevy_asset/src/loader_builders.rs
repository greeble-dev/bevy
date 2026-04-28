//! Implementations of the builder-pattern used for loading dependent assets via
//! [`LoadContext::load_builder`].

use crate::{
<<<<<<< HEAD
    io::Reader, meta::Settings, Asset, AssetLoadError, AssetRef, ErasedAssetLoader,
    ErasedLoadedAsset, Handle, LoadContext, LoadDirectError, LoadedAsset, LoadedUntypedAsset,
    UntypedHandle,
};
use alloc::{borrow::ToOwned, boxed::Box, string::String, sync::Arc};
use core::any::TypeId;
use serde::Serialize;
=======
    io::Reader,
    meta::{loader_settings_meta_transform, MetaTransform, Settings},
    Asset, AssetLoadError, AssetPath, ErasedAssetLoader, ErasedLoadedAsset, Handle, LoadContext,
    LoadDirectError, LoadedAsset, LoadedUntypedAsset, UntypedHandle,
};
use alloc::{borrow::ToOwned, boxed::Box, sync::Arc};
use core::any::{type_name, TypeId};
use std::path::Path;
use tracing::error;
>>>>>>> main

// Utility type for handling the sources of reader references
enum ReaderRef<'a> {
    Borrowed(&'a mut dyn Reader),
    Boxed(Box<dyn Reader + 'a>),
}

impl ReaderRef<'_> {
    pub fn as_mut(&mut self) -> &mut dyn Reader {
        match self {
            ReaderRef::Borrowed(r) => &mut **r,
            ReaderRef::Boxed(b) => &mut **b,
        }
    }
}

/// A builder for loading nested assets inside a [`LoadContext`].
pub struct NestedLoadBuilder<'ctx, 'builder> {
    load_context: &'builder mut LoadContext<'ctx>,
<<<<<<< HEAD
    // XXX TODO: Heinous. See `AssetRef::with_settings`.
    settings: Option<String>,
    typing: T,
    mode: M,
=======
    /// A function to modify the meta for an asset loader. In practice, this just mutates the loader
    /// settings of a load.
    meta_transform: Option<MetaTransform>,
    /// Whether unapproved paths are allowed to be loaded.
    override_unapproved: bool,
>>>>>>> main
}

impl<'ctx, 'builder> NestedLoadBuilder<'ctx, 'builder> {
    pub(crate) fn new(load_context: &'builder mut LoadContext<'ctx>) -> Self {
        NestedLoadBuilder {
            load_context,
<<<<<<< HEAD
            settings: None,
            typing: StaticTyped(()),
            mode: Deferred(()),
=======
            meta_transform: None,
            override_unapproved: false,
>>>>>>> main
        }
    }
}

<<<<<<< HEAD
impl<'ctx, 'builder, T: sealed::Typing, M: sealed::Mode> NestedLoader<'ctx, 'builder, T, M> {
    /// Configure the settings used to load the asset.
    ///
    /// If the settings type `S` does not match the settings expected by `A`'s asset loader, an error will be printed to the log
    /// and the asset load will fail.
    #[must_use]
    pub fn with_settings<S: Settings + Serialize + Default>(
        mut self,
        settings: impl Fn(&mut S) + Send + Sync + 'static,
    ) -> Self {
        assert!(self.settings.is_none());

        let mut settings_value = S::default();
        settings(&mut settings_value);

        let settings_ron = ron::ser::to_string(&settings_value).expect("XXX TODO");

        self.settings = Some(settings_ron);
        self
    }

    /// When [`load`]ing, you must pass in the asset type as a type parameter
    /// statically.
    ///
    /// If you don't know the type statically (at compile time), consider
    /// [`with_dynamic_type`] or [`with_unknown_type`].
    ///
    /// [`load`]: Self::load
    /// [`with_dynamic_type`]: Self::with_dynamic_type
    /// [`with_unknown_type`]: Self::with_unknown_type
    #[must_use]
    pub fn with_static_type(self) -> NestedLoader<'ctx, 'builder, StaticTyped, M> {
        NestedLoader {
            load_context: self.load_context,
            settings: self.settings,
            typing: StaticTyped(()),
            mode: self.mode,
        }
=======
impl<'ctx, 'builder> NestedLoadBuilder<'ctx, 'builder> {
    /// Use the given `settings` function to override the asset's [`AssetLoader`] settings.
    ///
    /// The type `S` must match the configured [`AssetLoader::Settings`] or `settings` changes will
    /// be ignored and an error will be printed to the log.
    ///
    /// Repeatedly calling this method will "chain" the operations (matching the order of these
    /// calls).
    ///
    /// [`AssetLoader`]: crate::AssetLoader
    /// [`AssetLoader::Settings`]: crate::AssetLoader::Settings
    #[must_use]
    pub fn with_settings<S: Settings>(
        mut self,
        settings: impl Fn(&mut S) + Send + Sync + 'static,
    ) -> Self {
        let new_transform = loader_settings_meta_transform(settings);
        if let Some(prev_transform) = self.meta_transform.take() {
            self.meta_transform = Some(Box::new(move |meta| {
                prev_transform(meta);
                new_transform(meta);
            }));
        } else {
            self.meta_transform = Some(new_transform);
        }
        self
    }

    /// Loads from unapproved paths are allowed, even if
    /// [`AssetPlugin::unapproved_path_mode`](crate::AssetPlugin::unapproved_path_mode) is
    /// [`Deny`](crate::UnapprovedPathMode::Deny).
    #[must_use = "the load doesn't start until LoadBuilder has been consumed"]
    pub fn override_unapproved(mut self) -> Self {
        self.override_unapproved = true;
        self
    }

    /// Loads the provided path as the given type and returns the handle.
    ///
    /// This is a "deferred" load, meaning the caller will not have access to the loaded data; to
    /// access the loaded data, use [`Self::load_value`].
    pub fn load<'a, A: Asset>(self, path: impl Into<AssetPath<'a>>) -> Handle<A> {
        // The doc comment slightly lies: if `LoadContext::should_load_dependencies` is true, the
        // load will not be started, but the matching handle will still be returned. The caller
        // can't tell the difference.
        self.load_internal(TypeId::of::<A>(), Some(type_name::<A>()), path.into())
            .typed_debug_checked()
>>>>>>> main
    }

    /// Loads the provided path as the given type and returns the handle.
    ///
<<<<<<< HEAD
    /// [`load`]: Self::load
    #[must_use]
    pub fn with_dynamic_type(
        self,
        asset_type_id: TypeId,
    ) -> NestedLoader<'ctx, 'builder, DynamicTyped, M> {
        NestedLoader {
            load_context: self.load_context,
            settings: self.settings,
            typing: DynamicTyped { asset_type_id },
            mode: self.mode,
        }
=======
    /// This is a "deferred" load, meaning the caller will not have access to the loaded data; to
    /// access the loaded data, use [`Self::load_erased_value`].
    pub fn load_erased<'a>(self, type_id: TypeId, path: impl Into<AssetPath<'a>>) -> UntypedHandle {
        self.load_internal(type_id, None, path.into())
>>>>>>> main
    }

    /// Loads the provided path with an unknown type (which is guessed based on the path or meta
    /// file).
    ///
<<<<<<< HEAD
    /// [`load`]: Self::load
    #[must_use]
    pub fn with_unknown_type(self) -> NestedLoader<'ctx, 'builder, UnknownTyped, M> {
        NestedLoader {
            load_context: self.load_context,
            settings: self.settings,
            typing: UnknownTyped(()),
            mode: self.mode,
        }
    }

    // convert between `M`s

    /// When [`load`]ing, create only asset handles, rather than returning the
    /// actual asset.
    ///
    /// [`load`]: Self::load
    pub fn deferred(self) -> NestedLoader<'ctx, 'builder, T, Deferred> {
        NestedLoader {
            load_context: self.load_context,
            settings: self.settings,
            typing: self.typing,
            mode: Deferred(()),
        }
    }

    /// The [`load`] call itself will load an asset, rather than scheduling the
    /// loading to happen later.
    ///
    /// This gives you access to the loaded asset, but requires you to be in an
    /// async context, and be able to `await` the resulting future.
    ///
    /// [`load`]: Self::load
    #[must_use]
    pub fn immediate<'c>(self) -> NestedLoader<'ctx, 'builder, T, Immediate<'builder, 'c>> {
        NestedLoader {
            load_context: self.load_context,
            settings: self.settings,
            typing: self.typing,
            mode: Immediate { reader: None },
        }
    }
}

// deferred loading logic

impl NestedLoader<'_, '_, StaticTyped, Deferred> {
    /// Retrieves a handle for the asset at the given path and adds that path as
    /// a dependency of this asset.
    ///
    /// This requires you to know the type of asset statically.
    /// - If you have runtime info for what type of asset you're loading (e.g. a
    ///   [`TypeId`]), use [`with_dynamic_type`].
    /// - If you do not know at all what type of asset you're loading, use
    ///   [`with_unknown_type`].
    ///
    /// [`with_dynamic_type`]: Self::with_dynamic_type
    /// [`with_unknown_type`]: Self::with_unknown_type
    pub fn load<'c, A: Asset>(self, path: impl Into<AssetRef<'c>>) -> Handle<A> {
=======
    /// This is a "deferred" load, meaning the caller will not have access to the loaded data; to
    /// access the loaded data, use [`Self::load_untyped_value`].
    pub fn load_untyped<'a>(self, path: impl Into<AssetPath<'a>>) -> Handle<LoadedUntypedAsset> {
>>>>>>> main
        let path = path.into().to_owned();
        // XXX TODO: How to restore this? Do we need a way to validate `AssetRef`?
        // if path.path() == Path::new("") {
        //     error!("Attempted to load an asset with an empty path \"{path}\"!");
        //     return Handle::default();
        // }
        let handle = if self.load_context.should_load_dependencies {
<<<<<<< HEAD
            self.load_context.asset_server.load_with_meta_transform(
                path.clone(),
                self.settings,
                (),
                true,
            )
        } else {
            self.load_context
                .asset_server
                .get_or_create_path_handle(path.clone(), None)
        };
        // `load` and `get_or_create_path_handle` always returns a Strong
        // variant, so we are safe to unwrap.
        let index = (&handle).try_into().unwrap();
        self.load_context.dependencies.insert(index);
        handle
    }
}

impl NestedLoader<'_, '_, DynamicTyped, Deferred> {
    /// Retrieves a handle for the asset at the given path and adds that path as
    /// a dependency of this asset.
    ///
    /// This requires you to pass in the asset type ID into
    /// [`with_dynamic_type`].
    ///
    /// [`with_dynamic_type`]: Self::with_dynamic_type
    pub fn load<'p>(self, path: impl Into<AssetRef<'p>>) -> UntypedHandle {
        let path = path.into().to_owned();
        // XXX TODO: How to restore this? Do we need a way to validate `AssetRef`?
        // if path.path() == Path::new("") {
        //     error!("Attempted to load an asset with an empty path \"{path}\"!");
        //     return UntypedHandle::default_for_type(self.typing.asset_type_id);
        // }
        let handle = if self.load_context.should_load_dependencies {
            self.load_context
                .asset_server
                .load_with_meta_transform_erased(
                    path,
                    self.typing.asset_type_id,
                    None,
                    self.settings,
=======
            self.load_context
                .asset_server
                .load_unknown_type_with_meta_transform(
                    path,
                    self.meta_transform,
>>>>>>> main
                    (),
                    self.override_unapproved,
                )
        } else {
            self.load_context
                .asset_server
<<<<<<< HEAD
                .get_or_create_path_handle_erased(
                    path,
                    self.typing.asset_type_id,
                    None,
                    // XXX TODO: Review. Meta transform parameter was replaced with `None`.
                    None,
                )
        };
        // `load_with_meta_transform_erased` and `get_or_create_path_handle_erased` always returns a
        // Strong variant, so we are safe to unwrap.
        let index = (&handle).try_into().unwrap();
        self.load_context.dependencies.insert(index);
        handle
    }
}

impl NestedLoader<'_, '_, UnknownTyped, Deferred> {
    /// Retrieves a handle for the asset at the given path and adds that path as
    /// a dependency of this asset.
    ///
    /// This will infer the asset type from metadata.
    pub fn load<'p>(self, path: impl Into<AssetRef<'p>>) -> Handle<LoadedUntypedAsset> {
        let path = path.into().to_owned();
        // XXX TODO: How to restore this? Do we need a way to validate `AssetRef`?
        // if path.path() == Path::new("") {
        //     error!("Attempted to load an asset with an empty path \"{path}\"!");
        //     return Handle::default();
        // }
        let handle = if self.load_context.should_load_dependencies {
            self.load_context
                .asset_server
                .load_unknown_type_with_meta_transform(
                    path.temporary_path_workaround(),
                    // XXX TODO: Review. Meta transform parameter was replaced with `None`.
                    None,
                    (),
                    false,
                )
        } else {
            self.load_context
                .asset_server
                .get_or_create_path_handle(path.clone(), None)
=======
                .get_or_create_path_handle(path, self.meta_transform)
>>>>>>> main
        };
        // `load_unknown_type_with_meta_transform` and `get_or_create_path_handle` always returns a
        // Strong variant, so we are safe to unwrap.
        let index = (&handle).try_into().unwrap();
        self.load_context.dependencies.insert(index);
        handle
    }

    /// Loads the provided path as the given type, returning the loaded data.
    ///
    /// This load is async and therefore needs to be awaited before returning the loaded data.
    pub async fn load_value<'a, A: Asset>(
        self,
        path: impl Into<AssetPath<'a>>,
    ) -> Result<LoadedAsset<A>, LoadDirectError> {
        self.load_typed_value_internal(path.into().into_owned(), None)
            .await
    }

    /// Loads the provided path as the given type, returning the loaded data.
    ///
    /// This load is async and therefore needs to be awaited before returning the loaded data.
    pub async fn load_erased_value<'a>(
        self,
<<<<<<< HEAD
        path: &AssetRef<'static>,
        asset_type_id: Option<TypeId>,
=======
        type_id: TypeId,
        path: impl Into<AssetPath<'a>>,
    ) -> Result<ErasedLoadedAsset, LoadDirectError> {
        self.load_value_internal(Some(type_id), &path.into().into_owned(), None)
            .await
            .map(|(_, asset)| asset)
    }

    /// Loads the provided path with an unknown type (which is guessed based on the path or meta
    /// file), returning the loaded data.
    ///
    /// This load is async and therefore needs to be awaited before returning the loaded data.
    pub async fn load_untyped_value<'a>(
        self,
        path: impl Into<AssetPath<'a>>,
    ) -> Result<ErasedLoadedAsset, LoadDirectError> {
        self.load_value_internal(None, &path.into().into_owned(), None)
            .await
            .map(|(_, asset)| asset)
    }

    /// Loads the given type from the given `reader`, returning the loaded data.
    ///
    /// This load is async and therefore needs to be awaited before returning the loaded data. The
    /// provided path determines the path used for handles of subassets, as well as any relative
    /// paths of assets used by the nested loader.
    pub async fn load_value_from_reader<'a, A: Asset>(
        self,
        path: impl Into<AssetPath<'a>>,
        reader: &'builder mut dyn Reader,
    ) -> Result<LoadedAsset<A>, LoadDirectError> {
        self.load_typed_value_internal(path.into().into_owned(), Some(reader))
            .await
    }

    /// Loads the given type from the given `reader`, returning the loaded data.
    ///
    /// This load is async and therefore needs to be awaited before returning the loaded data. The
    /// provided path determines the path used for handles of subassets, as well as any relative
    /// paths of assets used by the nested loader.
    pub async fn load_erased_value_from_reader<'a>(
        self,
        type_id: TypeId,
        path: impl Into<AssetPath<'a>>,
        reader: &'builder mut dyn Reader,
    ) -> Result<ErasedLoadedAsset, LoadDirectError> {
        self.load_value_internal(Some(type_id), &path.into().into_owned(), Some(reader))
            .await
            .map(|(_, asset)| asset)
    }

    /// Loads an asset from the given `reader` with an unknown type (which is guessed based on the
    /// path or meta file), returning the loaded data.
    ///
    /// This load is async and therefore needs to be awaited before returning the loaded data. The
    /// provided path determines the path used for handles of subassets, as well as any relative
    /// paths of assets used by the nested loader.
    pub async fn load_untyped_value_from_reader<'a>(
        self,
        path: impl Into<AssetPath<'a>>,
        reader: &'builder mut dyn Reader,
    ) -> Result<ErasedLoadedAsset, LoadDirectError> {
        self.load_value_internal(None, &path.into().into_owned(), Some(reader))
            .await
            .map(|(_, asset)| asset)
    }

    /// Acquires the handle for the given type and path, and if necessary, begins a corresponding
    /// (deferred) load.
    fn load_internal<'a>(
        self,
        type_id: TypeId,
        type_name: Option<&str>,
        path: AssetPath<'a>,
    ) -> UntypedHandle {
        let path = path.to_owned();
        if path.path() == Path::new("") {
            error!("Attempted to load an asset with an empty path \"{path}\"!");
            return UntypedHandle::default_for_type(type_id);
        }
        let handle = if self.load_context.should_load_dependencies {
            self.load_context.asset_server.load_with_meta_transform(
                path,
                type_id,
                type_name,
                self.meta_transform,
                (),
                self.override_unapproved,
            )
        } else {
            self.load_context
                .asset_server
                .get_or_create_path_handle_erased(path, type_id, type_name, self.meta_transform)
        };
        // `load_with_meta_transform` and `get_or_create_path_handle` always returns a Strong
        // variant, so we are safe to unwrap.
        let index = (&handle).try_into().unwrap();
        self.load_context.dependencies.insert(index);
        handle
    }

    /// Creates a future to do a nested load.
    ///
    /// The type is either provided, or it is deduced from the path or meta file. If `reader` is
    /// [`Some`], the load reads from the provided reader. Otherwise, the asset is loaded from
    /// `path`.
    async fn load_value_internal(
        self,
        type_id: Option<TypeId>,
        path: &AssetPath<'static>,
        reader: Option<&'builder mut dyn Reader>,
>>>>>>> main
    ) -> Result<(Arc<dyn ErasedAssetLoader>, ErasedLoadedAsset), LoadDirectError> {
        // XXX TODO: Support `AssetRef`.
        let path = &path.temporary_path_workaround();

        // XXX TODO: How to restore this? Do we need a way to validate `AssetRef`?
        // if path.path() == Path::new("") {
        //     error!("Attempted to load an asset with an empty path \"{path}\"!");
        //     return Err(LoadDirectError::EmptyPath(path.clone_owned()));
        // }
        if path.label().is_some() {
            return Err(LoadDirectError::RequestedSubasset(path.clone()));
        }
        self.load_context
            .asset_server
            .write_infos()
            .stats
            .started_load_tasks += 1;
<<<<<<< HEAD
        let (meta, loader, mut reader) = if let Some(reader) = self.mode.reader {
            let loader = if let Some(asset_type_id) = asset_type_id {
=======
        let (mut meta, loader, mut reader) = if let Some(reader) = reader {
            let loader = if let Some(type_id) = type_id {
>>>>>>> main
                self.load_context
                    .asset_server
                    .get_asset_loader_with_asset_type_id(type_id)
                    .await
                    .map_err(|error| LoadDirectError::LoadError {
                        dependency: path.clone().into(),
                        error: error.into(),
                    })?
            } else {
                self.load_context
                    .asset_server
                    .get_path_asset_loader(path)
                    .await
                    .map_err(|error| LoadDirectError::LoadError {
                        dependency: path.clone().into(),
                        error: error.into(),
                    })?
            };
            let meta = loader.default_meta();
            (meta, loader, ReaderRef::Borrowed(reader))
        } else {
            let (meta, loader, reader) = self
                .load_context
                .asset_server
                .get_meta_loader_and_reader(path, type_id)
                .await
                .map_err(|error| LoadDirectError::LoadError {
                    dependency: path.clone().into(),
                    error,
                })?;
            (meta, loader, ReaderRef::Boxed(reader))
        };

        let asset = self
            .load_context
            .load_direct_internal(
                path.clone(),
                meta.loader_settings().expect("meta corresponds to a load"),
                &*loader,
                reader.as_mut(),
                meta.processed_info().as_ref(),
            )
            .await?;
        Ok((loader, asset))
    }

    /// Same as [`Self::load_value_internal`], but with a generic to ensure the returned handle type
    /// is correct.
    #[expect(
        clippy::result_large_err,
        reason = "we need to give the user the correct error type"
    )]
    async fn load_typed_value_internal<A: Asset>(
        self,
<<<<<<< HEAD
        path: impl Into<AssetRef<'p>>,
=======
        path: AssetPath<'static>,
        reader: Option<&'builder mut dyn Reader>,
>>>>>>> main
    ) -> Result<LoadedAsset<A>, LoadDirectError> {
        self.load_value_internal(Some(TypeId::of::<A>()), &path, reader)
            .await
            .and_then(move |(loader, untyped_asset)| {
                untyped_asset
                    .downcast::<A>()
                    .map_err(|_| LoadDirectError::LoadError {
                        dependency: path.clone(),
                        error: AssetLoadError::RequestedHandleTypeMismatch {
                            path,
                            requested: TypeId::of::<A>(),
                            // XXX TODO: Needs work now that asset_type_name is Option. Could use untyped_asset.asset_type_name()? But needs refactoring as that's already been moved.
                            actual_asset_name: "", // loader.asset_type_name(),
                            loader_name: loader.type_path(),
                        },
                    })
            })
    }
}
<<<<<<< HEAD

impl NestedLoader<'_, '_, DynamicTyped, Immediate<'_, '_>> {
    /// Attempts to load the asset at the given `path` immediately.
    ///
    /// This requires you to pass in the asset type ID into
    /// [`with_dynamic_type`].
    ///
    /// [`with_dynamic_type`]: Self::with_dynamic_type
    pub async fn load<'p>(
        self,
        path: impl Into<AssetRef<'p>>,
    ) -> Result<ErasedLoadedAsset, LoadDirectError> {
        let path = path.into().into_owned();
        let asset_type_id = Some(self.typing.asset_type_id);
        self.load_internal(&path, asset_type_id)
            .await
            .map(|(_, asset)| asset)
    }
}

impl NestedLoader<'_, '_, UnknownTyped, Immediate<'_, '_>> {
    /// Attempts to load the asset at the given `path` immediately.
    ///
    /// This will infer the asset type from metadata.
    pub async fn load<'p>(
        self,
        path: impl Into<AssetRef<'p>>,
    ) -> Result<ErasedLoadedAsset, LoadDirectError> {
        let path = path.into().into_owned();
        self.load_internal(&path, None)
            .await
            .map(|(_, asset)| asset)
    }
}
=======
>>>>>>> main
