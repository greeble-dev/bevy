//! Implementations of the builder-pattern used for loading dependent assets via
//! [`LoadContext::load_builder`].

use crate::{
    basset::RootAssetRef,
    io::Reader,
    meta::{ProcessedInfo, Settings},
    Asset, AssetLoadError, AssetPath, AssetRef, ErasedAssetLoader, ErasedLoadedAsset, Handle,
    LoadContext, LoadDirectError, LoadedAsset, LoadedUntypedAsset, UntypedHandle,
};
use alloc::{borrow::ToOwned, boxed::Box, string::String, sync::Arc};
use core::any::{type_name, TypeId};
use serde::Serialize;
use std::path::Path;
use tracing::error;

// Utility type for handling the sources of reader references
#[expect(unused, reason = "XXX TODO: Decide if this is needed.")]
enum ReaderRef<'a> {
    Borrowed(&'a mut dyn Reader),
    Boxed(Box<dyn Reader + 'a>),
}

impl ReaderRef<'_> {
    #[expect(unused, reason = "XXX TODO: Decide if this is needed.")]
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
    // XXX TODO: Heinous. See `AssetRef::with_settings`.
    settings: Option<String>,
    /// Whether unapproved paths are allowed to be loaded.
    override_unapproved: bool,
}

impl<'ctx, 'builder> NestedLoadBuilder<'ctx, 'builder> {
    pub(crate) fn new(load_context: &'builder mut LoadContext<'ctx>) -> Self {
        NestedLoadBuilder {
            load_context,
            settings: None,
            override_unapproved: false,
        }
    }
}

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
    pub fn load<'a, A: Asset>(self, path: impl Into<AssetRef<'a>>) -> Handle<A> {
        // The doc comment slightly lies: if `LoadContext::should_load_dependencies` is true, the
        // load will not be started, but the matching handle will still be returned. The caller
        // can't tell the difference.
        self.load_internal(TypeId::of::<A>(), Some(type_name::<A>()), path.into())
            .typed_debug_checked()
    }

    /// Loads the provided path as the given type and returns the handle.
    ///
    /// This is a "deferred" load, meaning the caller will not have access to the loaded data; to
    /// access the loaded data, use [`Self::load_erased_value`].
    pub fn load_erased<'a>(self, type_id: TypeId, path: impl Into<AssetRef<'a>>) -> UntypedHandle {
        self.load_internal(type_id, None, path.into())
    }

    /// Loads the provided path with an unknown type (which is guessed based on the path or meta
    /// file).
    ///
    /// This is a "deferred" load, meaning the caller will not have access to the loaded data; to
    /// access the loaded data, use [`Self::load_untyped_value`].
    pub fn load_untyped<'a>(self, path: impl Into<AssetPath<'a>>) -> Handle<LoadedUntypedAsset> {
        let path = path.into().to_owned();
        if path.path() == Path::new("") {
            error!("Attempted to load an asset with an empty path \"{path}\"!");
            return Handle::default();
        }
        let handle = if self.load_context.should_load_dependencies {
            self.load_context
                .asset_server
                .load_unknown_type_with_meta_transform(
                    path,
                    // XXX TODO: Decide what to do here. This used to be `self.meta_transform`,
                    // but that got replaced with settings.
                    None,
                    (),
                    self.override_unapproved,
                )
        } else {
            self.load_context
                .asset_server
                .get_or_create_path_handle(path, None) // XXX TODO: Removed `self.meta_transform`.
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
        path: impl Into<AssetRef<'a>>,
    ) -> Result<LoadedAsset<A>, LoadDirectError> {
        self.load_typed_value_internal(path.into().into_owned())
            .await
    }

    /// Loads the provided path as the given type, returning the loaded data.
    ///
    /// This load is async and therefore needs to be awaited before returning the loaded data.
    pub async fn load_erased_value<'a>(
        self,
        type_id: TypeId,
        path: impl Into<AssetRef<'a>>,
    ) -> Result<ErasedLoadedAsset, LoadDirectError> {
        self.load_value_internal(Some(type_id), &path.into().into_owned())
            .await
            .map(|(_, asset)| asset)
    }

    /// Loads the provided path with an unknown type (which is guessed based on the path or meta
    /// file), returning the loaded data.
    ///
    /// This load is async and therefore needs to be awaited before returning the loaded data.
    pub async fn load_untyped_value<'a>(
        self,
        path: impl Into<AssetRef<'a>>,
    ) -> Result<ErasedLoadedAsset, LoadDirectError> {
        self.load_value_internal(None, &path.into().into_owned())
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
        self.load_typed_value_from_reader_internal(path.into().into_owned(), reader)
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
        self.load_value_from_reader_internal(Some(type_id), &path.into().into_owned(), reader)
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
        self.load_value_from_reader_internal(None, &path.into().into_owned(), reader)
            .await
            .map(|(_, asset)| asset)
    }

    /// Acquires the handle for the given type and path, and if necessary, begins a corresponding
    /// (deferred) load.
    fn load_internal<'a>(
        self,
        type_id: TypeId,
        type_name: Option<&str>,
        path: AssetRef<'a>,
    ) -> UntypedHandle {
        let mut path = path.to_owned();

        // XXX TODO: Review. Arguably should be done by `load_with_meta_transform`
        // and `get_or_create_path_handle_erased.
        if let Some(settings) = self.settings {
            path = path.with_settings(settings);
        }

        if let Err(err) = path.action().validate() {
            error!("{}", err);
            return UntypedHandle::default_for_type(type_id);
        }
        let handle = if self.load_context.should_load_dependencies {
            self.load_context.asset_server.load_with_meta_transform(
                path,
                type_id,
                type_name,
                None,
                (),
                self.override_unapproved,
            )
        } else {
            self.load_context
                .asset_server
                .get_or_create_path_handle_erased(path, type_id, type_name, None)
            // XXX TODO: Removed `self.meta_transform`.
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
        // XXX TODO: Check if we need to support this. Probably should be handled
        // by `LoadPath` having an optional type name so this can all be resolved
        // earlier.
        _type_id: Option<TypeId>,
        path: &AssetRef<'static>,
    ) -> Result<(Option<Arc<dyn ErasedAssetLoader>>, ErasedLoadedAsset), LoadDirectError> {
        // XXX TODO: How to restore this? We now have `BassetAction::validate()`, but
        // how do we map this onto a `LoadDirectError`?
        // if path.path() == Path::new("") {
        //     error!("Attempted to load an asset with an empty path \"{path}\"!");
        //     return Err(LoadDirectError::EmptyPath(path.clone_owned()));
        // }
        let Ok(path) = RootAssetRef::try_from(path.clone()) else {
            return Err(LoadDirectError::RequestedSubasset(path.clone()));
        };

        self.load_context
            .asset_server
            .write_infos()
            .stats
            .started_load_tasks += 1;

        // XXX TODO: We dropped support for processed info here since we're no longer
        // looking up the meta. Decide if we need a solution.
        let processed_info = Option::<&ProcessedInfo>::None;

        let asset = self
            .load_context
            .load_direct_internal(path.clone(), processed_info)
            .await?;

        // TODO: We dropped support for returning the loader here, which means
        // callers get less info for errors. Review alternatives.
        Ok((None, asset))
    }

    async fn load_value_from_reader_internal(
        self,
        type_id: Option<TypeId>,
        path: &AssetPath<'static>,
        reader: &'builder mut dyn Reader,
    ) -> Result<(Arc<dyn ErasedAssetLoader>, ErasedLoadedAsset), LoadDirectError> {
        if path.path() == Path::new("") {
            error!("Attempted to load an asset with an empty path \"{path}\"!");
            return Err(LoadDirectError::EmptyPath(path.clone_owned()));
        }
        if path.label().is_some() {
            return Err(LoadDirectError::RequestedSubasset(AssetRef::from(
                path.clone(),
            )));
        }
        self.load_context
            .asset_server
            .write_infos()
            .stats
            .started_load_tasks += 1;
        let loader = if let Some(type_id) = type_id {
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

        let asset = self
            .load_context
            .load_direct_from_reader_internal(
                path.clone(),
                meta.loader_settings().expect("meta corresponds to a load"),
                &*loader,
                reader,
                meta.processed_info().as_ref(),
            )
            .await?;
        Ok((loader, asset))
    }

    /// Same as [`Self::load_value_internal`], but with a generic to ensure the returned handle type
    /// is correct.
    #[cfg_attr(
        not(target_arch = "wasm32"),
        expect(
            clippy::result_large_err,
            reason = "we need to give the user the correct error type"
        )
    )]
    async fn load_typed_value_internal<A: Asset>(
        self,
        path: AssetRef<'static>,
    ) -> Result<LoadedAsset<A>, LoadDirectError> {
        self.load_value_internal(Some(TypeId::of::<A>()), &path)
            .await
            .and_then(move |(loader, untyped_asset)| {
                untyped_asset
                    .downcast::<A>()
                    .map_err(|_| LoadDirectError::LoadError {
                        dependency: path.clone(),
                        error: AssetLoadError::RequestedHandleTypeMismatch {
                            path,
                            requested: TypeId::of::<A>(),
                            // XXX TODO: Needs work now that asset_type_name is Option.
                            // Could use untyped_asset.asset_type_name()? But needs
                            // refactoring as that's already been moved.
                            actual_asset_name: "", // loader.asset_type_name(),
                            // XXX TODO: Similarly, we lost support for the loader path
                            // due to changes ins `load_value_internal`. Review alternatives.
                            loader_name: loader.map(|l| l.type_path()).unwrap_or("XXX TODO"),
                        },
                    })
            })
    }

    // XXX TODO: Document.
    // XXX TODO: Refactor? Somewhat duplicates `load_typed_value_internal`, although
    // takes `AssetPath` instead of `AssetRef`.
    #[expect(
        clippy::result_large_err,
        reason = "we need to give the user the correct error type"
    )]
    async fn load_typed_value_from_reader_internal<A: Asset>(
        self,
        path: AssetPath<'static>,
        reader: &'builder mut dyn Reader,
    ) -> Result<LoadedAsset<A>, LoadDirectError> {
        self.load_value_from_reader_internal(Some(TypeId::of::<A>()), &path, reader)
            .await
            .and_then(move |(loader, untyped_asset)| {
                untyped_asset
                    .downcast::<A>()
                    .map_err(|_| LoadDirectError::LoadError {
                        dependency: AssetRef::from(path.clone()),
                        error: AssetLoadError::RequestedHandleTypeMismatch {
                            path: AssetRef::from(path),
                            requested: TypeId::of::<A>(),
                            // XXX TODO: Needs work now that asset_type_name is Option. Could use untyped_asset.asset_type_name()? But needs refactoring as that's already been moved.
                            actual_asset_name: "", // loader.asset_type_name(),
                            loader_name: loader.type_path(),
                        },
                    })
            })
    }
}
