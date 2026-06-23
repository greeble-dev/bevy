use core::fmt::{self, Display};

/// A static accessor to type paths and names.
///
/// The engine uses this trait over [`core::any::type_name`] for stability and flexibility.
///
/// This trait is automatically implemented by the `#[derive(Reflect)]` macro
/// and allows type path information to be processed without an instance of that type.
///
/// Implementors may have difficulty in generating references with static
/// lifetimes. Luckily, this crate comes with some [utility] structs, to make generating these
/// statics much simpler.
///
/// # Stability
///
/// Certain parts of the engine, e.g. [(de)serialization], rely on type paths as identifiers
/// for matching dynamic values to concrete types.
///
/// Using [`core::any::type_name`], a scene containing `my_crate::foo::MyComponent` would break,
/// failing to deserialize if the component was moved from the `foo` module to the `bar` module,
/// becoming `my_crate::bar::MyComponent`.
/// This trait, through attributes when deriving itself or [`Reflect`], can ensure breaking changes are avoidable.
///
/// The only external factor we rely on for stability when deriving is the [`module_path!`] macro,
/// only if the derive does not provide a `#[type_path = "..."]` attribute.
///
/// # Anonymity
///
/// Some methods on this trait return `Option<&'static str>` over `&'static str`
/// because not all types define all parts of a type path, for example the array type `[T; N]`.
///
/// Such types are 'anonymous' in that they have only a defined [`type_path`] and [`short_type_path`]
/// and the methods [`crate_name`], [`module_path`] and [`type_ident`] all return `None`.
///
/// Primitives are treated like anonymous types, except they also have a defined [`type_ident`].
///
/// # Example
///
/// ```
/// use bevy_reflect::TypePath;
///
/// // This type path will not change with compiler versions or recompiles,
/// // although it will not be the same if the definition is moved.
/// #[derive(TypePath)]
/// struct NonStableTypePath;
///
/// // This type path will never change, even if the definition is moved.
/// #[derive(TypePath)]
/// #[type_path = "my_crate::foo"]
/// struct StableTypePath;
///
/// // Type paths can have any number of path segments.
/// #[derive(TypePath)]
/// #[type_path = "my_crate::foo::bar::baz"]
/// struct DeeplyNestedStableTypePath;
///
/// // Including just a crate name!
/// #[derive(TypePath)]
/// #[type_path = "my_crate"]
/// struct ShallowStableTypePath;
///
/// // We can also rename the identifier/name of types.
/// #[derive(TypePath)]
/// #[type_path = "my_crate::foo"]
/// #[type_name = "RenamedStableTypePath"]
/// struct NamedStableTypePath;
///
/// // Generics are also supported.
/// #[derive(TypePath)]
/// #[type_path = "my_crate::foo"]
/// struct StableGenericTypePath<T, const N: usize>([T; N]);
/// ```
///
/// [utility]: crate::utility
/// [(de)serialization]: crate::serde::ReflectDeserializer
/// [`Reflect`]: crate::Reflect
/// [`type_path`]: TypePath::type_path
/// [`short_type_path`]: TypePath::short_type_path
/// [`crate_name`]: TypePath::crate_name
/// [`module_path`]: TypePath::module_path
/// [`type_ident`]: TypePath::type_ident
#[diagnostic::on_unimplemented(
    message = "`{Self}` does not implement `TypePath` so cannot provide static type path information",
    note = "consider annotating `{Self}` with `#[derive(Reflect)]` or `#[derive(TypePath)]`"
)]
pub trait TypePath: 'static {
    // XXX TODO: Documentation?
    /// Returns the fully qualified path of the underlying type.
    ///
    /// Generic parameter types are also fully expanded.
    ///
    /// For `Option<Vec<usize>>`, this is `"std::option::Option<std::vec::Vec<usize>>"`.
    fn type_path() -> &'static str {
        Self::type_path_table().path()
    }

    /// Returns a short, pretty-print enabled path to the type.
    ///
    /// Generic parameter types are also shortened.
    ///
    /// For `Option<Vec<usize>>`, this is `"Option<Vec<usize>>"`.
    fn short_type_path() -> &'static str {
        Self::type_path_table().short_path()
    }

    /// Returns the name of the type, or [`None`] if it is [anonymous].
    ///
    /// Primitive types will return [`Some`].
    ///
    /// For `Option<Vec<usize>>`, this is `"Option"`.
    ///
    /// [anonymous]: TypePath#anonymity
    fn type_ident() -> Option<&'static str> {
        Self::type_path_table().ident()
    }

    /// Returns the name of the crate the type is in, or [`None`] if it is [anonymous].
    ///
    /// For `Option<Vec<usize>>`, this is `"core"`.
    ///
    /// [anonymous]: TypePath#anonymity
    fn crate_name() -> Option<&'static str> {
        Self::type_path_table().crate_name()
    }

    /// Returns the path to the module the type is in, or [`None`] if it is [anonymous].
    ///
    /// For `Option<Vec<usize>>`, this is `"std::option"`.
    ///
    /// [anonymous]: TypePath#anonymity
    fn module_path() -> Option<&'static str> {
        Self::type_path_table().module_path()
    }

    /// XXX TODO: Documentation.
    fn type_path_table() -> &'static TypePathTable;
}

/// XXX TODO: Documentation.
#[derive(PartialEq, Eq)]
pub struct TypePathTable {
    /// XXX TODO: Documentation.
    pub path: &'static str,
    /// XXX TODO: Documentation.
    pub short_path: &'static str,
    /// XXX TODO: Documentation.
    pub ident: Option<&'static str>,
    /// XXX TODO: Documentation.
    pub module_path: Option<&'static str>,
}

impl TypePathTable {
    // XXX TODO: Review documentation.
    /// Returns the fully qualified path of the underlying type.
    ///
    /// Generic parameter types are also fully expanded.
    ///
    /// For `Option<Vec<usize>>`, this is `"std::option::Option<std::vec::Vec<usize>>"`.
    pub fn path(&self) -> &'static str {
        self.path
    }

    /// Returns a short, pretty-print enabled path to the type.
    ///
    /// Generic parameter types are also shortened.
    ///
    /// For `Option<Vec<usize>>`, this is `"Option<Vec<usize>>"`.
    pub fn short_path(&self) -> &'static str {
        self.short_path
    }

    /// Returns the name of the type, or [`None`] if it is [anonymous].
    ///
    /// Primitive types will return [`Some`].
    ///
    /// For `Option<Vec<usize>>`, this is `"Option"`.
    ///
    /// [anonymous]: TypePath#anonymity
    pub fn ident(&self) -> Option<&'static str> {
        self.ident
    }

    /// Returns the name of the crate the type is in, or [`None`] if it is [anonymous].
    ///
    /// For `Option<Vec<usize>>`, this is `"core"`.
    ///
    /// [anonymous]: TypePath#anonymity
    // XXX TODO: Check that this covers all cases that the macro could previously
    // generate. See `ReflectTypePath::crate_name`.
    pub fn crate_name(&self) -> Option<&'static str> {
        self.module_path.and_then(|p| p.split(':').next())
    }

    /// Returns the path to the module the type is in, or [`None`] if it is [anonymous].
    ///
    /// For `Option<Vec<usize>>`, this is `"std::option"`.
    ///
    /// [anonymous]: TypePath#anonymity
    pub fn module_path(&self) -> Option<&'static str> {
        self.module_path
    }
}

impl Display for TypePathTable {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(self.path)
    }
}

// XXX TODO: Needed? Can probably auto-derive, but loses `crate_name`?
impl fmt::Debug for TypePathTable {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("TypePathTable")
            .field("type_path", &self.path)
            .field("short_type_path", &self.short_path)
            .field("type_ident", &self.ident)
            .field("crate_name", &self.crate_name())
            .field("module_path", &self.module_path)
            .finish()
    }
}

/// Dynamic dispatch for [`TypePath`].
///
/// Since this is a supertrait of [`Reflect`] its methods can be called on a `dyn Reflect`.
///
/// [`Reflect`]: crate::Reflect
#[diagnostic::on_unimplemented(
    message = "`{Self}` does not implement `TypePath` so cannot provide dynamic type path information",
    note = "consider annotating `{Self}` with `#[derive(Reflect)]` or `#[derive(TypePath)]`"
)]
pub trait DynamicTypePath {
    /// See [`TypePath::type_path`].
    fn reflect_type_path(&self) -> &'static str;

    /// See [`TypePath::type_path_table`].
    fn reflect_type_path_table(&self) -> &'static TypePathTable;
}

impl<T: TypePath> DynamicTypePath for T {
    #[inline]
    fn reflect_type_path(&self) -> &'static str {
        Self::type_path()
    }

    #[inline]
    fn reflect_type_path_table(&self) -> &'static TypePathTable {
        Self::type_path_table()
    }
}
