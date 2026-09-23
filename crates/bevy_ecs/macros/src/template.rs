use bevy_macro_utils::{fq_std::FQDefault, BevyManifest};
use proc_macro::TokenStream;
use quote::{format_ident, quote};
use syn::{
    parse::ParseStream, parse_macro_input, parse_quote, punctuated::Punctuated, spanned::Spanned,
    Data, DeriveInput, Fields, FieldsUnnamed, Ident, Index, MacroDelimiter, Meta, Path, Result,
    Token, WhereClause,
};

const TEMPLATE_DEFAULT_ATTRIBUTE: &str = "default";
const TEMPLATE_ATTRIBUTE: &str = "template";
const BUILT_IN_ATTRIBUTE: &str = "built_in";
const COPY_DEFAULT_ATTRIBUTE: &str = "copy_default";

pub(crate) fn derive_from_template(input: TokenStream) -> TokenStream {
    let ast = parse_macro_input!(input as DeriveInput);
    let bevy_ecs = BevyManifest::shared(|manifest| manifest.get_path("bevy_ecs"));

    let type_ident = &ast.ident;
    let (impl_generics, type_generics, where_clause) = &ast.generics.split_for_impl();

    let template_ident = format_ident!("{type_ident}Template");

    let type_visibility = &ast.vis;

    let mut copy_default = None;

    // XXX TODO: Review this mess - cut and pasted from `bevy_reflect` just to
    // get a basic `copy_default` working.
    //
    // XXX TODO: `copy_default` should be documented in `FromTemplate`.
    for attribute in &ast.attrs {
        match &attribute.meta {
            Meta::List(meta_list) if meta_list.path.is_ident(TEMPLATE_ATTRIBUTE) => {
                if let MacroDelimiter::Paren(_) = meta_list.delimiter {
                    attribute
                        .parse_args_with(|stream: ParseStream| {
                            let forked = stream.fork();
                            let ident = forked.parse::<Ident>()?;
                            if ident == COPY_DEFAULT_ATTRIBUTE {
                                stream.parse::<Ident>()?;
                                copy_default = Some(type_ident);
                            } else {
                                return Err(syn::Error::new(attribute.span(), "XXX TODO"));
                            }
                            Ok(())
                        })
                        .expect("XXX TODO");
                } else {
                    todo!("XXX TODO")
                    // return Err(syn::Error::new(
                    //         meta_list.delimiter.span().join(),
                    //         format_args!(
                    //             "`#[{TEMPLATE_ATTRIBUTE}(\"...\")]` must use parentheses `(` and `)`"
                    //         ),
                    //     ));
                }
            }
            _ => {}
        }
    }

    let template = match &ast.data {
        Data::Struct(data_struct) => {
            let result = match struct_impl(&data_struct.fields, &bevy_ecs, false, copy_default) {
                Ok(result) => result,
                Err(err) => return err.into_compile_error().into(),
            };
            let StructImpl {
                template_fields,
                template_field_builds,
                template_field_defaults,
                template_field_clones,
                template_field_asset_dependencies,
                ..
            } = result;
            match &data_struct.fields {
                Fields::Named(_) => {
                    quote! {
                        #[allow(missing_docs)]
                        #type_visibility struct #template_ident #impl_generics #where_clause {
                            #(#template_fields,)*
                        }

                        impl #impl_generics #bevy_ecs::template::Template for #template_ident #type_generics #where_clause {
                            type Output = #type_ident #type_generics;
                            fn build_template(&self, context: &mut #bevy_ecs::template::TemplateContext) -> #bevy_ecs::error::Result<Self::Output> {
                                #bevy_ecs::error::Result::Ok(#type_ident {
                                    #(#template_field_builds,)*
                                })
                            }

                            fn clone_template(&self) -> Self {
                                Self {
                                    #(#template_field_clones,)*
                                }
                            }

                            fn asset_dependencies(&self, dependencies: &mut #bevy_ecs::template::TemplateAssetDependencies) {
                                #(#template_field_asset_dependencies;)*
                            }
                        }

                        impl #impl_generics #FQDefault for #template_ident #type_generics #where_clause {
                            fn default() -> Self {
                                Self {
                                    #(#template_field_defaults,)*
                                }
                            }
                        }
                    }
                }
                Fields::Unnamed(_) => {
                    quote! {
                        #[allow(missing_docs)]
                        #type_visibility struct #template_ident #impl_generics (
                            #(#template_fields,)*
                        )  #where_clause;

                        impl #impl_generics #bevy_ecs::template::Template for #template_ident #type_generics #where_clause {
                            type Output = #type_ident #type_generics;
                            fn build_template(&self, context: &mut #bevy_ecs::template::TemplateContext) -> #bevy_ecs::error::Result<Self::Output> {
                                #bevy_ecs::error::Result::Ok(#type_ident (
                                    #(#template_field_builds,)*
                                ))
                            }

                            fn clone_template(&self) -> Self {
                                Self(
                                    #(#template_field_clones,)*
                                )
                            }

                            fn asset_dependencies(&self, dependencies: &mut #bevy_ecs::template::TemplateAssetDependencies) {
                                #(#template_field_asset_dependencies;)*
                            }
                        }

                        impl #impl_generics #FQDefault for #template_ident #type_generics #where_clause {
                            fn default() -> Self {
                                Self (
                                    #(#template_field_defaults,)*
                                )
                            }
                        }
                    }
                }
                Fields::Unit => {
                    quote! {
                        #[allow(missing_docs)]
                        #type_visibility struct #template_ident;

                        impl #impl_generics #bevy_ecs::template::Template for #template_ident #type_generics #where_clause {
                            type Output = #type_ident;
                            fn build_template(&self, context: &mut #bevy_ecs::template::TemplateContext) -> #bevy_ecs::error::Result<Self::Output> {
                                #bevy_ecs::error::Result::Ok(#type_ident)
                            }

                            fn clone_template(&self) -> Self {
                                Self
                            }

                            fn asset_dependencies(&self, _dependencies: &mut #bevy_ecs::template::TemplateAssetDependencies) {}
                        }

                        impl #impl_generics #FQDefault for #template_ident #type_generics #where_clause {
                            fn default() -> Self {
                                Self
                            }
                        }
                    }
                }
            }
        }
        Data::Enum(data_enum) => {
            let mut variant_definitions = Vec::new();
            let mut variant_builds = Vec::new();
            let mut variant_clones = Vec::new();
            let mut variant_asset_dependencies = Vec::new();
            let mut variant_default_ident = None;
            for variant in &data_enum.variants {
                let result = match struct_impl(&variant.fields, &bevy_ecs, true, copy_default) {
                    Ok(result) => result,
                    Err(err) => return err.into_compile_error().into(),
                };
                let StructImpl {
                    template_fields,
                    template_field_builds,
                    template_field_defaults,
                    template_field_clones,
                    template_field_asset_dependencies,
                    ..
                } = result;

                let is_default = variant
                    .attrs
                    .iter()
                    .any(|a| a.path().is_ident(TEMPLATE_DEFAULT_ATTRIBUTE));
                if is_default && variant_default_ident.is_some() {
                    panic!("Cannot have multiple default variants");
                }
                let variant_ident = &variant.ident;
                match &variant.fields {
                    Fields::Named(fields) => {
                        variant_definitions.push(quote! {
                            #variant_ident {
                                #(#template_fields,)*
                            }
                        });
                        let field_idents = fields.named.iter().map(|f| &f.ident);
                        variant_builds.push(quote! {
                            // TODO: proper assignments here
                            #template_ident::#variant_ident {
                                #(#field_idents,)*
                            } => {
                                #type_ident::#variant_ident {
                                    #(#template_field_builds,)*
                                }
                            }
                        });

                        let field_idents = fields.named.iter().map(|f| &f.ident);
                        variant_clones.push(quote! {
                            // TODO: proper assignments here
                            #template_ident::#variant_ident {
                                #(#field_idents,)*
                            } => {
                                #template_ident::#variant_ident {
                                    #(#template_field_clones,)*
                                }
                            }
                        });

                        let field_idents = fields.named.iter().map(|f| &f.ident);
                        variant_asset_dependencies.push(quote! {
                            // TODO: proper assignments here
                            #template_ident::#variant_ident {
                                #(#field_idents,)*
                            } => {
                                #(#template_field_asset_dependencies;)*
                            }
                        });

                        if is_default {
                            variant_default_ident = Some(quote! {
                                Self::#variant_ident {
                                    #(#template_field_defaults,)*
                                }
                            });
                        }
                    }
                    Fields::Unnamed(FieldsUnnamed { unnamed: f, .. }) => {
                        let field_idents = f
                            .iter()
                            .enumerate()
                            .map(|(i, _)| format_ident!("t{}", i))
                            .collect::<Vec<_>>();
                        variant_definitions.push(quote! {
                            #variant_ident(#(#template_fields,)*)
                        });
                        variant_builds.push(quote! {
                            // TODO: proper assignments here
                            #template_ident::#variant_ident(
                                #(#field_idents,)*
                             ) => {
                                #type_ident::#variant_ident(
                                    #(#template_field_builds,)*
                                )
                            }
                        });
                        variant_clones.push(quote! {
                            #template_ident::#variant_ident(
                                #(#field_idents,)*
                             ) => {
                                #template_ident::#variant_ident(
                                    #(#template_field_clones,)*
                                )
                            }
                        });
                        variant_asset_dependencies.push(quote! {
                            #template_ident::#variant_ident(
                                #(#field_idents,)*
                             ) => {
                                #(#template_field_asset_dependencies;)*
                            }
                        });
                        if is_default {
                            variant_default_ident = Some(quote! {
                                Self::#variant_ident(
                                    #(#template_field_defaults,)*
                                )
                            });
                        }
                    }
                    Fields::Unit => {
                        variant_definitions.push(quote! {#variant_ident});
                        variant_builds.push(
                            quote! {#template_ident::#variant_ident => #type_ident::#variant_ident},
                        );
                        variant_clones.push(
                            quote! {#template_ident::#variant_ident => #template_ident::#variant_ident},
                        );
                        variant_asset_dependencies
                            .push(quote! {#template_ident::#variant_ident => {}});
                        if is_default {
                            variant_default_ident = Some(quote! {
                                Self::#variant_ident
                            });
                        }
                    }
                }
            }

            if variant_default_ident.is_none() {
                panic!("Deriving Template for enums requires picking a default variant using #[default]");
            }

            quote! {
                #[allow(missing_docs)]
                #type_visibility enum #template_ident #type_generics #where_clause {
                    #(#variant_definitions,)*
                }

                impl #impl_generics #bevy_ecs::template::Template for #template_ident #type_generics #where_clause {
                    type Output = #type_ident #type_generics;
                    fn build_template(&self, context: &mut #bevy_ecs::template::TemplateContext) -> #bevy_ecs::error::Result<Self::Output> {
                        #bevy_ecs::error::Result::Ok(match self {
                            #(#variant_builds,)*
                        })
                    }

                    fn clone_template(&self) -> Self {
                        match self {
                            #(#variant_clones,)*
                        }
                    }

                    fn asset_dependencies(&self, dependencies: &mut #bevy_ecs::template::TemplateAssetDependencies) {
                        match self {
                            #(#variant_asset_dependencies,)*
                        };
                    }
                }

                impl #impl_generics #FQDefault for #template_ident #type_generics #where_clause {
                    fn default() -> Self {
                        #variant_default_ident
                    }
                }
            }
        }
        Data::Union(_) => panic!("Union types are not supported yet."),
    };

    let mut unpin_where_clause = where_clause.cloned().unwrap_or_else(|| WhereClause {
        where_token: <Token![where]>::default(),
        predicates: Punctuated::new(),
    });

    unpin_where_clause
        .predicates
        .push(parse_quote! { for<'a> [()]: #bevy_ecs::template::SpecializeFromTemplate });

    TokenStream::from(quote! {
        impl #impl_generics #bevy_ecs::template::FromTemplate for #type_ident #type_generics #where_clause {
            type Template = #template_ident #type_generics;
        }

        impl #impl_generics ::core::marker::Unpin for #type_ident #type_generics #unpin_where_clause {}

        #template
    })
}

struct StructImpl {
    template_fields: Vec<proc_macro2::TokenStream>,
    template_field_builds: Vec<proc_macro2::TokenStream>,
    template_field_defaults: Vec<proc_macro2::TokenStream>,
    template_field_clones: Vec<proc_macro2::TokenStream>,
    template_field_asset_dependencies: Vec<proc_macro2::TokenStream>,
}

enum TemplateType {
    FromTemplate,
    BuiltIn,
    Manual(Path),
}

fn struct_impl(
    fields: &Fields,
    bevy_ecs: &Path,
    is_enum: bool,
    copy_default: Option<&Ident>,
) -> Result<StructImpl> {
    let mut template_fields = Vec::with_capacity(fields.len());
    let mut template_field_builds = Vec::with_capacity(fields.len());
    let mut template_field_defaults = Vec::with_capacity(fields.len());
    let mut template_field_clones = Vec::with_capacity(fields.len());
    let mut template_field_asset_dependencies = Vec::with_capacity(fields.len());
    let is_named = matches!(fields, Fields::Named(_));
    for (index, field) in fields.iter().enumerate() {
        let is_pub = matches!(field.vis, syn::Visibility::Public(_));
        let field_maybe_pub = if is_pub { quote!(pub) } else { quote!() };
        let ident = &field.ident;
        let ty = &field.ty;
        let index = Index::from(index);
        let mut template_type = TemplateType::FromTemplate;
        for attr in &field.attrs {
            if attr.path().is_ident(TEMPLATE_ATTRIBUTE) {
                attr.parse_args_with(|stream: ParseStream| {
                    let forked = stream.fork();
                    let ident = forked.parse::<Ident>()?;
                    if ident == BUILT_IN_ATTRIBUTE {
                        stream.parse::<Ident>()?;
                        template_type = TemplateType::BuiltIn;
                    } else {
                        if let Ok(path) = stream.parse::<Path>() {
                            template_type = TemplateType::Manual(path);
                        } else {
                            return Err(syn::Error::new(
                                attr.span(),
                                "Expected a Template type path",
                            ));
                        }
                    }
                    Ok(())
                })?;
            }
        }

        let template_type = match template_type {
            TemplateType::FromTemplate => {
                quote!(<#ty as #bevy_ecs::template::FromTemplate>::Template)
            }
            TemplateType::BuiltIn => {
                quote!(<#ty as #bevy_ecs::template::BuiltInTemplate>::Template)
            }
            TemplateType::Manual(path) => quote! {#path},
        };

        if is_named {
            template_fields.push(quote! {
                #field_maybe_pub #ident: #template_type
            });
            if is_enum {
                template_field_builds.push(quote! {
                    #ident: #ident.build_template(context)?
                });
                template_field_clones.push(quote! {
                    #ident: #bevy_ecs::template::Template::clone_template(#ident)
                });
                template_field_asset_dependencies.push(quote! {
                    #bevy_ecs::template::Template::asset_dependencies(#ident, dependencies)
                });
            } else {
                template_field_builds.push(quote! {
                    #ident: self.#ident.build_template(context)?
                });
                template_field_clones.push(quote! {
                    #ident: #bevy_ecs::template::Template::clone_template(&self.#ident)
                });
                template_field_asset_dependencies.push(quote! {
                    #bevy_ecs::template::Template::asset_dependencies(&self.#ident, dependencies)
                });
            }

            if let Some(copy_default) = copy_default {
                if is_enum {
                    // XXX TODO: Enums are tricky because we can't simply do `default().#ident`. We need
                    // to do a single call to `default()` then a match and handle each variant.
                    todo!("XXX TODO");
                } else {
                    template_field_defaults.push(quote! {
                        #ident: #bevy_ecs::template::ToTemplate::to_template(#copy_default::default().#ident)
                    });
                }
            } else {
                template_field_defaults.push(quote! {
                    #ident: #FQDefault::default()
                });
            }
        } else {
            template_fields.push(quote! {
                #field_maybe_pub #template_type
            });
            if is_enum {
                let enum_tuple_ident = format_ident!("t{}", index);
                template_field_builds.push(quote! {
                    #enum_tuple_ident.build_template(context)?
                });
                template_field_clones.push(quote! {
                    #bevy_ecs::template::Template::clone_template(#enum_tuple_ident)
                });
                template_field_asset_dependencies.push(quote! {
                    #bevy_ecs::template::Template::asset_dependencies(#enum_tuple_ident, dependencies)
                });
            } else {
                template_field_builds.push(quote! {
                    self.#index.build_template(context)?
                });
                template_field_clones.push(quote! {
                    #bevy_ecs::template::Template::clone_template(&self.#index)
                });
                template_field_asset_dependencies.push(quote! {
                    #bevy_ecs::template::Template::asset_dependencies(&self.#index, dependencies)
                });
            }

            if let Some(copy_default) = copy_default {
                if is_enum {
                    // XXX TODO: Enums are tricky because we can't simply do `default().#ident`. We need
                    // to do a single call to `default()` then a match and handle each variant.
                    todo!("XXX TODO");
                } else {
                    template_field_defaults.push(quote! {
                        #bevy_ecs::template::ToTemplate::to_template(#copy_default::default().#index)
                    });
                }
            } else {
                template_field_defaults.push(quote! {
                    #FQDefault::default()
                });
            }
        }
    }
    Ok(StructImpl {
        template_fields,
        template_field_builds,
        template_field_defaults,
        template_field_clones,
        template_field_asset_dependencies,
    })
}
