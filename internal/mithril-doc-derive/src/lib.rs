extern crate proc_macro;

use proc_macro::TokenStream;
use quote::{format_ident, quote};
use syn::{Data, DeriveInput, Fields, Type, parse_macro_input};

mod doc;

fn extract_struct_info(
    ast: &DeriveInput,
    trait_name: &str,
    implement_default: bool,
) -> TokenStream {
    let name_ident = &ast.ident;
    let trait_ident = format_ident!("{trait_name}");
    let default_values_variable_ident = format_ident!("entries");

    let init_default_values_code_quote = if implement_default {
        quote! {
            let #default_values_variable_ident = <#name_ident>::default().collect().unwrap();
        }
    } else {
        quote! {
            let #default_values_variable_ident = Map::<String,Value>::new();
        }
    };

    let fields_info = extract_fields_info(ast).unwrap();
    let data_code_quote = fields_info.iter().map(|field_info| {
        let name = &field_info.name;
        let doc = &field_info.doc;
        let example = match &field_info.example {
            Some(value) => quote! {
                Some(#value.to_string())
            },
            None => quote! {
                None
            },
        };
        let is_mandatory = &field_info.is_mandatory;

        quote! {
            struct_data.add_param(#name, #doc, Some(#name.to_uppercase().to_string()), #default_values_variable_ident.get(#name).map(|v| v.to_string()), #example, #is_mandatory);
        }
    }).collect::<Vec<_>>();

    let output_quote = quote! {
        impl #trait_ident for #name_ident {
            fn extract() -> StructDoc {
                #init_default_values_code_quote
                let mut struct_data = StructDoc::default();
                #(#data_code_quote)*
                struct_data
            }
        }
    };

    output_quote.into()
}

fn extract_fields_info(ast: &DeriveInput) -> Result<Vec<FieldInfo>, String> {
    let data = &ast.data;
    let data_code = match data {
        Data::Enum(_) => {
            return Err("compile_error!(\"Enum types are not supported\")".parse().unwrap());
        }
        Data::Struct(data_struct) => {
            match &data_struct.fields {
                Fields::Named(fields) => fields.named.iter().map(format_field).collect(),
                _ => {
                    // Not implemented!
                    vec![]
                }
            }
        }
        Data::Union(_) => {
            // Not implemented!
            vec![]
        }
    };
    Ok(data_code)
}

struct FieldInfo {
    name: String,
    doc: String,
    example: Option<String>,
    is_mandatory: bool,
}

fn format_field(field: &syn::Field) -> FieldInfo {
    let _vis_str = match field.vis {
        syn::Visibility::Public(_) => "public",
        syn::Visibility::Restricted(_) => "restricted",
        syn::Visibility::Inherited => "inherited",
    };

    let doc = doc::extract_doc_comment(&field.attrs[..]).join("\n");

    let example = field
        .attrs
        .iter()
        .find(|attr| attr.path().is_ident("example"))
        .and_then(|attr| match &attr.meta {
            syn::Meta::NameValue(syn::MetaNameValue {
                value:
                    syn::Expr::Lit(syn::ExprLit {
                        lit: syn::Lit::Str(s),
                        ..
                    }),
                ..
            }) => Some(s.value()),
            _ => None,
        });

    let is_mandatory = match field.ty.clone() {
        Type::Path(type_path) => type_path
            .path
            .segments
            .first()
            .map(|segment| segment.ident != "Option")
            .unwrap_or(true),
        _ => true,
    };

    FieldInfo {
        name: field.ident.as_ref().unwrap().to_string(),
        doc,
        example,
        is_mandatory,
    }
}

/// To extract doc from a struct.
#[proc_macro_derive(Documenter, attributes(example))]
pub fn doc_extractor(input: TokenStream) -> TokenStream {
    // Parse the input tokens into a syntax tree
    let ast = parse_macro_input!(input as DeriveInput);
    extract_struct_info(&ast, "Documenter", false)
}

/// To extract doc from a struct with Default trait.
#[proc_macro_derive(DocumenterDefault, attributes(example))]
pub fn doc_extractor_default(input: TokenStream) -> TokenStream {
    // Parse the input tokens into a syntax tree
    let ast = parse_macro_input!(input as DeriveInput);
    extract_struct_info(&ast, "DocumenterDefault", true)
}
