extern crate proc_macro;

use proc_macro::TokenStream;
use quote::{format_ident, quote};
use syn::{parse_macro_input, Data, DeriveInput, Fields};

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
        quote! {
            struct_data.add_param(#name, #doc, #default_values_variable_ident.get(#name).map(|v| v.to_string()), #example);
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
            return Err("compile_error!(\"Enum types are not supported\")"
                .parse()
                .unwrap())
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
}

fn format_field(champ: &syn::Field) -> FieldInfo {
    let _vis_str = match champ.vis {
        syn::Visibility::Public(_) => "public",
        syn::Visibility::Restricted(_) => "restricted",
        syn::Visibility::Inherited => "inherited",
    };

    let _nb_attr = champ.attrs.len();

    let doc = champ
        .attrs
        .first()
        .map(|_| {
            // let m = &a.meta;
            // format!("{} {} => {}",
            // m.path().get_ident().as_ref().unwrap(),
            // match a.style {
            // syn::AttrStyle::Outer => "Outer".to_string(),
            // syn::AttrStyle::Inner(_) => "Inner".to_string(),
            // },
            // comment.join("\n")
            // )

            doc::extract_doc_comment(&champ.attrs[..]).join("\n")
        })
        .unwrap_or("".to_string());

    let example = champ
        .attrs
        .iter()
        .find(|attr| attr.path().is_ident("example"))
        .and_then(|attr| {
            match &attr.meta {
                syn::Meta::NameValue(syn::MetaNameValue {
                    value:
                        syn::Expr::Lit(syn::ExprLit {
                            lit: syn::Lit::Str(s),
                            ..
                        }),
                    ..
                }) => Some(s.value()),
                _ => None,
            }
        });

    let field_info = FieldInfo {
        name: champ.ident.as_ref().unwrap().to_string(),
        doc,
        example,
    };

    field_info
}

/// To extract doc from a struct.
#[proc_macro_derive(DocExtractor, attributes(example))]
pub fn doc_extractor(input: TokenStream) -> TokenStream {
    // Parse the input tokens into a syntax tree
    let ast = parse_macro_input!(input as DeriveInput);
    extract_struct_info(&ast, "DocExtractor", false)
}

/// To extract doc from a struct with Default trait.
#[proc_macro_derive(DocExtractorDefault, attributes(example))]
pub fn doc_extractor_default(input: TokenStream) -> TokenStream {
    // Parse the input tokens into a syntax tree
    let ast = parse_macro_input!(input as DeriveInput);
    extract_struct_info(&ast, "DocExtractorDefault", true)
}
