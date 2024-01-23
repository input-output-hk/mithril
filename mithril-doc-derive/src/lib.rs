extern crate proc_macro;


// https://github.com/dtolnay/syn
use proc_macro::TokenStream;
use quote::quote;
use syn::{parse_macro_input, DeriveInput, Data, Fields};

mod doc;

//https://www.youtube.com/watch?v=crWfcA064is
//https://blog.guillaume-gomez.fr/Rust/3/1


fn extract_struct_info(ast: &DeriveInput, trait_name: &str, implement_default: bool) -> String {
    let name = &ast.ident;

    let default_values_variable = "entries";
    let init_default_values_code = if implement_default {
        format!("let {default_values_variable} = {name}::default().collect().unwrap();")
    }
    else {
        format!("let {default_values_variable} = Map::<String,Value>::new();")
    };

    let data_code = extract_fields_info(ast).unwrap();
    let data_code = data_code.iter().map(|field_info| {
        format!("struct_data.add_param(\"{}\", \"{}\", {default_values_variable}.get(\"{}\").map(|v| v.to_string()));", 
            field_info.name, 
            field_info.doc,
            field_info.name)
    }).collect::<Vec<String>>().join("\n");

    let output = format!("impl {trait_name} for {name} {{
        fn extract() -> StructDoc {{
            {init_default_values_code}
            let mut struct_data = StructDoc::default();
            {data_code}
            struct_data
        }}
    }}");
    output

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
                Fields::Named(fields) => {
                    let formatted_fields = fields.named
                        .iter()
                        .map(format_field);

                    // let fields_list = formatted_fields.map(|field_info| {
                    //     format!("struct_data.add_param(\"{}\", \"{}\", entries.get(\"{}\").map(|v| v.to_string()));", 
                    //         field_info.name, 
                    //         field_info.doc,
                    //         field_info.name)
                    // }).collect::<Vec<String>>().join("\n");
                    //format!("{fields_list}")
                    formatted_fields.collect()
                },
                _ => {
                    //String::from("")
                    vec!()
                }
            }
        }
        Data::Union(_) => {
            //String::from("Data::Union not implemented !!!")
            vec!()
        }
    };
    Ok(data_code)
}


struct FieldInfo {
    name: String,
    doc: String,
}

fn format_field(champ: &syn::Field) -> FieldInfo {
    let _vis_str = match champ.vis {
        syn::Visibility::Public(_) => "public",
        syn::Visibility::Restricted(_) => "restricted",
        syn::Visibility::Inherited => "inherited",
    };

    let _nb_attr = champ.attrs.len();

                            
    let doc = champ.attrs.iter().next().map(|_| {
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
    }
    ).unwrap_or("".to_string());

    let field_info = FieldInfo {
        name: champ.ident.as_ref().unwrap().to_string(),
        doc: doc,
    };

    field_info

}

/// Sample to show how to transform using quote macro.
/// TODO Remove it if not usefull.s
#[allow(dead_code)]
fn sample_using_quote(ast: &DeriveInput) -> TokenStream {
    let name = &ast.ident;
    // Build the output, possibly using quasi-quotation
    quote! {
        impl DocExtractor for #name {
            fn execute() -> String {
                "DocExtractor::execute my_macro_imp_2>>>>".to_string()
            }
        }
    }.into()
}

/// To extract doc from a struct.
#[proc_macro_derive(DocExtractor)]
pub fn doc_extractor(input: TokenStream) -> TokenStream {
    // Parse the input tokens into a syntax tree
    let ast = parse_macro_input!(input as DeriveInput);
    let code = extract_struct_info(&ast, "DocExtractor", false);
    code.parse().unwrap()
    //my_macro_imp_2(&ast)
}

/// To extract doc from a struct with Default trait.
#[proc_macro_derive(DocExtractorDefault)]
pub fn doc_extractor_default(input: TokenStream) -> TokenStream {
    // Parse the input tokens into a syntax tree
    let ast = parse_macro_input!(input as DeriveInput);
    let code = extract_struct_info(&ast, "DocExtractorDefault", true);
    code.parse().unwrap()
    //my_macro_imp_2(&ast)
}
