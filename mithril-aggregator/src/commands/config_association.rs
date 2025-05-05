/// Call `extract_config` on each SubCommand so it could not be forget to implement it.
/// All variant must be listed otherwise there is a compilation error.
/// The associated command to the variant must be the right one otherwise there is a compilation error.
///
/// # Example
///
/// ```ignore
/// # use std::collections::HashMap;
/// # use mithril_doc::StructDoc;
/// # use mithril_aggregator::extract_all;
/// fn extract_config(command_path: String) -> HashMap<String, StructDoc> {
///     extract_all!(
///         command_path,
///         PseudoCommand,
///         CommandA = { PseudoCommandA },
///         CommandB = { pseudo_module::PseudoCommandB },
///     )
/// }
/// ```
#[macro_export]
macro_rules! extract_all {
    (@check_type $variant_val:ident, { $type:ident }) => {
        {let _:$type = $variant_val;}
    };
    (@check_type $variant_val:ident, { $module:ident::$type:ident }) => {
        {let _:$module::$type = $variant_val;}
    };
    (@check_type $variant_val:ident, {}) => {
        let _ = $variant_val;
    };

    (@extract_config $config_id:ident, { $type:ident }) => {
        $type::extract_config($config_id)
    };
    (@extract_config $config_id:ident, { $module:ident::$type:ident }) => {
        $module::$type::extract_config($config_id)
    };
    (@extract_config $config_id:ident, {}) => {
        {
            let _ = $config_id;
            std::collections::HashMap::new()
        }
    };

    ($command_path: ident, $E:path, $($variant:ident = $cmd:tt,)*) => {
        {
            // Check that all enum variants are used and the associated variant value
            use $E as E;
            let _ = |dummy: E| {
                match dummy {
                    $(E::$variant(x) => {
                        extract_all!(@check_type x, $cmd);
                    }),*
                }
            };

            // Build config HashMap
            let mut configs = HashMap::new();
            $(
                let config_id = format!(
                    "{} {}",
                    $command_path, stringify!($variant).to_lowercase()
                );
                configs.extend(extract_all!(@extract_config config_id, $cmd));
            )*
            configs
        }

    };
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use mithril_common::assert_equivalent_macro;
    use mithril_doc::StructDoc;

    #[allow(dead_code)]
    #[derive(Debug, Clone)]
    enum PseudoCommand {
        CommandA(PseudoCommandA),
        CommandB(pseudo_module::PseudoCommandB),
        CommandWithoutConfig(PseudoCommandWithoutConfig),
        CommandConfigUnwanted(PseudoCommandConfigUnwanted),
        CommandE(EnumCommandE),
    }

    #[derive(Debug, Clone)]
    struct PseudoCommandA {}
    impl PseudoCommandA {
        pub fn extract_config(command_path: String) -> HashMap<String, StructDoc> {
            let mut struct_doc = StructDoc::default();
            struct_doc.add_param("field_command_a", "", None, None, None, true);
            HashMap::from([(command_path, struct_doc)])
        }
    }

    #[derive(Debug, Clone)]
    struct PseudoCommandWithoutConfig {}
    impl PseudoCommandWithoutConfig {
        pub fn extract_config(_command_path: String) -> HashMap<String, StructDoc> {
            HashMap::new()
        }
    }

    #[derive(Debug, Clone)]
    struct PseudoCommandConfigUnwanted {}

    #[allow(dead_code)]
    #[derive(Debug, Clone)]
    enum EnumCommandE {
        SubCommandE(PseudoSubCommandE),
    }

    impl EnumCommandE {
        pub fn extract_config(command_path: String) -> HashMap<String, StructDoc> {
            extract_all!(
                command_path,
                EnumCommandE,
                SubCommandE = { PseudoSubCommandE },
            )
        }
    }

    #[derive(Debug, Clone)]
    struct PseudoSubCommandE {}
    impl PseudoSubCommandE {
        pub fn extract_config(command_path: String) -> HashMap<String, StructDoc> {
            let mut struct_doc = StructDoc::default();
            struct_doc.add_param("field_sub_command_e", "", None, None, None, true);
            HashMap::from([(command_path, struct_doc)])
        }
    }

    mod pseudo_module {
        use super::*;

        #[derive(Debug, Clone)]
        pub struct PseudoCommandB {}
        impl PseudoCommandB {
            pub fn extract_config(command_path: String) -> HashMap<String, StructDoc> {
                let mut struct_doc = StructDoc::default();
                struct_doc.add_param("field_command_b", "", None, None, None, true);
                HashMap::from([(command_path, struct_doc)])
            }
        }
    }

    #[test]
    fn test_extract_all_should_construct_hashmap_with_subcommand() {
        let command_path = "mithril".to_string();
        let configs = extract_all!(
            command_path,
            PseudoCommand,
            CommandA = { PseudoCommandA },
            CommandB = { pseudo_module::PseudoCommandB },
            CommandWithoutConfig = { PseudoCommandWithoutConfig },
            CommandConfigUnwanted = {},
            CommandE = { EnumCommandE },
        );

        let keys: Vec<String> = configs.clone().into_keys().collect();
        let expected = vec![
            "mithril commanda".to_string(),
            "mithril commandb".to_string(),
            "mithril commande subcommande".to_string(),
        ];

        assert_equivalent_macro!(expected, keys);
    }

    #[test]
    fn test_extract_all_should_associate_struct_doc() {
        let command_path = "mithril".to_string();
        let configs = extract_all!(
            command_path,
            PseudoCommand,
            CommandA = { PseudoCommandA },
            CommandB = { pseudo_module::PseudoCommandB },
            CommandWithoutConfig = { PseudoCommandWithoutConfig },
            CommandConfigUnwanted = {},
            CommandE = { EnumCommandE },
        );
        let doc_command_b = configs.get("mithril commandb").unwrap();
        assert!(doc_command_b.get_field("field_command_b").is_some());
        assert_eq!(1, doc_command_b.get_ordered_data().len());

        let doc_sub_command_e = configs.get("mithril commande subcommande").unwrap();
        assert!(doc_sub_command_e.get_field("field_sub_command_e").is_some());
        assert_eq!(1, doc_sub_command_e.get_ordered_data().len());
    }
}
