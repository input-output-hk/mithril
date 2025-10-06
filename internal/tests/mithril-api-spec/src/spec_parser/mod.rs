mod yaml;

/// Parser for OpenApi Specification
///
/// Returns the parsed spec as a [serde_json::Value] as this crate uses a jsonschema validator
/// to do the validation.
pub(crate) struct OpenApiSpecParser;

impl OpenApiSpecParser {
    pub(crate) fn parse_yaml(spec_path: &str) -> Result<serde_json::Value, String> {
        use saphyr::LoadableYamlNode;

        let yaml_spec = std::fs::read_to_string(spec_path)
            .map_err(|e| format!("Could not read spec file `{spec_path}`: {e}"))?;
        let openapi = saphyr::Yaml::load_from_str(&yaml_spec)
            .map_err(|e| format!("Could not parse spec file `{spec_path}`: {e}"))?;

        if openapi.is_empty() {
            Err("No spec found in file".to_string())
        } else {
            yaml::convert_yaml_to_serde_json(&openapi[0])
        }
    }
}
