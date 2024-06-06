//! Tools to helps validate conformity to an OpenAPI specification

use glob::glob;
use jsonschema::JSONSchema;
use reqwest::Url;
use serde::Serialize;
use serde_json::{json, Value, Value::Null};

use warp::http::Response;
use warp::http::StatusCode;
use warp::hyper::body::Bytes;

use crate::era::SupportedEra;

/// APISpec helps validate conformity to an OpenAPI specification
pub struct APISpec<'a> {
    openapi: Value,
    path: Option<&'a str>,
    method: Option<&'a str>,
    content_type: Option<&'a str>,
}

impl<'a> APISpec<'a> {
    /// Verify conformity helper of API Specs
    pub fn verify_conformity(
        spec_files: Vec<String>,
        method: &str,
        path: &str,
        content_type: &str,
        request_body: &impl Serialize,
        response: &Response<Bytes>,
        status_code: &StatusCode,
    ) -> Result<(), String> {
        if spec_files.is_empty() {
            return Err(
                "OpenAPI need a spec file to validate conformity. None were given.".to_string(),
            );
        }

        for spec_file in spec_files {
            if let Err(e) = APISpec::from_file(&spec_file)
                .method(method)
                .path(path)
                .content_type(content_type)
                .validate_request(request_body)
                .and_then(|api| api.validate_response(response))
                .and_then(|api| api.validate_status(response, status_code))
            {
                return Err(format!(
                    "OpenAPI invalid response in {spec_file} on route {path}, reason: {e}\nresponse: {response:#?}"
                ));
            }
        }
        Ok(())
    }

    /// APISpec factory from spec
    pub fn from_file(path: &str) -> APISpec<'a> {
        let yaml_spec = std::fs::read_to_string(path).unwrap();
        let openapi: serde_json::Value = serde_yaml::from_str(&yaml_spec).unwrap();
        APISpec {
            openapi,
            path: None,
            method: None,
            content_type: Some("application/json"),
        }
    }

    /// Sets the path to specify/check.
    pub fn path(&'a mut self, path: &'a str) -> &mut APISpec {
        self.path = Some(path);
        self
    }

    /// Sets the method to specify/check.
    pub fn method(&'a mut self, method: &'a str) -> &mut APISpec {
        self.method = Some(method);
        self
    }

    /// Sets the content type to specify/check, note that it defaults to "application/json".
    pub fn content_type(&'a mut self, content_type: &'a str) -> &mut APISpec {
        self.content_type = Some(content_type);
        self
    }

    /// Validates if a request is valid
    fn validate_request(&'a self, request_body: &impl Serialize) -> Result<&APISpec, String> {
        let path = self.path.unwrap();
        let method = self.method.unwrap().to_lowercase();
        let content_type = self.content_type.unwrap();

        let openapi_path_entry = path.split('?').next().unwrap();
        let operation_object = &self.openapi["paths"][openapi_path_entry][method];

        self.validate_query_parameters(path, operation_object)?;

        let request_schema = &operation_object["requestBody"]["content"][content_type]["schema"];
        let value = &json!(&request_body);
        self.validate_conformity(value, request_schema)
    }

    fn validate_query_parameters(
        &'a self,
        path: &str,
        operation_object: &Value,
    ) -> Result<&APISpec, String> {
        let fake_base_url = "http://0.0.0.1";
        let url = Url::parse(&format!("{}{}", fake_base_url, path)).unwrap();

        check_query_parameter_limitations(&url, operation_object);

        let mut query_pairs = url.query_pairs();
        if let Some(parameter) = query_pairs.next() {
            let spec_parameter = &operation_object["parameters"][0];
            let spec_parameter_name = &spec_parameter["name"].as_str().unwrap();
            let spec_parameter_in = &spec_parameter["in"].as_str().unwrap();
            if spec_parameter_in.eq(&"query") && spec_parameter_name.eq(&parameter.0) {
                Ok(self)
            } else {
                Err(format!("Unexpected query parameter '{}'", parameter.0))
            }
        } else {
            Ok(self)
        }
    }

    /// Validates if the status is the expected one
    fn validate_status(
        &'a self,
        response: &Response<Bytes>,
        expected_status_code: &StatusCode,
    ) -> Result<&APISpec, String> {
        if expected_status_code.as_u16() != response.status().as_u16() {
            return Err(format!(
                "expected status code {} but was {}",
                expected_status_code.as_u16(),
                response.status().as_u16(),
            ));
        }

        Ok(self)
    }

    /// Validates if a response is valid
    fn validate_response(&'a self, response: &Response<Bytes>) -> Result<&APISpec, String> {
        let body = response.body();
        let status = response.status();

        let path = self.path.unwrap();
        let path = path.split('?').next().unwrap();
        let method = self.method.unwrap().to_lowercase();
        let content_type = self.content_type.unwrap();
        let mut openapi = self.openapi.clone();

        let response_spec = {
            match &mut openapi["paths"][path][&method]["responses"] {
                Null => None,
                responses_spec => {
                    let status_code = status.as_str();
                    if responses_spec
                        .as_object()
                        .unwrap()
                        .contains_key(status_code)
                    {
                        Some(&responses_spec[status_code])
                    } else {
                        Some(&responses_spec["default"])
                    }
                }
            }
        };
        match response_spec {
            Some(response_spec) => {
                let response_schema = match &response_spec["content"] {
                    Null => &Null,
                    content => {
                        if content[content_type] == Null {
                            return Err(format!(
                                "Expected content type '{}' but spec is '{}'",
                                content_type, response_spec["content"],
                            ));
                        }
                        &content[content_type]["schema"]
                    }
                };
                if body.is_empty() {
                    match response_schema.as_object() {
                        Some(_) => Err("Non empty body expected".to_string()),
                        None => Ok(self),
                    }
                } else {
                    match response_schema.as_object() {
                        Some(_) => match &serde_json::from_slice(body) {
                            Ok(value) => self.validate_conformity(value, response_schema),
                            Err(_) => Err(format!("Expected a valid json but got: {body:?}")),
                        },
                        None => Err(format!("Expected empty body but got: {body:?}")),
                    }
                }
            }
            None => Err(format!(
                "Unmatched path and method: {path} {}",
                method.to_uppercase()
            )),
        }
    }

    /// Validates conformity of a value against a schema
    fn validate_conformity(&'a self, value: &Value, schema: &Value) -> Result<&APISpec, String> {
        match schema {
            Null => match value {
                Null => Ok(self),
                _ => Err(format!("Expected nothing but got: {value:?}")),
            },
            _ => {
                let mut schema = schema.as_object().unwrap().clone();
                let components = self.openapi["components"].clone();
                schema.insert(String::from("components"), components);

                let validator = JSONSchema::compile(&json!(schema)).unwrap();
                match validator.validate(value).map_err(|errs| {
                    errs.into_iter()
                        .map(|e| e.to_string())
                        .collect::<Vec<String>>()
                        .join(", ")
                }) {
                    Ok(_) => Ok(self),
                    Err(e) => Err(e),
                }
            }
        }
    }

    /// Get default spec file
    pub fn get_default_spec_file() -> String {
        "../openapi.yaml".to_string()
    }

    /// Get spec file for era
    pub fn get_era_spec_file(era: SupportedEra) -> String {
        format!("../openapi-{}", era)
    }

    /// Get all spec files
    pub fn get_all_spec_files() -> Vec<String> {
        APISpec::get_all_spec_files_from("..")
    }

    /// Get all spec files in the directory
    pub fn get_all_spec_files_from(root_path: &str) -> Vec<String> {
        let mut open_api_spec_files = Vec::new();
        for entry in glob(&format!("{}/openapi*.yaml", root_path)).unwrap() {
            let entry_path = entry.unwrap().to_str().unwrap().to_string();
            open_api_spec_files.push(entry_path.clone());
            open_api_spec_files.push(entry_path);
        }

        open_api_spec_files
    }

    /// Verify that examples are conform to the type definition.
    pub fn verify_examples(&self) -> Vec<String> {
        self.verify_examples_value("", &self.openapi)
    }

    fn verify_examples_value(&self, path_to_value: &str, root_value: &Value) -> Vec<String> {
        let mut errors: Vec<String> = vec![];

        errors.append(&mut self.verify_example_conformity(path_to_value, root_value));

        if let Some(object) = root_value.as_object() {
            for (value_key, value) in object {
                errors.append(
                    &mut self.verify_examples_value(&format!("{path_to_value} {value_key}"), value),
                );
            }
        }

        if let Some(array) = root_value.as_array() {
            for value in array {
                errors
                    .append(&mut self.verify_examples_value(&format!("{path_to_value}[?]"), value));
            }
        }

        errors
    }

    fn verify_example_conformity(&self, name: &str, component: &Value) -> Vec<String> {
        if let Some(example) = component.get("example") {
            // The type definition is at the same level as the example (components) unless there is a schema property (paths).
            let component_definition = component.get("schema").unwrap_or(component);

            let result = self.validate_conformity(example, component_definition);
            if let Err(e) = result {
                return vec![format!(
                    "- {}: Error\n    {}\n    Example: {}\n",
                    name, e, example
                )];
            }
        }

        vec![]
    }
}

// TODO: For now, it verifies only one parameter,
// should verify with multiple query parameters using an openapi.yaml file for test.
fn check_query_parameter_limitations(url: &Url, operation_object: &Value) {
    if url.query_pairs().count() >= 2 {
        panic!("This method does not work with multiple parameters");
    }

    if let Some(parameters) = operation_object["parameters"].as_array() {
        let len = parameters
            .iter()
            .filter(|p| p["in"].eq("query"))
            .collect::<Vec<_>>()
            .len();
        if len >= 2 {
            panic!("This method does not work with multiple parameters");
        }
    }
}

#[cfg(test)]
mod tests {
    use std::fs;
    use std::path::{Path, PathBuf};
    use warp::http::Method;
    use warp::http::StatusCode;

    use super::*;
    use crate::entities;
    use crate::messages::{CertificatePendingMessage, SignerMessagePart};
    use crate::test_utils::{fake_data, TempDir};

    fn build_empty_response(status_code: u16) -> Response<Bytes> {
        Response::builder()
            .status(status_code)
            .body(Bytes::new())
            .unwrap()
    }

    fn build_json_response<T: Serialize>(status_code: u16, value: T) -> Response<Bytes> {
        Response::builder()
            .status(status_code)
            .body(Bytes::from(json!(value).to_string().into_bytes()))
            .unwrap()
    }

    fn build_response(status_code: u16, content: &'static [u8]) -> Response<Bytes> {
        Response::builder()
            .status(status_code)
            .body(Bytes::from_static(content))
            .unwrap()
    }

    fn get_temp_dir(dir_name: &str) -> PathBuf {
        TempDir::create("apispec", dir_name)
    }

    fn get_temp_openapi_filename(name: &str, id: u32) -> PathBuf {
        get_temp_dir(&format!("{name}-{id}")).join("openapi.yaml")
    }

    fn write_minimal_open_api_file(
        version: &str,
        path: &Path,
        openapi_paths: &str,
        openapi_components: &str,
    ) {
        fs::write(
            path,
            format!(
                r#"openapi: "3.0.0"
info:
  version: {version}
  title: Minimal Open Api File

paths:
{openapi_paths}

components:
  schemas:
{openapi_components}
"#
            ),
        )
        .unwrap()
    }

    /// To check that the example is verified,
    /// we create an openapi.yaml with an invalid example.
    /// If the example is verified, we should have an error message.
    /// A simple invalid example is one with a wrong type (string instead of integer)
    fn check_example_error_is_detected(
        id: u32,
        paths: &str,
        components: &str,
        expected_error_message: &str,
    ) {
        let file = get_temp_openapi_filename("example", id);

        write_minimal_open_api_file("1.0.0", &file, paths, components);

        let api_spec = APISpec::from_file(file.to_str().unwrap());
        let errors: Vec<String> = api_spec.verify_examples();

        assert_eq!(1, errors.len());
        let error_message = errors.first().unwrap();
        assert!(
            error_message.contains(expected_error_message),
            "Error message: {:?}\nshould contains: {}\n",
            errors,
            expected_error_message
        );
    }

    #[test]
    fn test_validate_a_response_without_body() {
        APISpec::from_file(&APISpec::get_default_spec_file())
            .method(Method::GET.as_str())
            .path("/certificate-pending")
            .validate_request(&Null)
            .unwrap()
            .validate_response(&build_empty_response(204))
            .unwrap();
    }

    #[test]
    fn test_validate_ok_when_request_without_body_and_expects_response() {
        APISpec::from_file(&APISpec::get_default_spec_file())
            .method(Method::GET.as_str())
            .path("/certificate-pending")
            .validate_request(&Null)
            .unwrap()
            .validate_response(&build_json_response(
                200,
                CertificatePendingMessage::dummy(),
            ))
            .unwrap();
    }

    #[test]
    fn test_validate_ok_when_request_with_body_and_expects_no_response() {
        assert!(APISpec::from_file(&APISpec::get_default_spec_file())
            .method(Method::POST.as_str())
            .path("/register-signer")
            .validate_request(&SignerMessagePart::dummy())
            .unwrap()
            .validate_response(&Response::<Bytes>::new(Bytes::new()))
            .is_err());
    }

    #[test]
    fn test_validate_ok_when_response_match_default_status_code() {
        // INTERNAL_SERVER_ERROR(500) is not one of the defined status code
        // for this route, so it's the default response spec that is used.
        let response = build_json_response(
            StatusCode::INTERNAL_SERVER_ERROR.into(),
            entities::InternalServerError::new("an error occurred".to_string()),
        );

        APISpec::from_file(&APISpec::get_default_spec_file())
            .method(Method::POST.as_str())
            .path("/register-signer")
            .validate_response(&response)
            .unwrap();
    }

    #[test]
    fn test_should_fail_when_the_status_code_is_not_the_expected_one() {
        let response = build_json_response(
            StatusCode::INTERNAL_SERVER_ERROR.into(),
            entities::InternalServerError::new("an error occurred".to_string()),
        );

        let mut api_spec = APISpec::from_file(&APISpec::get_default_spec_file());
        let result = api_spec
            .method(Method::GET.as_str())
            .path("/certificate-pending")
            .validate_request(&Null)
            .unwrap()
            .validate_status(&response, &StatusCode::OK);

        assert!(result.is_err());
        assert_eq!(
            result.err().unwrap().to_string(),
            format!(
                "expected status code {} but was {}",
                StatusCode::OK.as_u16(),
                StatusCode::INTERNAL_SERVER_ERROR.as_u16()
            )
        );
    }

    #[test]
    fn test_should_be_ok_when_the_status_code_is_the_right_one() {
        let response = build_json_response(
            StatusCode::INTERNAL_SERVER_ERROR.into(),
            entities::InternalServerError::new("an error occurred".to_string()),
        );

        APISpec::from_file(&APISpec::get_default_spec_file())
            .method(Method::GET.as_str())
            .path("/certificate-pending")
            .validate_request(&Null)
            .unwrap()
            .validate_status(&response, &StatusCode::INTERNAL_SERVER_ERROR)
            .unwrap();
    }

    #[test]
    fn test_validate_returns_error_when_route_does_not_exist() {
        let mut api_spec = APISpec::from_file(&APISpec::get_default_spec_file());
        let result = api_spec
            .method(Method::GET.as_str())
            .path("/route-not-existing-in-openapi-spec")
            .validate_response(&build_response(200, b"abcdefgh"));

        assert!(result.is_err());
        assert_eq!(
            result.err(),
            Some("Unmatched path and method: /route-not-existing-in-openapi-spec GET".to_string())
        );
    }

    #[test]
    fn test_validate_returns_error_when_route_exists_but_method_does_not() {
        let mut api_spec = APISpec::from_file(&APISpec::get_default_spec_file());
        let result = api_spec
            .method(Method::OPTIONS.as_str())
            .path("/certificate-pending")
            .validate_response(&build_response(200, b"abcdefgh"));

        assert!(result.is_err());
        assert_eq!(
            result.err(),
            Some("Unmatched path and method: /certificate-pending OPTIONS".to_string())
        );
    }
    #[test]
    fn test_validate_returns_error_when_route_exists_but_expects_non_empty_response() {
        let mut api_spec = APISpec::from_file(&APISpec::get_default_spec_file());
        let result = api_spec
            .method(Method::GET.as_str())
            .path("/certificate-pending")
            .validate_response(&build_empty_response(200));

        assert!(result.is_err());
        assert_eq!(result.err(), Some("Non empty body expected".to_string()));
    }

    #[test]
    fn test_validate_returns_error_when_route_exists_but_expects_empty_response() {
        {
            let mut api_spec = APISpec::from_file(&APISpec::get_default_spec_file());
            let result = api_spec
                .method(Method::POST.as_str())
                .path("/register-signer")
                .validate_response(&build_response(201, b"abcdefgh"));

            assert!(result.is_err());
            assert_eq!(
                result.err(),
                Some("Expected empty body but got: b\"abcdefgh\"".to_string())
            );
        }
        {
            let mut api_spec = APISpec::from_file(&APISpec::get_default_spec_file());
            let result = api_spec
                .method(Method::POST.as_str())
                .path("/register-signer")
                .validate_response(&build_json_response(201, "something"));

            assert!(result.is_err());
            assert_eq!(
                result.err(),
                Some("Expected empty body but got: b\"\\\"something\\\"\"".to_string())
            );
        }
    }

    #[test]
    fn test_validate_returns_error_when_json_is_not_valid() {
        let mut api_spec = APISpec::from_file(&APISpec::get_default_spec_file());
        let result = api_spec
            .method(Method::GET.as_str())
            .path("/certificate-pending")
            .validate_request(&Null)
            .unwrap()
            .validate_response(&build_response(200, b"not a json"));
        assert_eq!(
            result.err(),
            Some("Expected a valid json but got: b\"not a json\"".to_string())
        );
    }

    #[test]
    fn test_validate_returns_errors_when_route_exists_but_does_not_expect_request_body() {
        assert!(APISpec::from_file(&APISpec::get_default_spec_file())
            .method(Method::GET.as_str())
            .path("/certificate-pending")
            .validate_request(&fake_data::beacon())
            .is_err());
    }
    #[test]
    fn test_validate_returns_error_when_route_exists_but_expects_non_empty_request_body() {
        assert!(APISpec::from_file(&APISpec::get_default_spec_file())
            .method(Method::POST.as_str())
            .path("/register-signer")
            .validate_request(&Null)
            .is_err());
    }

    #[test]
    fn test_validate_returns_error_when_content_type_does_not_exist() {
        let mut api_spec = APISpec::from_file(&APISpec::get_default_spec_file());
        let result = api_spec
            .method(Method::GET.as_str())
            .path("/certificate-pending")
            .content_type("whatever")
            .validate_request(&Null)
            .unwrap()
            .validate_response(&build_empty_response(200));

        assert!(result.is_err());
        assert_eq!(
            result.err().unwrap().to_string(),
            "Expected content type 'whatever' but spec is '{\"application/json\":{\"schema\":{\"$ref\":\"#/components/schemas/CertificatePendingMessage\"}}}'",
        );
    }

    #[test]
    fn test_validate_a_response_with_query_parameters() {
        APISpec::from_file(&APISpec::get_default_spec_file())
            .method(Method::GET.as_str())
            .path("/proof/cardano-transaction?transaction_hashes={hash}")
            .validate_request(&Null)
            .unwrap()
            .validate_response(&build_empty_response(404))
            .map(|_apispec| ())
            .unwrap();
    }

    #[test]
    fn test_validate_a_request_with_wrong_query_parameter_name() {
        let mut api_spec = APISpec::from_file(&APISpec::get_default_spec_file());
        let result = api_spec
            .method(Method::GET.as_str())
            .path("/proof/cardano-transaction?whatever=123")
            .validate_request(&Null);

        assert!(result.is_err());
        assert_eq!(
            result.err().unwrap().to_string(),
            "Unexpected query parameter 'whatever'",
        );
    }

    #[test]
    fn test_validate_a_request_should_failed_when_query_parameter_is_in_path() {
        let mut api_spec = APISpec::from_file(&APISpec::get_default_spec_file());
        let result = api_spec
            .method(Method::GET.as_str())
            .path("/artifact/cardano-transaction/{hash}?hash=456")
            .validate_request(&Null);

        assert!(result.is_err());
        assert_eq!(
            result.err().unwrap().to_string(),
            "Unexpected query parameter 'hash'",
        );
    }

    #[test]
    fn test_validate_query_parameters_with_correct_parameter_name() {
        let api_spec = APISpec::from_file(&APISpec::get_default_spec_file());
        api_spec
            .validate_query_parameters(
                "/proof/cardano-transaction?transaction_hashes=a123,b456",
                &api_spec.openapi["paths"]["/proof/cardano-transaction"]["get"],
            )
            .map(|_apispec| ())
            .unwrap()
    }

    #[test]
    fn test_validate_query_parameters_with_wrong_query_parameter_name() {
        let api_spec = APISpec::from_file(&APISpec::get_default_spec_file());
        let result = api_spec.validate_query_parameters(
            "/proof/cardano-transaction?whatever=123",
            &api_spec.openapi["paths"]["/proof/cardano-transaction"]["get"],
        );

        assert!(result.is_err());
        assert_eq!(
            result.err().unwrap().to_string(),
            "Unexpected query parameter 'whatever'",
        );
    }

    #[test]
    fn test_verify_conformity_with_expected_status() {
        APISpec::verify_conformity(
            APISpec::get_all_spec_files(),
            Method::GET.as_str(),
            "/certificate-pending",
            "application/json",
            &Null,
            &build_json_response(200, CertificatePendingMessage::dummy()),
            &StatusCode::OK,
        )
        .unwrap()
    }

    #[test]
    fn test_verify_conformity_with_non_expected_status_returns_error() {
        let response = build_json_response(200, CertificatePendingMessage::dummy());

        let spec_file = APISpec::get_default_spec_file();
        let result = APISpec::verify_conformity(
            vec![spec_file.clone()],
            Method::GET.as_str(),
            "/certificate-pending",
            "application/json",
            &Null,
            &response,
            &StatusCode::BAD_REQUEST,
        );

        let error_reason = format!(
            "expected status code {} but was {}",
            StatusCode::BAD_REQUEST.as_u16(),
            StatusCode::OK.as_u16()
        );
        let error_message = format!(
            "OpenAPI invalid response in {spec_file} on route /certificate-pending, reason: {error_reason}\nresponse: {response:#?}"
        );
        assert!(result.is_err());
        assert_eq!(result.err().unwrap().to_string(), error_message);
    }

    #[test]
    fn test_verify_conformity_when_no_spec_file_returns_error() {
        let result = APISpec::verify_conformity(
            vec![],
            Method::GET.as_str(),
            "/certificate-pending",
            "application/json",
            &Null,
            &build_json_response(200, CertificatePendingMessage::dummy()),
            &StatusCode::OK,
        );

        assert!(result.is_err());
        assert_eq!(
            result.err().unwrap().to_string(),
            "OpenAPI need a spec file to validate conformity. None were given."
        );
    }

    #[test]
    fn test_get_all_spec_files_not_empty() {
        let spec_files = APISpec::get_all_spec_files();
        assert!(!spec_files.is_empty());
        assert!(spec_files.contains(&APISpec::get_default_spec_file()))
    }

    #[test]
    fn test_example_is_verified_on_object() {
        let components = r#"
        MyComponent:
            type: object
            properties:
                id:
                    type: integer
            example:
                {
                    "id": "abc",
                }
        "#;
        check_example_error_is_detected(
            line!(),
            "",
            components,
            "\"abc\" is not of type \"integer\"",
        );
    }

    #[test]
    fn test_example_is_verified_on_array() {
        let components = r#"
        MyComponent:
            type: array
            items:
                type: integer
            example:
                [
                    "abc"
                ]
      "#;
        check_example_error_is_detected(
            line!(),
            "",
            components,
            "\"abc\" is not of type \"integer\"",
        );
    }

    #[test]
    fn test_example_is_verified_on_array_item() {
        let components = r#"
        MyComponent:
            type: array
            items:
                type: integer
                example: 
                    "abc"
        "#;
        check_example_error_is_detected(
            line!(),
            "",
            components,
            "\"abc\" is not of type \"integer\"",
        );
    }

    #[test]
    fn test_example_is_verified_on_parameter() {
        let paths = r#"
        /my_route:
            get:
                parameters:
                    -   name: id
                        in: path
                        schema:
                            type: integer
                        example: "abc"
        "#;
        check_example_error_is_detected(line!(), paths, "", "\"abc\" is not of type \"integer\"");
    }

    #[test]
    fn test_example_is_verified_on_array_parameter() {
        let paths = r#"
        /my_route:
            get:
                parameters:
                    -   name: id
                        in: path
                        schema:
                            type: array
                            items:
                                type: integer
                        example: 
                            [
                                "abc"
                            ]
        "#;
        check_example_error_is_detected(line!(), paths, "", "\"abc\" is not of type \"integer\"");
    }

    #[test]
    fn test_example_is_verified_on_array_parameter_schema() {
        let paths = r#"
        /my_route:
            get:
                parameters:
                    -   name: id
                        in: path
                        schema:
                            type: array
                            items:
                                type: integer
                            example: 
                                [
                                    "abc"
                                ]
        "#;
        check_example_error_is_detected(line!(), paths, "", "\"abc\" is not of type \"integer\"");
    }

    #[test]
    fn test_example_is_verified_on_array_parameter_item() {
        let paths = r#"
        /my_route:
            get:
                parameters:
                    -   name: id
                        in: path
                        schema:
                            type: array
                            items:
                                type: integer
                                example: 
                                    "abc"
        "#;
        check_example_error_is_detected(line!(), paths, "", "\"abc\" is not of type \"integer\"");
    }

    #[test]
    fn test_example_is_verified_on_referenced_component() {
        let paths = r#"
        /my_route:
            get:
                parameters:
                    -   name: id
                        in: path
                        schema:
                            $ref: '#/components/schemas/MyComponent'
                        example: "abc"
        "#;
        let components = r#"
        MyComponent:
            type: integer
        "#;

        check_example_error_is_detected(
            line!(),
            paths,
            components,
            "\"abc\" is not of type \"integer\"",
        );
    }
}
