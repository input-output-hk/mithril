//! Tools to helps validate conformity to an OpenAPI specification

use glob::glob;
use jsonschema::JSONSchema;
// TODO APISpec is used only for test in modules but we need to add this dependency to expose it.
// Can we use the feature "test_tools" ?
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
    ) {
        for spec_file in spec_files {
            APISpec::from_file(&spec_file)
                .method(method)
                .path(path)
                .content_type(content_type)
                .validate_request(request_body)
                .map_err(|e| panic!("OpenAPI invalid request in {spec_file}, reason: {e}\nresponse: {response:#?}"))
                .unwrap()
                .validate_response(response)
                .map_err(|e| panic!("OpenAPI invalid response in {spec_file}, reason: {e}\nresponse: {response:#?}"))
                .unwrap();
        }
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
    pub fn validate_request(
        &'a mut self,
        request_body: &impl Serialize,
    ) -> Result<&mut APISpec, String> {
        let path = self.path.unwrap();
        let method = self.method.unwrap().to_lowercase();
        let content_type = self.content_type.unwrap();

        let request_schema = &mut self.openapi.clone()["paths"][path][method]["requestBody"]
            ["content"][content_type]["schema"];
        let value = &json!(&request_body);
        self.validate_conformity(value, request_schema)
    }

    /// Validates if the status is the expected one
    pub fn validate_status(
        &'a mut self,
        response: &Response<Bytes>,
        expected_status_code: &StatusCode,
    ) -> Result<&mut APISpec, String> {
        if expected_status_code.as_u16() != response.status().as_u16() {
            return Err(format!(
                "expected status code {} but was {}",
                expected_status_code.as_u16(),
                response.status().as_u16(),
            ));
        }

        return Ok(self);
    }

    /// Validates if a response is valid
    pub fn validate_response_and_status(
        &'a mut self,
        response: &Response<Bytes>,
        expected_status_code: &StatusCode,
    ) -> Result<&mut APISpec, String> {
        self.validate_status(response, expected_status_code)
            .and_then(|s| s.validate_response(response))

        // // Return ????
        // let result = self.validate_status(response, expected_status_code);
        // // Recreate Err because it need a Result<&mut APISpec, String> when Result<&APISpec, String> is return
        // // Can we do in a better way ?
        // if let Err(err) = result {
        //     return Err(err);
        // }

        // // if expected_status_code.as_u16() != response.status().as_u16() {
        // //     return Err(format!(
        // //         "expected status code {} but was {}",
        // //         expected_status_code.as_u16(),
        // //         response.status().as_u16(),
        // //     ));
        // // }

        // return self.validate_response(response);
    }
    /// Validates if a response is valid
    pub fn validate_response(
        &'a mut self,
        response: &Response<Bytes>,
    ) -> Result<&mut APISpec, String> {
        let body = response.body();
        let status = response.status();

        let path = self.path.unwrap();
        let method = self.method.unwrap().to_lowercase();
        let content_type = self.content_type.unwrap();
        let mut openapi = self.openapi.clone();

        let response_spec = {
            match &mut openapi["paths"][path][&method]["responses"] {
                Null => None,
                response_spec => {
                    let status_code = status.as_str();
                    if response_spec.as_object().unwrap().contains_key(status_code) {
                        Some(&response_spec[status_code])
                    } else {
                        Some(&response_spec["default"])
                    }
                }
            }
        };

        match response_spec {
            Some(response_spec) => {
                let response_schema = &mut response_spec.clone()["content"][content_type]["schema"];
                if body.is_empty() {
                    match response_spec.as_object() {
                        Some(_) => match response_schema.as_object() {
                            Some(_) => Err("non empty body expected".to_string()),
                            None => Ok(self),
                        },
                        None => Err("empty body expected".to_string()),
                    }
                } else {
                    match &serde_json::from_slice(body) {
                        Ok(value) => self.validate_conformity(value, response_schema),
                        Err(_) => Err("non empty body expected".to_string()),
                    }
                }
            }
            None => Err(format!("unmatched path and method: {path} {method}")),
        }
    }

    /// Validates conformity of a value against a schema
    pub fn validate_conformity(
        &'a mut self,
        value: &Value,
        schema: &mut Value,
    ) -> Result<&mut APISpec, String> {
        match schema {
            Null => match value {
                Null => Ok(self),
                _ => Err(format!("Expected nothing but got: {value:?}")),
            },
            _ => {
                let schema = &mut schema.as_object_mut().unwrap().clone();
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
    pub fn get_defaut_spec_file() -> String {
        "../openapi.yaml".to_string()
    }

    /// Get spec file for era
    pub fn get_era_spec_file(era: SupportedEra) -> String {
        format!("../openapi-{}", era)
    }

    /// Get all spec files
    pub fn get_all_spec_files() -> Vec<String> {
        let mut open_api_spec_files = Vec::new();
        for entry in glob("../openapi*.yaml").unwrap() {
            let entry_path = entry.unwrap().to_str().unwrap().to_string();
            open_api_spec_files.push(entry_path.clone());
            open_api_spec_files.push(entry_path);
        }

        open_api_spec_files
    }
}

#[cfg(test)]
mod tests {
    use warp::http::Method;
    use warp::http::StatusCode;

    use super::*;
    use crate::entities;
    use crate::messages::{CertificatePendingMessage, SignerMessagePart};
    use crate::test_utils::fake_data;

    #[test]
    fn test_apispec_validate_ok() {
        // Route exists and does not expect request body, but expects response
        assert!(APISpec::from_file(&APISpec::get_defaut_spec_file())
            .method(Method::GET.as_str())
            .path("/certificate-pending")
            .validate_request(&Null)
            .unwrap()
            .validate_response(&Response::<Bytes>::new(Bytes::from(
                json!(CertificatePendingMessage::dummy())
                    .to_string()
                    .into_bytes(),
            )))
            .is_ok());

        // Route exists and expects request body, but does not expect response
        assert!(APISpec::from_file(&APISpec::get_defaut_spec_file())
            .method(Method::POST.as_str())
            .path("/register-signer")
            .validate_request(&SignerMessagePart::dummy())
            .unwrap()
            .validate_response(&Response::<Bytes>::new(Bytes::new()))
            .is_err());

        // Route exists and matches default status code
        let mut response = Response::<Bytes>::new(Bytes::from(
            json!(&entities::InternalServerError::new(
                "an error occurred".to_string(),
            ))
            .to_string()
            .into_bytes(),
        ));
        *response.status_mut() = StatusCode::INTERNAL_SERVER_ERROR;
        assert!(APISpec::from_file(&APISpec::get_defaut_spec_file())
            .method(Method::POST.as_str())
            .path("/register-signer")
            .validate_response(&response)
            .is_ok());
    }

    #[test]
    fn test_apispec_should_fail_when_the_status_code_is_not_the_expected_one() {
        // Route exists and matches default status code
        let mut response = Response::<Bytes>::new(Bytes::from(
            json!(&entities::InternalServerError::new(
                "an error occurred".to_string(),
            ))
            .to_string()
            .into_bytes(),
        ));
        *response.status_mut() = StatusCode::INTERNAL_SERVER_ERROR;
        {
            let mut api_spec = APISpec::from_file(&APISpec::get_defaut_spec_file());
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
        {
            let mut api_spec = APISpec::from_file(&APISpec::get_defaut_spec_file());
            let result = api_spec
                .method(Method::GET.as_str())
                .path("/certificate-pending")
                .validate_request(&Null)
                .unwrap()
                .validate_response_and_status(&response, &StatusCode::OK);

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
    }

    #[test]
    fn test_apispec_should_be_ok_when_the_status_code_is_the_right_one() {
        // Route exists and matches default status code
        let mut response = Response::<Bytes>::new(Bytes::from(
            json!(&entities::InternalServerError::new(
                "an error occurred".to_string(),
            ))
            .to_string()
            .into_bytes(),
        ));
        *response.status_mut() = StatusCode::INTERNAL_SERVER_ERROR;

        // Is it better to use unwrap to see the error message ?
        let mut api_spec = APISpec::from_file(&APISpec::get_defaut_spec_file());
        let result = api_spec
            .method(Method::GET.as_str())
            .path("/certificate-pending")
            .validate_request(&Null)
            .unwrap()
            .validate_status(&response, &StatusCode::INTERNAL_SERVER_ERROR);

        // TODO it could be better to not have to unwrap
        // We can call one function with a chain of validations
        // We can return an ApiSpec with function is_ok and is_err.

        assert!(result.is_ok());
        // Can we put Debug on ApiSpec to uses expect_err ?

        // How to specify this is the response we expect ? Because it is not here
        // Validate response check there is one response expected.
        // We want to specify the one expected.

        let mut api_spec = APISpec::from_file(&APISpec::get_defaut_spec_file());
        let result = api_spec
            .method(Method::GET.as_str())
            .path("/certificate-pending")
            .validate_request(&Null)
            .unwrap()
            .validate_response_and_status(&response, &StatusCode::INTERNAL_SERVER_ERROR);

        assert!(result.is_ok());
    }

    #[test]
    fn test_apispec_validate_errors() {
        // Route does not exist
        assert!(APISpec::from_file(&APISpec::get_defaut_spec_file())
            .method(Method::GET.as_str())
            .path("/route-not-existing-in-openapi-spec")
            .validate_response(&Response::<Bytes>::new(Bytes::from_static(b"abcdefgh")))
            .is_err());

        // Route exists, but method does not
        assert!(APISpec::from_file(&APISpec::get_defaut_spec_file())
            .method(Method::OPTIONS.as_str())
            .path("/certificate-pending")
            .validate_response(&Response::<Bytes>::new(Bytes::from_static(b"abcdefgh")))
            .is_err());

        // Route exists, but expects non empty reponse
        assert!(APISpec::from_file(&APISpec::get_defaut_spec_file())
            .method(Method::GET.as_str())
            .path("/certificate-pending")
            .validate_response(&Response::<Bytes>::new(Bytes::new()))
            .is_err());

        // Route exists, but expects empty reponse
        assert!(APISpec::from_file(&APISpec::get_defaut_spec_file())
            .method(Method::POST.as_str())
            .path("/register-signer")
            .validate_response(&Response::<Bytes>::new(Bytes::from_static(b"abcdefgh")))
            .is_err());

        // Route exists, but does not expect request body
        assert!(APISpec::from_file(&APISpec::get_defaut_spec_file())
            .method(Method::GET.as_str())
            .path("/certificate-pending")
            .validate_request(&fake_data::beacon())
            .is_err());

        // Route exists, but expects non empty request body
        assert!(APISpec::from_file(&APISpec::get_defaut_spec_file())
            .method(Method::POST.as_str())
            .path("/register-signer")
            .validate_request(&Null)
            .is_err());
    }

    #[test]
    fn test_get_all_spec_files_not_empty() {
        let spec_files = APISpec::get_all_spec_files();
        assert!(!spec_files.is_empty());
        assert!(spec_files.contains(&APISpec::get_defaut_spec_file()))
    }
}
