#![cfg(test)]
const API_SPEC_FILE: &str = "../../openapi.yaml";

use http::response::Response;
use jsonschema::JSONSchema;
use serde::Serialize;
use serde_json::{json, Value, Value::Null};
use warp::hyper::body::Bytes;

/// APISpec helps validate conformity to an OpenAPI specification
pub(crate) struct APISpec<'a> {
    openapi: Value,
    path: Option<&'a str>,
    method: Option<&'a str>,
}

impl<'a> APISpec<'a> {
    /// APISpec factory from spec
    pub(crate) fn from_file(path: &str) -> APISpec<'a> {
        let yaml_spec = std::fs::read_to_string(path).unwrap();
        let openapi: serde_json::Value = serde_yaml::from_str(&yaml_spec).unwrap();
        APISpec {
            openapi,
            path: None,
            method: None,
        }
    }

    /// Sets the path to specify/check.
    pub(crate) fn path(&'a mut self, path: &'a str) -> &mut APISpec {
        self.path = Some(path);
        self
    }

    /// Sets the method to specify/check.
    pub(crate) fn method(&'a mut self, method: &'a str) -> &mut APISpec {
        self.method = Some(method);
        self
    }

    /// Validates if a request is valid
    pub(crate) fn validate_request(
        &'a mut self,
        request_body: &impl Serialize,
    ) -> Result<&mut APISpec, String> {
        let path = self.path.unwrap();
        let method = self.method.unwrap().to_lowercase();

        let request_schema = &mut self.openapi.clone()["paths"][path][method]["requestBody"]
            ["content"]["application/json"]["schema"];
        let value = &json!(&request_body);
        self.validate_conformity(value, request_schema)
    }

    /// Validates if a response is valid
    pub(crate) fn validate_response(
        &'a mut self,
        response: &Response<Bytes>,
    ) -> Result<&mut APISpec, String> {
        let body = response.body();
        let status = response.status();

        let path = self.path.unwrap();
        let method = self.method.unwrap().to_lowercase();
        let status_code = status.as_str();

        let response_spec =
            &mut self.openapi.clone()["paths"][path][method]["responses"][status_code];
        let response_schema = &mut response_spec.clone()["content"]["application/json"]["schema"];
        if body.is_empty() {
            match response_spec.as_object() {
                Some(_) => match response_schema.as_object() {
                    Some(_) => Err("non empty body expected".to_string()),
                    None => Ok(self),
                },
                None => Err("empty body expected".to_string()),
            }
        } else {
            match &serde_json::from_slice(&body) {
                Ok(value) => self.validate_conformity(value, response_schema),
                Err(_) => Err("non empty body expected".to_string()),
            }
        }
    }

    /// Validates conformity of a value against a schema
    pub(crate) fn validate_conformity(
        &'a mut self,
        value: &Value,
        schema: &mut Value,
    ) -> Result<&mut APISpec, String> {
        match schema {
            Null => match value {
                Null => Ok(self),
                _ => Err("null schema provided".to_string()),
            },
            _ => {
                let schema = &mut schema.as_object_mut().unwrap().clone();
                let components = self.openapi["components"].clone();
                schema.insert(String::from("components"), components);

                let validator = JSONSchema::compile(&json!(schema)).unwrap();
                match validator.validate(&value).map_err(|errs| {
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
}

#[cfg(test)]
mod tests {
    use warp::http::Method;

    use super::*;
    use crate::fake_data;

    #[test]
    fn test_apispec_validate_ok() {
        // Route exists and does not expect request body, but expects response
        assert!(APISpec::from_file(API_SPEC_FILE)
            .method(&Method::GET.as_str())
            .path(&"/certificate-pending")
            .validate_request(&Null)
            .unwrap()
            .validate_response(&Response::<Bytes>::new(Bytes::from(
                json!(fake_data::certificate_pending())
                    .to_string()
                    .into_bytes()
            )))
            .is_ok());

        // Route exists and expects request body, but does not expect response
        assert!(APISpec::from_file(API_SPEC_FILE)
            .method(&Method::POST.as_str())
            .path(&"/register-signer")
            .validate_request(&fake_data::signers(1)[0])
            .unwrap()
            .validate_response(&Response::<Bytes>::new(Bytes::new()))
            .is_err());
    }

    #[test]
    fn test_apispec_validate_errors() {
        // Route does not exist
        assert!(APISpec::from_file(API_SPEC_FILE)
            .method(&Method::GET.as_str())
            .path(&"/route-not-existing-in-openapi-spec")
            .validate_response(&Response::<Bytes>::new(Bytes::from_static(b"abcdefgh")))
            .is_err());

        // Route exists, but method does not
        assert!(APISpec::from_file(API_SPEC_FILE)
            .method(&Method::OPTIONS.as_str())
            .path(&"/certificate-pending")
            .validate_response(&Response::<Bytes>::new(Bytes::from_static(b"abcdefgh")))
            .is_err());

        // Route exists, but expects non empty reponse
        assert!(APISpec::from_file(API_SPEC_FILE)
            .method(&Method::GET.as_str())
            .path(&"/certificate-pending")
            .validate_response(&Response::<Bytes>::new(Bytes::new()))
            .is_err());

        // Route exists, but expects empty reponse
        assert!(APISpec::from_file(API_SPEC_FILE)
            .method(&Method::POST.as_str())
            .path(&"/register-signer")
            .validate_response(&Response::<Bytes>::new(Bytes::from_static(b"abcdefgh")))
            .is_err());

        // Route exists, but does not expect request body
        assert!(APISpec::from_file(API_SPEC_FILE)
            .method(&Method::GET.as_str())
            .path(&"/certificate-pending")
            .validate_request(&fake_data::beacon())
            .is_err());

        // Route exists, but expects non empty request body
        assert!(APISpec::from_file(API_SPEC_FILE)
            .method(&Method::POST.as_str())
            .path(&"/register-signer")
            .validate_request(&Null)
            .is_err());
    }
}
