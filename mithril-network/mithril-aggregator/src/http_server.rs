use log::{debug, info};
use std::convert::Infallible;
use std::net::IpAddr;
use warp::{http::Method, http::StatusCode, Filter};

use crate::entities;
use crate::fake_data;

const SERVER_BASE_PATH: &str = "aggregator";

/// Server
pub struct Server {
    ip: IpAddr,
    port: u16,
}

impl Server {
    /// Server factory
    pub fn new(ip: String, port: u16) -> Self {
        Self {
            ip: ip.parse::<IpAddr>().unwrap(),
            port: port,
        }
    }

    /// Start
    pub async fn start(&self) {
        info!("Start Server");
        let routes = router::routes();
        warp::serve(routes).run((self.ip, self.port)).await;
    }
}

mod router {
    use super::*;

    /// Routes
    pub fn routes() -> impl Filter<Extract = impl warp::Reply, Error = warp::Rejection> + Clone {
        let cors = warp::cors()
            .allow_any_origin()
            .allow_headers(vec!["content-type"])
            .allow_methods(vec![Method::GET, Method::POST, Method::OPTIONS]);

        let routes = warp::any().and(warp::path(SERVER_BASE_PATH)).and(
            certificate_pending()
                .or(certificate_certificate_hash())
                .or(snapshots())
                .or(snapshot_digest())
                .or(register_signer())
                .or(register_signatures())
                .with(cors),
        );
        routes
    }

    /// GET /certificate-pending
    pub fn certificate_pending(
    ) -> impl Filter<Extract = impl warp::Reply, Error = warp::Rejection> + Clone {
        warp::path!("certificate-pending")
            .and(warp::get())
            .and_then(handlers::certificate_pending)
    }

    /// GET /certificate/{certificate_hash}
    pub fn certificate_certificate_hash(
    ) -> impl Filter<Extract = impl warp::Reply, Error = warp::Rejection> + Clone {
        warp::path!("certificate" / String)
            .and(warp::get())
            .and_then(handlers::certificate_certificate_hash)
    }

    /// GET /snapshots
    pub fn snapshots() -> impl Filter<Extract = impl warp::Reply, Error = warp::Rejection> + Clone {
        warp::path!("snapshots")
            .and(warp::get())
            .and_then(handlers::snapshots)
    }

    /// GET /snapshot/digest
    pub fn snapshot_digest(
    ) -> impl Filter<Extract = impl warp::Reply, Error = warp::Rejection> + Clone {
        warp::path!("snapshot" / String)
            .and(warp::get())
            .and_then(handlers::snapshot_digest)
    }

    /// POST /register-signer
    pub fn register_signer(
    ) -> impl Filter<Extract = impl warp::Reply, Error = warp::Rejection> + Clone {
        warp::path!("register-signer")
            .and(warp::post())
            .and(warp::body::json())
            .and_then(handlers::register_signer)
    }

    /// POST /register-signatures
    pub fn register_signatures(
    ) -> impl Filter<Extract = impl warp::Reply, Error = warp::Rejection> + Clone {
        warp::path!("register-signatures")
            .and(warp::post())
            .and(warp::body::json())
            .and_then(handlers::register_signatures)
    }
}

mod handlers {
    use super::*;

    /// Certificate Pending
    pub async fn certificate_pending() -> Result<impl warp::Reply, Infallible> {
        debug!("certificate_pending");

        // Certificate pending
        let certificate_pending = fake_data::certificate_pending();

        Ok(warp::reply::json(&certificate_pending))
    }

    /// Certificate by certificate hash
    pub async fn certificate_certificate_hash(
        certificate_hash: String,
    ) -> Result<impl warp::Reply, Infallible> {
        debug!("certificate_certificate_hash/{}", certificate_hash);

        // Certificate
        let certificate = fake_data::certificate(certificate_hash);

        Ok(warp::reply::json(&certificate))
    }

    /// Snapshots
    pub async fn snapshots() -> Result<impl warp::Reply, Infallible> {
        debug!("snapshots");

        // Snapshots
        let snapshots = fake_data::snapshots(10);

        Ok(warp::reply::json(&snapshots))
    }

    /// Snapshot by digest
    pub async fn snapshot_digest(digest: String) -> Result<impl warp::Reply, Infallible> {
        debug!("snapshot_digest/{}", digest);

        // Snapshot
        let snapshots = fake_data::snapshots(10);
        let snapshot = snapshots.last();

        Ok(warp::reply::json(&snapshot))
    }

    /// Register Signer
    pub async fn register_signer(signer: entities::Signer) -> Result<impl warp::Reply, Infallible> {
        debug!("register_signer/{:?}", signer);

        Ok(StatusCode::CREATED)
    }

    /// Register Signatures
    pub async fn register_signatures(
        signatures: Vec<entities::SingleSignature>,
    ) -> Result<impl warp::Reply, Infallible> {
        debug!("register_signatures/{:?}", signatures);

        Ok(StatusCode::CREATED)
    }
}

#[cfg(test)]
mod tests {
    const API_SPEC_FILE: &str = "../../openapi.yaml";

    use http::response::Response;
    use jsonschema::JSONSchema;
    use serde::Serialize;
    use serde_json::{json, Value, Value::Null};
    use warp::hyper::body::Bytes;
    use warp::test::request;

    use super::*;
    use crate::fake_data;

    /// APISpec helps validate conformity to an OpenAPI specification
    struct APISpec<'a> {
        openapi: Value,
        path: Option<&'a str>,
        method: Option<&'a str>,
    }

    impl<'a> APISpec<'a> {
        /// APISpec factory from spec
        fn from_file(path: &str) -> APISpec<'a> {
            let yaml_spec = std::fs::read_to_string(path).unwrap();
            let openapi: serde_json::Value = serde_yaml::from_str(&yaml_spec).unwrap();
            APISpec {
                openapi,
                path: None,
                method: None,
            }
        }

        /// Sets the path to specify/check.
        fn path(&'a mut self, path: &'a str) -> &mut APISpec {
            self.path = Some(path);
            self
        }

        /// Sets the method to specify/check.
        fn method(&'a mut self, method: &'a str) -> &mut APISpec {
            self.method = Some(method);
            self
        }

        /// Validates if a request is valid
        fn validate_request(
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
        fn validate_response(
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
            let response_schema =
                &mut response_spec.clone()["content"]["application/json"]["schema"];
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
        fn validate_conformity(
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

    #[tokio::test]
    async fn test_certificate_pending_get_ok() {
        let method = Method::GET.as_str();
        let path = "/certificate-pending";

        let response = request()
            .method(&method)
            .path(&format!("/{}{}", SERVER_BASE_PATH, path))
            .reply(&router::routes())
            .await;

        APISpec::from_file(API_SPEC_FILE)
            .method(&method)
            .path(&path)
            .validate_request(&Null)
            .unwrap()
            .validate_response(&response)
            .expect("OpenAPI error");
    }

    #[tokio::test]
    async fn test_certificate_certificate_hash_get_ok() {
        let method = Method::GET.as_str();
        let path = "/certificate/{certificate_hash}";

        let response = request()
            .method(&method)
            .path(&format!("/{}{}", SERVER_BASE_PATH, path))
            .reply(&router::routes())
            .await;

        APISpec::from_file(API_SPEC_FILE)
            .method(&method)
            .path(&path)
            .validate_request(&Null)
            .unwrap()
            .validate_response(&response)
            .expect("OpenAPI error");
    }

    #[tokio::test]
    async fn test_snapshots_get_ok() {
        let method = Method::GET.as_str();
        let path = "/snapshots";

        let response = request()
            .method(&method)
            .path(&format!("/{}{}", SERVER_BASE_PATH, path))
            .reply(&router::routes())
            .await;

        APISpec::from_file(API_SPEC_FILE)
            .method(&method)
            .path(&path)
            .validate_request(&Null)
            .unwrap()
            .validate_response(&response)
            .expect("OpenAPI error");
    }

    #[tokio::test]
    async fn test_snapshot_digest_get_ok() {
        let method = Method::GET.as_str();
        let path = "/snapshot/{digest}";

        let response = request()
            .method(&method)
            .path(&format!("/{}{}", SERVER_BASE_PATH, path))
            .reply(&router::routes())
            .await;

        APISpec::from_file(API_SPEC_FILE)
            .method(&method)
            .path(&path)
            .validate_request(&Null)
            .unwrap()
            .validate_response(&response)
            .expect("OpenAPI error");
    }

    #[tokio::test]
    async fn test_register_signer_post_ok() {
        let signer = &fake_data::signers(1)[0];

        let method = Method::POST.as_str();
        let path = "/register-signer";

        let response = request()
            .method(&method)
            .path(&format!("/{}{}", SERVER_BASE_PATH, path))
            .json(signer)
            .reply(&router::routes())
            .await;

        APISpec::from_file(API_SPEC_FILE)
            .method(&method)
            .path(&path)
            .validate_request(&signer)
            .unwrap()
            .validate_response(&response)
            .expect("OpenAPI error");
    }

    #[tokio::test]
    async fn test_register_signatures_post_ok() {
        let signatures = &fake_data::single_signatures(1);

        let method = Method::POST.as_str();
        let path = "/register-signatures";

        let response = request()
            .method(&method)
            .path(&format!("/{}{}", SERVER_BASE_PATH, path))
            .json(signatures)
            .reply(&router::routes())
            .await;

        APISpec::from_file(API_SPEC_FILE)
            .method(&method)
            .path(&path)
            .validate_request(&signatures)
            .unwrap()
            .validate_response(&response)
            .expect("OpenAPI error");
    }
}
