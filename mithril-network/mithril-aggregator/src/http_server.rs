use log::{debug, info};
use std::convert::Infallible;
use std::net::IpAddr;
use warp::{http::Method, http::StatusCode, Filter};

use crate::entities;
use crate::fake_data;

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

        let routes = warp::any().and(warp::path("aggregator")).and(
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
    use jsonschema::JSONSchema;
    use openapiv3::OpenAPI;
    use serde_json::{json, Value};
    use warp::http::StatusCode;
    use warp::hyper::body::Bytes;
    use warp::test::request;

    use super::*;
    use crate::fake_data;

    struct APISpec<'a> {
        openapi: Value,
        path: Option<&'a str>,
        method: Option<&'a str>,
    }

    fn read_spec<'a>() -> APISpec<'a> {
        let yaml_spec = std::fs::read_to_string("../../openapi.yaml").unwrap();
        let openapi: serde_json::Value = serde_yaml::from_str(&yaml_spec).unwrap();

        APISpec {
            openapi,
            path: None,
            method: None,
        }
    }

    impl<'a> APISpec<'a> {
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

        fn validate(&mut self, bytes: &Bytes) -> Result<(), String> {
            let value: serde_json::Value =
                serde_json::from_str(std::str::from_utf8(&bytes).unwrap()).unwrap();

            let schema = &mut self.openapi["paths"][self.path.unwrap()]
                [self.method.unwrap().to_lowercase()]["responses"]["200"]["content"]
                ["application/json"]["schema"]
                .as_object_mut()
                .unwrap()
                .clone();
            let components = &mut self.openapi.as_object_mut().unwrap();
            schema.append(*components);

            let validator = JSONSchema::compile(&json!(schema)).unwrap();

            validator
                .validate(&value)
                .map_err(|errs| errs.into_iter().map(|e| e.to_string()).collect())
        }

        /// Verifies the given body matches the current path's expected output
        fn matches(&mut self, bytes: &Bytes) {
            let result = self.validate(bytes);
            assert!(result.is_ok());
        }

        /// Verifies the given body _does not_ match the current path's expected output
        fn does_not_match(&mut self, bytes: &Bytes) {
            let result = self.validate(bytes);

            match result {
                Err(_) => {}
                Ok(_) => assert!(false),
            }
        }
    }

    #[test]
    fn test_can_validate_api_route() {
        let mut openapi = read_spec();

        let spec = openapi.method("GET").path("/certificate-pending");
        let cert = fake_data::certificate_pending();

        spec.matches(&Bytes::copy_from_slice(
            serde_json::to_string(&cert).unwrap().as_bytes(),
        ));
    }

    #[test]
    fn test_can_invalidate_api_route() {
        let mut openapi = read_spec();

        let spec = openapi.method("GET").path("/certificate-pending");

        spec.does_not_match(&Bytes::copy_from_slice(b"{}"));
    }

    #[tokio::test]
    async fn test_certificate_pending_get() {
        let response = request()
            .method("GET")
            .path("/certificate-pending")
            .reply(&router::certificate_pending())
            .await;

        assert_eq!(response.status(), StatusCode::OK);

        let mut openapi: APISpec = read_spec();
        openapi
            .method("GET")
            .path("/certificate-pending")
            .matches(response.body());
    }

    #[tokio::test]
    async fn test_certificate_certificate_hash_get() {
        let response = request()
            .method("GET")
            .path("/certificate/hash123")
            .reply(&router::certificate_certificate_hash())
            .await;

        assert_eq!(response.status(), StatusCode::OK);
        assert_ne!(response.body(), "")
    }

    #[tokio::test]
    async fn test_snapshots_get() {
        let response = request()
            .method("GET")
            .path("/snapshots")
            .reply(&router::snapshots())
            .await;

        assert_eq!(response.status(), StatusCode::OK);
        assert_ne!(response.body(), "")
    }

    #[tokio::test]
    async fn test_snapshot_digest_get() {
        let response = request()
            .method("GET")
            .path("/snapshot/digest123")
            .reply(&router::snapshot_digest())
            .await;

        assert_eq!(response.status(), StatusCode::OK);
        assert_ne!(response.body(), "")
    }

    #[tokio::test]
    async fn test_register_signer_post() {
        let signer = &fake_data::signers(1)[0];
        let response = request()
            .method("POST")
            .path("/register-signer")
            .json(signer)
            .reply(&router::register_signer())
            .await;

        assert_eq!(response.status(), StatusCode::CREATED);
        assert_eq!(response.body(), "")
    }

    #[tokio::test]
    async fn test_register_signatures_post() {
        let signatures = &fake_data::single_signatures(1);
        let response = request()
            .method("POST")
            .path("/register-signatures")
            .json(signatures)
            .reply(&router::register_signatures())
            .await;

        assert_eq!(response.status(), StatusCode::CREATED);
        assert_eq!(response.body(), "")
    }
}
