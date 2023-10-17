use std::net::SocketAddr;

use mithril_common::{
    test_utils::test_http_server::{test_http_server, TestHttpServer},
    StdResult,
};
use reqwest::StatusCode;
use warp::Filter;

pub struct Relay {
    server: Option<TestHttpServer>,
}

impl Relay {
    pub fn new(_topic_name: &str) -> Self {
        Self { server: None }
    }

    pub async fn start(&mut self) -> StdResult<SocketAddr> {
        let server = test_http_server(
            warp::path("register-signatures")
                .map(|| warp::reply::with_status(warp::reply::reply(), StatusCode::CREATED)),
        );
        let address = server.address();
        self.server = Some(server);

        Ok(address)
    }
}
