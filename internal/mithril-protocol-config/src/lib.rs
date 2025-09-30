/// HTTP request timeout duration in milliseconds
const HTTP_REQUEST_TIMEOUT_DURATION: u64 = 30000;
pub mod http_client;
pub mod interface;
pub mod model;
pub mod test;

#[cfg(test)]
pub(crate) mod test_tools {
    mithril_common::define_test_logger!();
}
