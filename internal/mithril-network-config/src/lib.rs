/// HTTP request timeout duration in milliseconds
const HTTP_REQUEST_TIMEOUT_DURATION: u64 = 30000;

mod aggregator_client;
mod from_epoch_settings;
pub mod http;
pub mod interface;
pub mod signer_epoch_settings;

#[cfg(test)]
pub(crate) mod test_tools {
    mithril_common::define_test_logger!();
}
