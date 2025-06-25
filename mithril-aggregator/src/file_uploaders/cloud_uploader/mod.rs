mod api;
mod gcloud_backend;
mod interface;

pub use api::*;
pub use gcloud_backend::*;
pub use interface::*;

use percent_encoding::{AsciiSet, NON_ALPHANUMERIC, utf8_percent_encode};

const ENCODE_SET: &AsciiSet = &NON_ALPHANUMERIC
    .remove(b'*')
    .remove(b'-')
    .remove(b'.')
    .remove(b'_');

/// Encode a string for use in a GCP URL, satisfying: https://cloud.google.com/storage/docs/request-endpoints#encoding
pub fn gcp_percent_encode(input: &str) -> String {
    utf8_percent_encode(input, ENCODE_SET).to_string()
}
