#![allow(missing_docs)]

mod decoder;
mod encoder;
mod single_signature;

pub use decoder::MessageDecoder;
pub use encoder::MessageEncoder;
pub use single_signature::{SingleSignatureMessage, SINGLE_SIGNATURE_MESSAGE_SCHEMA};
