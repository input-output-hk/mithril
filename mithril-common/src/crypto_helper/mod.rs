mod codec;
mod conversions;
pub mod tests_setup;
mod types;

pub use codec::*;
pub use types::*;

/// The current protocol version
pub const PROTOCOL_VERSION: ProtocolVersion = "0.1.0";
