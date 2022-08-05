use std::error::Error as StdError;

mod certificate_handler;
mod entities;
mod protocol_initializer_store;
mod single_signer;
mod state_machine;

pub use certificate_handler::*;
pub use entities::Config;
pub use protocol_initializer_store::{ProtocolInitializerStore, ProtocolInitializerStorer};
pub use single_signer::*;
pub use state_machine::*;

type AsyncError = Box<dyn StdError + Sync + Send>;
