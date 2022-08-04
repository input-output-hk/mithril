use std::error::Error as StdError;

mod certificate_handler;
mod entities;
<<<<<<< HEAD
mod protocol_initializer_store;
mod runtime;
=======
//mod runtime;
>>>>>>> [wip] refactor single signer
mod single_signer;
//mod state_machine;

pub use certificate_handler::{CertificateHandlerHTTPClient, DumbCertificateHandler};
pub use entities::Config;
<<<<<<< HEAD
pub use protocol_initializer_store::{ProtocolInitializerStore, ProtocolInitializerStorer};
pub use runtime::Runtime;
=======
//pub use runtime::Runtime;
>>>>>>> [wip] refactor single signer
pub use single_signer::MithrilSingleSigner;
//pub use state_machine::*;

type AsyncError = Box<dyn StdError + Sync + Send>;
