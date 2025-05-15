//! Utilities module
//! This module contains tools needed mostly for the snapshot download and unpack.

cfg_fs! {
    pub const ANCILLARIES_NOT_SIGNED_BY_MITHRIL:&str = "Ancillary verification does not use the Mithril certification: as a mitigation, IOG owned keys are used to sign these files.";

    mod ancillary_verifier;
    mod stream_reader;
    mod bootstrap_files;
    mod unexpected_downloaded_file_verifier;

    pub use ancillary_verifier::AncillaryVerifier;
    pub(crate) use unexpected_downloaded_file_verifier::*;
    pub use stream_reader::*;
    pub use bootstrap_files::*;
}

cfg_fs_unstable! {
    mod fs;
    mod vec_deque_extensions;

    pub use fs::*;
    pub use vec_deque_extensions::VecDequeExtensions;
}
