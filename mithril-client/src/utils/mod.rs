//! Utilities module
//! This module contains tools needed mostly for the snapshot download and unpack.

cfg_fs! {
    mod ancillary_verifier;
    mod stream_reader;
    mod bootstrap_files;

    pub use ancillary_verifier::AncillaryVerifier;
    pub use stream_reader::*;
    pub use bootstrap_files::*;
}

cfg_fs_unstable! {
    mod fs;
    mod vec_deque_extensions;

    pub use fs::*;
    pub use vec_deque_extensions::VecDequeExtensions;
}
