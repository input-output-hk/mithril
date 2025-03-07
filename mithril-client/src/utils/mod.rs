//! Utilities module
//! This module contains tools needed mostly for the snapshot download and unpack.

cfg_fs! {
    mod stream_reader;

    pub use stream_reader::*;
}

cfg_fs_unstable! {
    mod fs;
    mod vec_deque_extensions;

    pub use fs::*;
    pub use vec_deque_extensions::VecDequeExtensions;
}
