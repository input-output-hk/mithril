//! Utilities module
//! This module contains tools needed mostly for the snapshot download and unpack.

cfg_fs! {
    mod stream_reader;

    pub use stream_reader::*;
}
