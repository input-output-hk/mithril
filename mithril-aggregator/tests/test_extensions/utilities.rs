use mithril_common::test_utils::TempDir;
use slog_scope::debug;
use std::{
    path::PathBuf,
    sync::atomic::{AtomicUsize, Ordering},
};

pub static COMMENT_COUNT: AtomicUsize = AtomicUsize::new(0);

/// Create a directory to save test artefacts. This directory is cleaned if it
/// already exists, it is created if not. This directory is kept at the end to
/// allow debuging.
pub fn get_test_dir(subdir_name: &str) -> PathBuf {
    TempDir::create("aggregator-integration", subdir_name)
}

pub fn comment(comment: String) {
    let old_count = COMMENT_COUNT.fetch_add(1, Ordering::SeqCst);
    debug!("COMMENT {:02} 💬 {}", old_count + 1, comment);
}

#[macro_export]
macro_rules! comment {
    ( $($comment:tt)* ) => {{
        test_extensions::utilities::comment(format!($($comment)*));
    }};
}
