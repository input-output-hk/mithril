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
    let parent_dir = std::env::temp_dir().join("mithril_test").join(subdir_name);

    if parent_dir.exists() {
        std::fs::remove_dir_all(&parent_dir)
            .unwrap_or_else(|e| panic!("Could not remove dir {parent_dir:?}: {e}"));
    }
    std::fs::create_dir_all(&parent_dir)
        .unwrap_or_else(|e| panic!("Could not create dir {parent_dir:?}: {e}"));

    parent_dir
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
