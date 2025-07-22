use mithril_common::test::TempDir;
use slog_scope::debug;
use std::{
    path::PathBuf,
    sync::atomic::{AtomicUsize, Ordering},
};

pub static COMMENT_COUNT: AtomicUsize = AtomicUsize::new(0);

/// Create a directory to save test artifacts. This directory is cleaned if it
/// already exists, it is created if not. This directory is kept at the end to
/// allow debugging.
pub fn get_test_dir(subdir_name: &str) -> PathBuf {
    TempDir::create("aggregator-integration", subdir_name)
}

pub fn comment(comment: String) {
    let old_count = COMMENT_COUNT.fetch_add(1, Ordering::SeqCst);
    debug!("COMMENT {:02} 💬 {}", old_count + 1, comment);
}

pub fn tx_hash(block_number: u64, tx_index: u64) -> String {
    format!("tx_hash-{block_number}-{tx_index}")
}

#[macro_export]
macro_rules! comment {
    ( $($comment:tt)* ) => {{
        test_extensions::utilities::comment(format!($($comment)*));
    }};
}

#[macro_export]
macro_rules! async_wait {
    ( max_iter:$max_iter:expr, sleep_ms:$sleep_ms:expr, condition:$condition:expr, error_msg:$($error_msg:tt)*
    ) => {{
        let mut max_iteration: usize = $max_iter;
        while $condition {
            max_iteration -= 1;
            if max_iteration == 0 {
                return Err(anyhow::anyhow!($($error_msg)*));
            }
            tokio::time::sleep(Duration::from_millis($sleep_ms)).await;
        }

        Ok(())
    }};
}
pub use async_wait;
