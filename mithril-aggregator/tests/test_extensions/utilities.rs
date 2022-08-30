use mithril_common::crypto_helper::{
    ProtocolInitializer, ProtocolPartyId, ProtocolSigner, ProtocolSignerVerificationKey,
    ProtocolStake,
};
use slog_scope::debug;
use std::sync::atomic::{AtomicUsize, Ordering};

pub type TestSigner = (
    ProtocolPartyId,
    ProtocolStake,
    ProtocolSignerVerificationKey,
    ProtocolSigner,
    ProtocolInitializer,
);

pub static COMMENT_COUNT: AtomicUsize = AtomicUsize::new(0);

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
