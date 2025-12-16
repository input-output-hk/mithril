mod merkle_tree;

pub use merkle_tree::*;

use blake2::{Blake2b, digest::consts::U32};
use digest::{Digest, FixedOutput};
use std::fmt::Debug;

pub trait MembershipDigest {
    type ConcatenationHash: Digest + FixedOutput + Clone + Debug + Send + Sync;
    #[cfg(feature = "future_snark")]
    type SnarkHash: Digest + FixedOutput + Clone;
}

/// Only for tests
#[derive(Clone, Debug, Default)]
pub struct CustomMembershipDigest {}

impl MembershipDigest for CustomMembershipDigest {
    type ConcatenationHash = Blake2b<U32>;
    #[cfg(feature = "future_snark")]
    type SnarkHash = Blake2b<U32>;
}
