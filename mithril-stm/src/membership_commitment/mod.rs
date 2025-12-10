mod merkle_tree;

pub use merkle_tree::*;

use blake2::{Blake2b, digest::consts::U32};
use digest::{Digest, FixedOutput};
use std::fmt::Debug;

pub trait MembershipDigest: Send + Sync {
    type ConcatenationHash: Digest + FixedOutput + Clone + Debug;
    #[cfg(feature = "future_snark")]
    type SnarkHash: Digest + FixedOutput + Clone;
}

/// Only for tests
#[derive(Clone, Debug)]
pub struct CustomMembershipDigest {}

impl MembershipDigest for CustomMembershipDigest {
    type ConcatenationHash = Blake2b<U32>;
    #[cfg(feature = "future_snark")]
    type SnarkHash = Blake2b<U32>;
}
