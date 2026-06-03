macro_rules! cfg_num_integer {
    ($($item:item)*) => {
        $(
            #[cfg(any(feature = "num-integer-backend", target_family = "wasm", target_env = "musl", windows))]
            $item
        )*
    };
}

macro_rules! cfg_rug {
    ($($item:item)*) => {
        $(
            #[cfg(not(any(feature = "num-integer-backend", target_family = "wasm", target_env = "musl", windows)))]
            $item
        )*
    };
}

mod concatenation;
#[cfg(feature = "future_snark")]
mod halo2_snark;

#[cfg(feature = "future_snark")]
mod ivc_halo2_snark;

pub use concatenation::{
    AggregateVerificationKeyForConcatenation, ConcatenationClerk, ConcatenationProof,
};
pub(crate) use concatenation::{ConcatenationProofSigner, SingleSignatureForConcatenation};

#[cfg(feature = "future_snark")]
pub use halo2_snark::AggregateVerificationKeyForSnark;

#[cfg(feature = "future_snark")]
pub use halo2_snark::{MERKLE_TREE_DEPTH_FOR_SNARK, SnarkProof};

#[cfg(all(test, feature = "future_snark"))]
pub(crate) use halo2_snark::SnarkSetup;
#[cfg(feature = "future_snark")]
pub(crate) use halo2_snark::{
    SingleSignatureForSnark, SnarkClerk, SnarkProofSigner, SnarkProver, SnarkVerifierSetup,
    compute_target_value_for_snark_lottery,
};

#[cfg(all(test, feature = "future_snark"))]
pub(crate) use halo2_snark::RIGID_SLOT_BYTES as SNARK_AGGREGATE_VERIFICATION_KEY_RIGID_SLOT_BYTES;
