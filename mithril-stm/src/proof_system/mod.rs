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

pub use concatenation::{
    AggregateVerificationKeyForConcatenation, ConcatenationClerk, ConcatenationProof,
};
pub(crate) use concatenation::{ConcatenationProofSigner, SingleSignatureForConcatenation};
