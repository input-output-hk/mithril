//! `EpochData`: per-call external input carrying the certificate's epoch identifier
//! and the upcoming epoch's announcements.

use crate::circuits::{halo2::types::CircuitBase, halo2_ivc::PREIMAGE_SIZE};

/// Per-call external input carrying the certificate's epoch identifier and the
/// upcoming epoch's announcements.
// TODO: remove this allow dead_code directive when the IVC prover consumes this epoch data
#[allow(dead_code)]
pub(crate) struct EpochData {
    /// Raw protocol-message preimage.
    pub(crate) message_preimage: [u8; PREIMAGE_SIZE],
    /// Certificate's epoch, decoded from `PREIMAGE_CURRENT_EPOCH_BYTES`.
    pub(crate) current_epoch: CircuitBase,
    /// Next epoch's Merkle-tree commitment, decoded from `PREIMAGE_NEXT_MERKLE_ROOT_BYTES`.
    pub(crate) next_merkle_root: CircuitBase,
    /// Next epoch's protocol parameters, decoded from `PREIMAGE_NEXT_PROTOCOL_PARAMS_BYTES`.
    pub(crate) next_protocol_parameters: CircuitBase,
}
