//! `DecodedProtocolMessage`: the certificate's protocol-message preimage together with
//! the three fields decoded from its fixed byte ranges. Consumed as a per-call external
//! input by the IVC prover input preparation step.

use crate::{
    BaseFieldElement, StmResult,
    circuits::halo2_ivc::{
        PREIMAGE_CURRENT_EPOCH_BYTES, PREIMAGE_NEXT_MERKLE_TREE_COMMITMENT_BYTES,
        PREIMAGE_NEXT_PROTOCOL_PARAMETERS_BYTES, PREIMAGE_SIZE,
    },
};

/// The certificate's protocol-message preimage together with the three fields decoded
/// from its fixed byte ranges. Consumed as a per-call external input by the IVC prover
/// input preparation step.
#[derive(Clone)]
pub(crate) struct DecodedProtocolMessage {
    /// Raw protocol-message preimage.
    message_preimage: [u8; PREIMAGE_SIZE],
    /// Certificate's epoch, decoded from `PREIMAGE_CURRENT_EPOCH_BYTES`.
    current_epoch: BaseFieldElement,
    /// Next epoch's Merkle-tree commitment, decoded from `PREIMAGE_NEXT_MERKLE_TREE_COMMITMENT_BYTES`.
    next_merkle_tree_commitment: BaseFieldElement,
    /// Next epoch's protocol parameters, decoded from `PREIMAGE_NEXT_PROTOCOL_PARAMETERS_BYTES`.
    next_protocol_parameters: BaseFieldElement,
}

#[allow(dead_code)]
impl DecodedProtocolMessage {
    /// Builds a `DecodedProtocolMessage` by decoding the three announced fields from the
    /// protocol-message preimage. The decoding mirrors the circuit's `combine_bytes`
    /// step: bytes are interpreted little-endian against powers-of-256, reducing modulo
    /// the base field for the 32-byte ranges.
    // TODO: Replace with the protocol message functions introduced in PR 3288, when merged.
    pub(crate) fn new(message_preimage: [u8; PREIMAGE_SIZE]) -> StmResult<Self> {
        let current_epoch_bytes: [u8; 8] =
            message_preimage[PREIMAGE_CURRENT_EPOCH_BYTES].try_into()?;
        let current_epoch = BaseFieldElement::from(u64::from_le_bytes(current_epoch_bytes));

        let next_merkle_tree_commitment_bytes: [u8; 32] =
            message_preimage[PREIMAGE_NEXT_MERKLE_TREE_COMMITMENT_BYTES].try_into()?;
        let next_merkle_tree_commitment =
            BaseFieldElement::from_raw(&next_merkle_tree_commitment_bytes)?;

        let next_protocol_parameters_bytes: [u8; 32] =
            message_preimage[PREIMAGE_NEXT_PROTOCOL_PARAMETERS_BYTES].try_into()?;
        let next_protocol_parameters = BaseFieldElement::from_raw(&next_protocol_parameters_bytes)?;

        Ok(Self {
            message_preimage,
            current_epoch,
            next_merkle_tree_commitment,
            next_protocol_parameters,
        })
    }

    /// Returns the raw protocol-message preimage.
    pub(crate) fn message_preimage(&self) -> &[u8; PREIMAGE_SIZE] {
        &self.message_preimage
    }

    /// Returns the certificate's epoch.
    pub(crate) fn current_epoch(&self) -> BaseFieldElement {
        self.current_epoch
    }

    /// Returns the next epoch's Merkle-tree commitment.
    pub(crate) fn next_merkle_tree_commitment(&self) -> BaseFieldElement {
        self.next_merkle_tree_commitment
    }

    /// Returns the next epoch's protocol parameters.
    pub(crate) fn next_protocol_parameters(&self) -> BaseFieldElement {
        self.next_protocol_parameters
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn new_decodes_each_field_from_correct_range() {
        let mut preimage = [0u8; PREIMAGE_SIZE];
        preimage[PREIMAGE_NEXT_MERKLE_TREE_COMMITMENT_BYTES].copy_from_slice(&[0x11; 32]);
        preimage[PREIMAGE_NEXT_PROTOCOL_PARAMETERS_BYTES].copy_from_slice(&[0x22; 32]);
        preimage[PREIMAGE_CURRENT_EPOCH_BYTES].copy_from_slice(&42u64.to_le_bytes());

        let decoded_protocol_message = DecodedProtocolMessage::new(preimage).unwrap();

        assert_eq!(
            decoded_protocol_message.current_epoch(),
            BaseFieldElement::from(42u64)
        );
        assert_eq!(
            decoded_protocol_message.next_merkle_tree_commitment(),
            BaseFieldElement::from_raw(&[0x11; 32]).unwrap()
        );
        assert_eq!(
            decoded_protocol_message.next_protocol_parameters(),
            BaseFieldElement::from_raw(&[0x22; 32]).unwrap()
        );
        assert_eq!(decoded_protocol_message.message_preimage(), &preimage);
    }

    #[test]
    fn new_decodes_genesis_zero_preimage_to_zero_fields() {
        let preimage = [0u8; PREIMAGE_SIZE];
        let decoded_protocol_message = DecodedProtocolMessage::new(preimage).unwrap();

        let zero = BaseFieldElement::from(0u64);
        assert_eq!(decoded_protocol_message.current_epoch(), zero);
        assert_eq!(decoded_protocol_message.next_merkle_tree_commitment(), zero);
        assert_eq!(decoded_protocol_message.next_protocol_parameters(), zero);
    }

    #[test]
    fn new_reduces_max_value_modulo_field() {
        let mut preimage = [0u8; PREIMAGE_SIZE];
        preimage[PREIMAGE_NEXT_MERKLE_TREE_COMMITMENT_BYTES].copy_from_slice(&[0xFF; 32]);

        let decoded_protocol_message = DecodedProtocolMessage::new(preimage).unwrap();

        assert_eq!(
            decoded_protocol_message.next_merkle_tree_commitment(),
            BaseFieldElement::from_raw(&[0xFF; 32]).unwrap()
        );
    }
}
