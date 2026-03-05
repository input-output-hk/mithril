use anyhow::Context;

use crate::{StmResult, signature_scheme::BaseFieldElement};

/// Build the SNARK message from a Merkle tree root and a raw message.
///
/// The root is converted via `from_bytes`, which requires a canonical field element
/// (rejects values >= p). The message is converted via `from_raw`, which interprets
/// the bytes as a little-endian integer and applies modular reduction.
///
/// # Error
/// Returns an error if the root or the message is not exactly 32 bytes, or if the root
/// is not a canonical field element.
pub(crate) fn build_snark_message(
    merkle_root: &[u8],
    message: &[u8],
) -> StmResult<[BaseFieldElement; 2]> {
    let root_bytes: [u8; 32] = merkle_root
        .try_into()
        .with_context(|| "Merkle tree root must be exactly 32 bytes.")?;
    let root_as_base_field_element = BaseFieldElement::from_bytes(&root_bytes)
        .with_context(|| "Failed to convert Merkle tree root to BaseFieldElement.")?;

    let msg_bytes: [u8; 32] = message
        .try_into()
        .with_context(|| "Message must be exactly 32 bytes.")?;
    let message_as_base_field_element = BaseFieldElement::from_raw(&msg_bytes)
        .with_context(|| "Failed to convert message to BaseFieldElement.")?;

    Ok([root_as_base_field_element, message_as_base_field_element])
}
