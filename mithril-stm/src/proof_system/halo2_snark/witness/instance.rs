use crate::BaseFieldElement;

/// Public inputs to the SNARK circuit.
///
/// Contains the Merkle tree root of the registration commitment and the
/// signed message, both represented as base field elements.
// TODO: remove this allow dead_code directive when function is called or future_snark is activated
#[allow(dead_code)]
pub(crate) struct Instance {
    /// The root of the SNARK registration Merkle tree.
    merkle_tree_root: BaseFieldElement,
    /// The signed message as a base field element.
    message: BaseFieldElement,
}

// TODO: remove this allow dead_code directive when function is called or future_snark is activated
#[allow(dead_code)]
impl Instance {
    /// Create a new `Instance` from the Merkle tree root and message.
    pub(crate) fn new(merkle_tree_root: BaseFieldElement, message: BaseFieldElement) -> Self {
        Self {
            merkle_tree_root,
            message,
        }
    }
}
