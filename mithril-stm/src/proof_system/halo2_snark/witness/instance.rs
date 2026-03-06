use crate::BaseFieldElement;

// TODO: remove this allow dead_code directive when function is called or future_snark is activated
#[allow(dead_code)]
pub(crate) struct Instance {
    merkle_tree_root: BaseFieldElement,
    message: BaseFieldElement,
}
// TODO: remove this allow dead_code directive when function is called or future_snark is activated
#[allow(dead_code)]
impl Instance {
    pub(crate) fn new(merkle_tree_root: BaseFieldElement, message: BaseFieldElement) -> Self {
        Self {
            merkle_tree_root,
            message,
        }
    }
}
