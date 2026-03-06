use crate::BaseFieldElement;

#[allow(dead_code)]
pub(crate) struct Instance {
    merkle_tree_root: BaseFieldElement,
    message: BaseFieldElement,
}
#[allow(dead_code)]
impl Instance {
    pub(crate) fn new(merkle_tree_root: BaseFieldElement, message: BaseFieldElement) -> Self {
        Self {
            merkle_tree_root,
            message,
        }
    }
}
