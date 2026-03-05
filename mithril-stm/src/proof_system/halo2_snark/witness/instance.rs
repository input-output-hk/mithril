use crate::BaseFieldElement;

pub(crate) struct Instance {
    merkle_tree_root: BaseFieldElement,
    message: BaseFieldElement,
}

impl Instance {
    pub(crate) fn new(merkle_tree_root: BaseFieldElement, message: BaseFieldElement) -> Self {
        Self {
            merkle_tree_root,
            message,
        }
    }
}
