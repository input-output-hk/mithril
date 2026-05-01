use super::{ScalarFieldElement, BaseFieldElement};

pub struct StandardSchnorrSignature {
    pub(crate) response: ScalarFieldElement,
    pub(crate) challenge: BaseFieldElement,
}