use crate::Stake;

/// The type used for eligibility evaluation
pub struct EligibilityValue {}

impl EligibilityValue {
    /// Computes the eligibility value from the given stake
    pub fn compute_eligibility_value(stake: Stake) -> Self {
        Self {}
    }
}
