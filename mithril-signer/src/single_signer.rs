use mithril_aggregator::entities::{ProtocolParameters, SingleSignature};
#[cfg(test)]
use mockall::automock;

#[cfg_attr(test, automock)]
pub trait SingleSigner {
    fn compute_single_signatures(
        &self,
        message: Vec<u8>,
        stake_distribution: Vec<String>,
        protocol_parameters: &ProtocolParameters,
    ) -> Result<Vec<SingleSignature>, String>;
}
