// @todo: remove this
#![allow(dead_code)]
use mithril_aggregator::entities;

use ark_bls12_377::Bls12_377;
use mithril::key_reg::KeyReg;
use mithril::merkle_tree::MTHashLeaf;
use mithril::mithril_proof::concat_proofs::{ConcatProof, TrivialEnv};
use mithril::msp::{MspPk, MspSk};
use mithril::stm::{
    Index, MTValue, PartyId, Stake, StmClerk, StmInitializer, StmMultiSig, StmParameters, StmSig,
    StmSigner,
};
use thiserror::Error;

pub type Bytes = Vec<u8>;

// Protocol types alias
type H = blake2::Blake2b;
type F = <H as MTHashLeaf<MTValue<Bls12_377>>>::F;
pub type ProtocolPartyId = PartyId;
pub type ProtocolStake = Stake;
pub type ProtocolStakeDistribution = Vec<(ProtocolPartyId, ProtocolStake)>;
pub type ProtocolParameters = StmParameters;
pub type ProtocolLotteryIndex = Index;
pub type ProtocolSigner = StmSigner<H, Bls12_377>;
pub type ProtocolInitializer = StmInitializer<Bls12_377>;
pub type ProtocolClerk = StmClerk<H, Bls12_377, TrivialEnv>;
pub type ProtocolKeyRegistration = KeyReg<Bls12_377>;
pub type ProtocolProof = ConcatProof<Bls12_377, H, F>;
pub type ProtocolSingleSignature = StmSig<Bls12_377, F>;
pub type ProtocolMultiSignature = StmMultiSig<Bls12_377, ProtocolProof>;
pub type ProtocolSignerVerificationKey = MspPk<Bls12_377>;
pub type ProtocolSignerSecretKey = MspSk<Bls12_377>;

use mithril_aggregator::entities::{SignerWithStake, SingleSignature};
#[cfg(test)]
use mockall::automock;

#[cfg_attr(test, automock)]
pub trait SingleSigner {
    fn compute_single_signatures(
        &self,
        message: Bytes,
        stake_distribution: Vec<entities::SignerWithStake>,
        protocol_parameters: &entities::ProtocolParameters,
    ) -> Result<Vec<entities::SingleSignature>, SingleSignerError>;
}

#[derive(Error, Debug, PartialEq)]
pub enum SingleSignerError {
    #[error("the signer is not registered in the stake distribution")]
    UnregisteredVerificationKey(),
}

pub struct MithrilSingleSigner {}

impl MithrilSingleSigner {
    pub fn new() -> Self {
        Self {}
    }
}

impl SingleSigner for MithrilSingleSigner {
    fn compute_single_signatures(
        &self,
        _message: Bytes,
        _stake_distribution: Vec<SignerWithStake>,
        _protocol_parameters: &entities::ProtocolParameters,
    ) -> Result<Vec<SingleSignature>, SingleSignerError> {
        Err(SingleSignerError::UnregisteredVerificationKey())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use mithril_aggregator::fake_data;

    #[test]
    fn cant_compute_if_signer_verification_key_is_not_registered() {
        let single_signer = MithrilSingleSigner::new();
        let stake_distribution = fake_data::signers_with_stakes(5);
        let protocol_parameters = fake_data::protocol_parameters();

        let sign_result = single_signer.compute_single_signatures(
            "message".as_bytes().to_vec(),
            stake_distribution,
            &protocol_parameters,
        );

        assert_eq!(
            SingleSignerError::UnregisteredVerificationKey(),
            sign_result.unwrap_err()
        )
    }
}
