use super::*;

#[derive(PartialEq, Eq, Clone)]
pub struct SignerRegistration {
    pub stake: Stake,
    pub bls_public_key: BlsVerificationKeyProofOfPossession,
    #[cfg(feature = "future_snark")]
    pub schnorr_public_key: Option<SchnorrVerificationKeyProofOfPossession>,
}
