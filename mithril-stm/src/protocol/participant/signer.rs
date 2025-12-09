use blake2::digest::{Digest, FixedOutput};

use crate::{
    ClosedKeyRegistration, Parameters, SingleSignature, Stake, is_lottery_won,
    signature_scheme::{BlsSignature, BlsSigningKey, BlsVerificationKey},
};

/// Wrapper of the MultiSignature Verification key
pub type VerificationKey = BlsVerificationKey;

/// Participant in the protocol can sign messages.
/// * If the signer has `closed_reg`, then it can generate Stm certificate.
///     * This kind of signer can only be generated out of an `Initializer` and a `ClosedKeyRegistration`.
///     * This ensures that a `MerkleTree` root is not computed before all participants have registered.
/// * If the signer does not have `closed_reg`, then it is a core signer.
///     * This kind of signer cannot participate certificate generation.
///     * Signature generated can be verified by a full node verifier (core verifier).
#[derive(Debug, Clone)]
pub struct Signer<D: Digest> {
    signer_index: u64,
    stake: Stake,
    params: Parameters,
    sk: BlsSigningKey,
    vk: VerificationKey,
    closed_reg: Option<ClosedKeyRegistration<D>>,
}

impl<D: Clone + Digest + FixedOutput> Signer<D> {
    /// Create a Signer for given input
    pub(crate) fn set_signer(
        signer_index: u64,
        stake: Stake,
        params: Parameters,
        sk: BlsSigningKey,
        vk: VerificationKey,
        closed_reg: ClosedKeyRegistration<D>,
    ) -> Signer<D> {
        Self {
            signer_index,
            stake,
            params,
            sk,
            vk,
            closed_reg: Some(closed_reg),
        }
    }

    /// Create a Signer for given input
    #[deprecated(since = "0.5.0", note = "Use `set_signer` instead")]
    pub fn set_stm_signer(
        signer_index: u64,
        stake: Stake,
        params: Parameters,
        sk: BlsSigningKey,
        vk: VerificationKey,
        closed_reg: ClosedKeyRegistration<D>,
    ) -> Signer<D> {
        Self::set_signer(signer_index, stake, params, sk, vk, closed_reg)
    }

    /// This function produces a signature following the description of Section 2.4.
    /// Once the signature is produced, this function checks whether any index in `[0,..,self.params.m]`
    /// wins the lottery by evaluating the dense mapping.
    /// It records all the winning indexes in `Self.indexes`.
    /// If it wins at least one lottery, it stores the signer's merkle tree index. The proof of membership
    /// will be handled by the aggregator.
    pub fn sign(&self, msg: &[u8]) -> Option<SingleSignature> {
        let closed_reg = self.closed_reg.as_ref().expect("Closed registration not found! Cannot produce SingleSignatures. Use core_sign to produce core signatures (not valid for an StmCertificate).");
        let msgp = closed_reg
            .merkle_tree
            .to_merkle_tree_batch_commitment()
            .concatenate_with_message(msg);
        let sigma = self.sk.sign(&msgp);

        let indexes = self.check_lottery(&msgp, &sigma, closed_reg.total_stake);

        if !indexes.is_empty() {
            Some(SingleSignature {
                sigma,
                indexes,
                signer_index: self.signer_index,
            })
        } else {
            None
        }
    }

    /// Extract the verification key.
    pub fn get_verification_key(&self) -> VerificationKey {
        self.vk
    }

    /// Extract the verification key.
    #[deprecated(since = "0.5.0", note = "Use `get_verification_key` instead")]
    pub fn verification_key(&self) -> VerificationKey {
        Self::get_verification_key(self)
    }

    /// Extract stake from the signer.
    pub fn get_stake(&self) -> Stake {
        self.stake
    }

    /// Collects and returns the winning indices.
    pub fn check_lottery(&self, msg: &[u8], sigma: &BlsSignature, total_stake: Stake) -> Vec<u64> {
        let mut indexes = Vec::new();
        for index in 0..self.params.m {
            if is_lottery_won(
                self.params.phi_f,
                sigma.evaluate_dense_mapping(msg, index),
                self.stake,
                total_stake,
            ) {
                indexes.push(index);
            }
        }
        indexes
    }

    /// Get Parameters
    pub(crate) fn get_parameters(&self) -> Parameters {
        self.params
    }

    /// Get Parameters
    #[deprecated(since = "0.5.0", note = "Use `get_parameters` instead")]
    pub fn get_params(&self) -> Parameters {
        Self::get_parameters(self)
    }

    /// Get closed key registration
    pub(crate) fn get_closed_key_registration(&self) -> Option<ClosedKeyRegistration<D>> {
        self.closed_reg.clone()
    }

    /// Get closed key registration
    #[deprecated(since = "0.5.0", note = "Use `get_closed_key_registration` instead")]
    pub fn get_closed_reg(&self) -> Option<ClosedKeyRegistration<D>> {
        Self::get_closed_key_registration(self)
    }

    /// Create a basic signer (no registration data) for given input
    pub(crate) fn set_basic_signer(
        signer_index: u64,
        stake: Stake,
        params: Parameters,
        sk: BlsSigningKey,
        vk: VerificationKey,
    ) -> Signer<D> {
        Self {
            signer_index,
            stake,
            params,
            sk,
            vk,
            closed_reg: None,
        }
    }

    /// Create a core signer (no registration data) for given input
    #[deprecated(since = "0.5.0", note = "Use `set_basic_signer` instead")]
    pub fn set_core_signer(
        signer_index: u64,
        stake: Stake,
        params: Parameters,
        sk: BlsSigningKey,
        vk: VerificationKey,
    ) -> Signer<D> {
        Self::set_basic_signer(signer_index, stake, params, sk, vk)
    }

    /// A basic signature generated without closed key registration.
    /// The basic signature can be verified by basic verifier.
    /// Once the signature is produced, this function checks whether any index in `[0,..,self.params.m]`
    /// wins the lottery by evaluating the dense mapping.
    /// It records all the winning indexes in `Self.indexes`.
    pub fn basic_sign(&self, msg: &[u8], total_stake: Stake) -> Option<SingleSignature> {
        let sigma = self.sk.sign(msg);

        let indexes = self.check_lottery(msg, &sigma, total_stake);
        if !indexes.is_empty() {
            Some(SingleSignature {
                sigma,
                indexes,
                signer_index: self.signer_index,
            })
        } else {
            None
        }
    }

    /// A basic signature generated without closed key registration.
    /// The basic signature can be verified by basic verifier.
    /// Once the signature is produced, this function checks whether any index in `[0,..,self.params.m]`
    /// wins the lottery by evaluating the dense mapping.
    /// It records all the winning indexes in `Self.indexes`.
    #[deprecated(since = "0.5.0", note = "Use `basic_sign` instead")]
    pub fn core_sign(&self, msg: &[u8], total_stake: Stake) -> Option<SingleSignature> {
        Self::basic_sign(self, msg, total_stake)
    }
}
