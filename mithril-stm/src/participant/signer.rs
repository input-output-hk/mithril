use crate::bls_multi_signature::{Signature, SigningKey, VerificationKey};
use crate::eligibility_check::ev_lt_phi;
use crate::key_reg::ClosedKeyReg;
use crate::stm::{Stake, StmParameters, StmSig};
use blake2::digest::{Digest, FixedOutput};

/// Wrapper of the MultiSignature Verification key
pub type StmVerificationKey = VerificationKey;

/// Participant in the protocol can sign messages.
/// * If the signer has `closed_reg`, then it can generate Stm certificate.
///     * This kind of signer can only be generated out of an `StmInitializer` and a `ClosedKeyReg`.
///     * This ensures that a `MerkleTree` root is not computed before all participants have registered.
/// * If the signer does not have `closed_reg`, then it is a core signer.
///     * This kind of signer cannot participate certificate generation.
///     * Signature generated can be verified by a full node verifier (core verifier).
#[derive(Debug, Clone)]
pub struct StmSigner<D: Digest> {
    signer_index: u64,
    stake: Stake,
    params: StmParameters,
    sk: SigningKey,
    vk: StmVerificationKey,
    closed_reg: Option<ClosedKeyReg<D>>,
}

impl<D: Clone + Digest + FixedOutput> StmSigner<D> {
    /// Create an StmSigner for given input
    pub fn set_stm_signer(
        signer_index: u64,
        stake: Stake,
        params: StmParameters,
        sk: SigningKey,
        vk: StmVerificationKey,
        closed_reg: ClosedKeyReg<D>,
    ) -> StmSigner<D> {
        Self {
            signer_index,
            stake,
            params,
            sk,
            vk,
            closed_reg: Some(closed_reg),
        }
    }

    /// Create a core signer (no registration data) for given input
    pub fn set_core_signer(
        signer_index: u64,
        stake: Stake,
        params: StmParameters,
        sk: SigningKey,
        vk: StmVerificationKey,
    ) -> StmSigner<D> {
        Self {
            signer_index,
            stake,
            params,
            sk,
            vk,
            closed_reg: None,
        }
    }

    /// This function produces a signature following the description of Section 2.4.
    /// Once the signature is produced, this function checks whether any index in `[0,..,self.params.m]`
    /// wins the lottery by evaluating the dense mapping.
    /// It records all the winning indexes in `Self.indexes`.
    /// If it wins at least one lottery, it stores the signer's merkle tree index. The proof of membership
    /// will be handled by the aggregator.
    pub fn sign(&self, msg: &[u8]) -> Option<StmSig> {
        let closed_reg = self.closed_reg.as_ref().expect("Closed registration not found! Cannot produce StmSignatures. Use core_sign to produce core signatures (not valid for an StmCertificate).");
        let msgp = closed_reg
            .merkle_tree
            .to_commitment_batch_compat()
            .concat_with_msg(msg);
        let signature = self.core_sign(&msgp, closed_reg.total_stake)?;

        Some(StmSig {
            sigma: signature.sigma,
            signer_index: self.signer_index,
            indexes: signature.indexes,
        })
    }

    /// Extract the verification key.
    pub fn verification_key(&self) -> StmVerificationKey {
        self.vk
    }

    /// Extract stake from the signer.
    pub fn get_stake(&self) -> Stake {
        self.stake
    }

    /// A core signature generated without closed registration.
    /// The core signature can be verified by core verifier.
    /// Once the signature is produced, this function checks whether any index in `[0,..,self.params.m]`
    /// wins the lottery by evaluating the dense mapping.
    /// It records all the winning indexes in `Self.indexes`.
    pub fn core_sign(&self, msg: &[u8], total_stake: Stake) -> Option<StmSig> {
        let sigma = self.sk.sign(msg);

        let indexes = self.check_lottery(msg, &sigma, total_stake);
        if !indexes.is_empty() {
            Some(StmSig {
                sigma,
                indexes,
                signer_index: self.signer_index,
            })
        } else {
            None
        }
    }

    /// Collects and returns the winning indices.
    pub fn check_lottery(&self, msg: &[u8], sigma: &Signature, total_stake: Stake) -> Vec<u64> {
        let mut indexes = Vec::new();
        for index in 0..self.params.m {
            if ev_lt_phi(
                self.params.phi_f,
                sigma.eval(msg, index),
                self.stake,
                total_stake,
            ) {
                indexes.push(index);
            }
        }
        indexes
    }

    /// Get StmParameters
    pub fn get_params(&self) -> StmParameters {
        self.params
    }

    /// Get closed key registration
    pub fn get_closed_reg(&self) -> Option<ClosedKeyReg<D>> {
        self.closed_reg.clone()
    }

    /// Get verification key
    pub fn get_vk(&self) -> StmVerificationKey {
        self.vk
    }
}
