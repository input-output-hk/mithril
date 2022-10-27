use crate::crypto_helper::cardano::{
    KeyRegWrapper, ProtocolRegistrationErrorWrapper, StmInitializerWrapper,
};

use mithril::stm::{
    Index, Stake, StmAggrSig, StmAggrVerificationKey, StmClerk, StmParameters, StmSig, StmSigner,
    StmVerificationKeyPoP,
};
use mithril::AggregationError;

#[cfg(any(test, feature = "allow_skip_signer_certification"))]
use mithril::{key_reg::KeyReg, stm::StmInitializer};

use blake2::{digest::consts::U32, Blake2b};
use ed25519_dalek;
use kes_summed_ed25519::kes::Sum6KesSig;

/// A protocol version
pub type ProtocolVersion<'a> = &'a str;

// Protocol types alias
type D = Blake2b<U32>;

/// The id of a mithril party.
pub type ProtocolPartyId = String;

/// Alias of [MithrilCore:Stake](type@mithril::stm::Stake).
pub type ProtocolStake = Stake;

/// A list of [Party Id][ProtocolPartyId] associated with its [Stake][ProtocolStake].
pub type ProtocolStakeDistribution = Vec<(ProtocolPartyId, ProtocolStake)>;

/// Alias of [MithrilCore::StmParameters](struct@mithril::stm::StmParameters).
pub type ProtocolParameters = StmParameters;

/// Alias of [MithrilCore::Index](type@mithril::stm::Index).
pub type ProtocolLotteryIndex = Index;

/// Alias of [MithrilCore:StmSigner](struct@mithril::stm::StmSigner).
pub type ProtocolSigner = StmSigner<D>;

/// Alias of a wrapper of [MithrilCore:StmInitializer](struct@mithril::stm::StmInitializer).
pub type ProtocolInitializer = StmInitializerWrapper;

/// Alias of [MithrilCore:StmClerk](struct@mithril::stm::StmClerk).
pub type ProtocolClerk = StmClerk<D>;

/// Alias of a wrapper of [MithrilCore:KeyReg](struct@mithril::key_reg::KeyReg).
pub type ProtocolKeyRegistration = KeyRegWrapper;

/// Alias of [MithrilCore:StmSig](struct@mithril::stm::StmSig).
pub type ProtocolSingleSignature = StmSig<D>;

/// Alias of [MithrilCore:StmAggrSig](struct@mithril::stm::StmAggrSig).
pub type ProtocolMultiSignature = StmAggrSig<D>;

/// Alias of [MithrilCore:StmVerificationKeyPoP](type@mithril::stm::StmVerificationKeyPoP).
pub type ProtocolSignerVerificationKey = StmVerificationKeyPoP;

/// Alias of [KES:Sum6KesSig](https://github.com/input-output-hk/kes/blob/master/src/kes.rs).
pub type ProtocolSignerVerificationKeySignature = Sum6KesSig;

/// Alias of [MithrilCore:StmAggrVerificationKey](struct@mithril::stm::StmAggrVerificationKey).
pub type ProtocolAggregateVerificationKey = StmAggrVerificationKey<D>;

/// Alias of [Ed25519:PublicKey](https://docs.rs/ed25519-dalek/latest/ed25519_dalek/struct.PublicKey.html).
pub type ProtocolGenesisVerificationKey = ed25519_dalek::PublicKey;

/// Alias of [Ed25519:SecretKey](https://docs.rs/ed25519-dalek/latest/ed25519_dalek/struct.SecretKey.html).
pub type ProtocolGenesisSecretKey = ed25519_dalek::SecretKey;

/// Alias of [Ed25519:Signature](https://docs.rs/ed25519-dalek/latest/ed25519_dalek/struct.Signature.html).
pub type ProtocolGenesisSignature = ed25519_dalek::Signature;

// Error alias
/// Alias of a wrapper of [MithrilCore:RegisterError](enum@mithril::RegisterError).
pub type ProtocolRegistrationError = ProtocolRegistrationErrorWrapper;

/// Alias of [MithrilCore:AggregationError](enum@mithril::AggregationError).
pub type ProtocolAggregationError = AggregationError;

// Test only
/// (Test only) Alias of [MithrilCore:StmInitializer](struct@mithril::stm::StmInitializer).
#[cfg(any(test, feature = "allow_skip_signer_certification"))]
pub type ProtocolInitializerNotCertified = StmInitializer;

/// (Test only) Alias of [MithrilCore:KeyReg](struct@mithril::key_reg::KeyReg). (Test only)
#[cfg(any(test, feature = "allow_skip_signer_certification"))]
pub type ProtocolKeyRegistrationNotCertified = KeyReg;
