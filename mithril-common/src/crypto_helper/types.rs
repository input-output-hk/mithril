use crate::crypto_helper::cardano::{
    KeyRegWrapper, ProtocolRegistrationErrorWrapper, StmInitializerWrapper,
};

use mithril_stm::stm::{
    Index, Stake, StmAggrSig, StmAggrVerificationKey, StmClerk, StmParameters, StmSig, StmSigner,
    StmVerificationKeyPoP,
};
use mithril_stm::AggregationError;

use blake2::{digest::consts::U32, Blake2b};
use ed25519_dalek;
use kes_summed_ed25519::kes::Sum6KesSig;

/// A protocol version
pub type ProtocolVersion<'a> = &'a str;

// Protocol types alias
type D = Blake2b<U32>;

/// The id of a mithril party.
pub type ProtocolPartyId = String;

/// Alias of [MithrilStm:Stake](type@mithril_stm::stm::Stake).
pub type ProtocolStake = Stake;

/// A list of [Party Id][ProtocolPartyId] associated with its [Stake][ProtocolStake].
pub type ProtocolStakeDistribution = Vec<(ProtocolPartyId, ProtocolStake)>;

/// Alias of [MithrilStm::StmParameters](struct@mithril_stm::stm::StmParameters).
pub type ProtocolParameters = StmParameters;

/// Alias of [MithrilStm::Index](type@mithril_stm::stm::Index).
pub type ProtocolLotteryIndex = Index;

/// Alias of [MithrilStm:StmSigner](struct@mithril_stm::stm::StmSigner).
pub type ProtocolSigner = StmSigner<D>;

/// Alias of a wrapper of [MithrilStm:StmInitializer](struct@mithril_stm::stm::StmInitializer).
pub type ProtocolInitializer = StmInitializerWrapper;

/// Alias of [MithrilStm:StmClerk](struct@mithril_stm::stm::StmClerk).
pub type ProtocolClerk = StmClerk<D>;

/// Alias of a wrapper of [MithrilStm:KeyReg](struct@mithril_stm::key_reg::KeyReg).
pub type ProtocolKeyRegistration = KeyRegWrapper;

/// Alias of [MithrilStm:StmSig](struct@mithril_stm::stm::StmSig).
pub type ProtocolSingleSignature = StmSig;

/// Alias of [MithrilStm:StmAggrSig](struct@mithril_stm::stm::StmAggrSig).
pub type ProtocolMultiSignature = StmAggrSig<D>;

/// Alias of [MithrilStm:StmVerificationKeyPoP](type@mithril_stm::stm::StmVerificationKeyPoP).
pub type ProtocolSignerVerificationKey = StmVerificationKeyPoP;

/// Alias of [KES:Sum6KesSig](https://github.com/input-output-hk/kes/blob/master/src/kes.rs).
pub type ProtocolSignerVerificationKeySignature = Sum6KesSig;

/// Alias of [MithrilStm:StmAggrVerificationKey](struct@mithril_stm::stm::StmAggrVerificationKey).
pub type ProtocolAggregateVerificationKey = StmAggrVerificationKey<D>;

/// Alias of [Ed25519:PublicKey](https://docs.rs/ed25519-dalek/latest/ed25519_dalek/struct.PublicKey.html).
pub type ProtocolGenesisVerificationKey = ed25519_dalek::PublicKey;

/// Alias of [Ed25519:SecretKey](https://docs.rs/ed25519-dalek/latest/ed25519_dalek/struct.SecretKey.html).
pub type ProtocolGenesisSecretKey = ed25519_dalek::SecretKey;

/// Alias of [Ed25519:Signature](https://docs.rs/ed25519-dalek/latest/ed25519_dalek/struct.Signature.html).
pub type ProtocolGenesisSignature = ed25519_dalek::Signature;

// Error alias
/// Alias of a wrapper of [MithrilStm:RegisterError](enum@mithril_stm::RegisterError).
pub type ProtocolRegistrationError = ProtocolRegistrationErrorWrapper;

/// Alias of [MithrilStm:AggregationError](enum@mithril_stm::AggregationError).
pub type ProtocolAggregationError = AggregationError;
