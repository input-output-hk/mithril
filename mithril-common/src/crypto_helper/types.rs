use crate::crypto_helper::cardano::{
    KeyRegWrapper, ProtocolRegistrationErrorWrapper, StmClerkWrapper, StmInitializerWrapper,
    StmSignerWrapper,
};

use mithril::stm::{
    Index, Stake, StmAggrSig, StmAggrVerificationKey, StmParameters, StmSig, StmVerificationKeyPoP,
};
use mithril::AggregationError;

#[cfg(any(test, feature = "skip_signer_certification"))]
use mithril::key_reg::KeyReg;
#[cfg(any(test, feature = "skip_signer_certification"))]
use mithril::stm::{StmClerk, StmInitializer, StmSigner};

use blake2::{digest::consts::U32, Blake2b};
use ed25519_dalek;
use kes_summed_ed25519::kes::Sum6KesSig;

/// A protocol version
pub type ProtocolVersion<'a> = &'a str;

// Protocol types alias
type D = Blake2b<U32>;

/// The id of a mithril party.
pub type ProtocolPartyId = String;

/// Alias of [MithrilCore:Stake](https://mithril.network/mithril-core/doc/mithril/stm/type.Stake.html).
pub type ProtocolStake = Stake;

/// A list of [Party Id][ProtocolPartyId] associated with its [Stake][ProtocolStake].
pub type ProtocolStakeDistribution = Vec<(ProtocolPartyId, ProtocolStake)>; // todo: should eventually be Vec<(PoolId, ProtocolStake)>

/// Alias of [MithrilCore::StmParameters](https://mithril.network/mithril-core/doc/mithril/stm/struct.StmParameters.html).
pub type ProtocolParameters = StmParameters;

/// Alias of [MithrilCore::Index](https://mithril.network/mithril-core/doc/mithril/stm/type.Index.html).
pub type ProtocolLotteryIndex = Index;

/// Alias of a wrapper of [MithrilCore:StmSigner](https://mithril.network/mithril-core/doc/mithril/stm/struct.StmSigner.html).
pub type ProtocolSigner = StmSignerWrapper;

/// Alias of a wrapper of [MithrilCore:StmInitializer](https://mithril.network/mithril-core/doc/mithril/stm/struct.StmInitializer.html).
pub type ProtocolInitializer = StmInitializerWrapper;

/// Alias of a wrapper of [MithrilCore:StmClerk](https://mithril.network/mithril-core/doc/mithril/stm/struct.StmClerk.html).
pub type ProtocolClerk = StmClerkWrapper;

/// Alias of a wrapper of [MithrilCore:KeyReg](https://mithril.network/mithril-core/doc/mithril/key_reg/struct.KeyReg.html).
pub type ProtocolKeyRegistration = KeyRegWrapper;

/// Alias of [MithrilCore:StmSig](https://mithril.network/mithril-core/doc/mithril/stm/struct.StmSig.html).
pub type ProtocolSingleSignature = StmSig<D>;

/// Alias of [MithrilCore:StmAggrSig](https://mithril.network/mithril-core/doc/mithril/stm/struct.StmAggrSig.html).
pub type ProtocolMultiSignature = StmAggrSig<D>;

/// Alias of [MithrilCore:StmVerificationKeyPoP](https://mithril.network/mithril-core/doc/mithril/stm/type.StmVerificationKeyPoP.html).
pub type ProtocolSignerVerificationKey = StmVerificationKeyPoP;

/// Alias of [KES:Sum6KesSig](https://github.com/input-output-hk/kes/blob/master/src/kes.rs).
pub type ProtocolSignerVerificationKeySignature = Sum6KesSig;

/// Alias of [MithrilCore:StmAggrVerificationKey](https://mithril.network/mithril-core/doc/mithril/stm/struct.StmAggrVerificationKey.html).
pub type ProtocolAggregateVerificationKey = StmAggrVerificationKey<D>;

/// Alias of [Ed25519:PublicKey](https://docs.rs/ed25519-dalek/latest/ed25519_dalek/struct.PublicKey.html).
pub type ProtocolGenesisVerificationKey = ed25519_dalek::PublicKey;

/// Alias of [Ed25519:SecretKey](https://docs.rs/ed25519-dalek/latest/ed25519_dalek/struct.SecretKey.html).
pub type ProtocolGenesisSecretKey = ed25519_dalek::SecretKey;

/// Alias of [Ed25519:Signature](https://docs.rs/ed25519-dalek/latest/ed25519_dalek/struct.Signature.html).
pub type ProtocolGenesisSignature = ed25519_dalek::Signature;

// Error alias
/// Alias of [MithrilCore:RegisterError](https://mithril.network/mithril-core/doc/mithril/error/enum.RegisterError.html).
pub type ProtocolRegistrationError = ProtocolRegistrationErrorWrapper;

/// Alias of [MithrilCore:AggregationError](https://mithril.network/mithril-core/doc/mithril/error/enum.AggregationError.html).
pub type ProtocolAggregationError = AggregationError;

// Test only
/// (Test only) Alias of [MithrilCore:StmSigner](https://mithril.network/mithril-core/doc/mithril/stm/struct.StmSigner.html).
#[cfg(any(test, feature = "skip_signer_certification"))]
pub type ProtocolSignerNotCertified = StmSigner<D>;

/// (Test only) Alias of [MithrilCore:StmInitializer](https://mithril.network/mithril-core/doc/mithril/stm/struct.StmInitializer.html).
#[cfg(any(test, feature = "skip_signer_certification"))]
pub type ProtocolInitializerNotCertified = StmInitializer;

/// (Test only) Alias of [MithrilCore:StmClerk](https://mithril.network/mithril-core/doc/mithril/stm/struct.StmClerk.html).
#[cfg(any(test, feature = "skip_signer_certification"))]
pub type ProtocolClerkNotCertified = StmClerk<D>;

/// (Test only) Alias of [MithrilCore:KeyReg](https://mithril.network/mithril-core/doc/mithril/key_reg/struct.KeyReg.html). (Test only)
#[cfg(any(test, feature = "skip_signer_certification"))]
pub type ProtocolKeyRegistrationNotCertified = KeyReg;
