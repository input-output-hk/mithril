use blake2::{digest::consts::U32, Blake2b};
use ed25519_dalek;
use mithril::key_reg::KeyReg;
use mithril::stm::{
    Index, Stake, StmAggrSig, StmAggrVerificationKey, StmClerk, StmInitializer, StmParameters,
    StmSig, StmSigner, StmVerificationKeyPoP,
};
/// A protocol version
pub type ProtocolVersion<'a> = &'a str;

// Protocol types alias
type D = Blake2b<U32>;
/// The id of a mithril party.
pub type ProtocolPartyId = String;
/// Alias of [MithrilCore:Stake](https://mithril.network/mithril-core/doc/mithril/stm/type.Stake.html).
pub type ProtocolStake = Stake;
/// A list of [Party Id][ProtocolPartyId] associated with its [Stake][ProtocolStake].
pub type ProtocolStakeDistribution = Vec<(ProtocolPartyId, ProtocolStake)>;
/// Alias of [MithrilCore::StmParameters](https://mithril.network/mithril-core/doc/mithril/stm/struct.StmParameters.html).
pub type ProtocolParameters = StmParameters;
/// Alias of [MithrilCore::Index](https://mithril.network/mithril-core/doc/mithril/stm/type.Index.html).
pub type ProtocolLotteryIndex = Index;
/// Alias of [MithrilCore:StmSigner](https://mithril.network/mithril-core/doc/mithril/stm/struct.StmSigner.html).
pub type ProtocolSigner = StmSigner<D>;
/// Alias of [MithrilCore:StmInitializer](https://mithril.network/mithril-core/doc/mithril/stm/struct.StmInitializer.html).
pub type ProtocolInitializer = StmInitializer;
/// Alias of [MithrilCore:StmClerk](https://mithril.network/mithril-core/doc/mithril/stm/struct.StmClerk.html).
pub type ProtocolClerk = StmClerk<D>;
/// Alias of [MithrilCore:KeyReg](https://mithril.network/mithril-core/doc/mithril/key_reg/struct.KeyReg.html).
pub type ProtocolKeyRegistration = KeyReg;
/// Alias of [MithrilCore:StmSig](https://mithril.network/mithril-core/doc/mithril/stm/struct.StmSig.html).
pub type ProtocolSingleSignature = StmSig<D>;
/// Alias of [MithrilCore:StmAggrSig](https://mithril.network/mithril-core/doc/mithril/stm/struct.StmAggrSig.html).
pub type ProtocolMultiSignature = StmAggrSig<D>;
/// Alias of [MithrilCore:StmVerificationKeyPoP](https://mithril.network/mithril-core/doc/mithril/stm/type.StmVerificationKeyPoP.html).
pub type ProtocolSignerVerificationKey = StmVerificationKeyPoP;
/// Alias of [MithrilCore:StmAggrVerificationKey](https://mithril.network/mithril-core/doc/mithril/stm/struct.StmAggrVerificationKey.html).
pub type ProtocolAggregateVerificationKey = StmAggrVerificationKey<D>;
/// Alias of [Ed25519:PublicKey](https://docs.rs/ed25519-dalek/latest/ed25519_dalek/struct.PublicKey.html).
pub type ProtocolGenesisVerificationKey = ed25519_dalek::PublicKey;
/// Alias of [Ed25519:SecretKey](https://docs.rs/ed25519-dalek/latest/ed25519_dalek/struct.SecretKey.html).
pub type ProtocolGenesisSecretKey = ed25519_dalek::SecretKey;
/// Alias of [Ed25519:Signature](https://docs.rs/ed25519-dalek/latest/ed25519_dalek/struct.Signature.html).
pub type ProtocolGenesisSignature = ed25519_dalek::Signature;
