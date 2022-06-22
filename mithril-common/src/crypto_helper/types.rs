use mithril::key_reg::KeyReg;
use mithril::stm::{
    Index, Stake, StmAggrSig, StmAggrVerificationKey, StmClerk, StmInitializer, StmParameters,
    StmSig, StmSigner, StmVerificationKeyPoP,
};

pub type Bytes = Vec<u8>;

// Protocol types alias
type D = blake2::Blake2b;
pub type ProtocolPartyId = String;
pub type ProtocolStake = Stake;
pub type ProtocolStakeDistribution = Vec<(ProtocolPartyId, ProtocolStake)>;
pub type ProtocolParameters = StmParameters;
pub type ProtocolLotteryIndex = Index;
pub type ProtocolSigner = StmSigner<D>;
pub type ProtocolInitializer = StmInitializer;
pub type ProtocolClerk = StmClerk<D>;
pub type ProtocolKeyRegistration = KeyReg;
pub type ProtocolSingleSignature = StmSig<D>;
pub type ProtocolMultiSignature = StmAggrSig<D>;
pub type ProtocolSignerVerificationKey = StmVerificationKeyPoP;
pub type ProtocolAggregateVerificationKey = StmAggrVerificationKey<D>;
