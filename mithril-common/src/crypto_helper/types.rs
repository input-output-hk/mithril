use mithril::key_reg::KeyReg;
use mithril::msp::{MspPk, MspSk};
use mithril::stm::{
    Index, PartyId, Stake, StmClerk, StmInitializer, StmMultiSig, StmParameters, StmSig, StmSigner,
};

pub type Bytes = Vec<u8>;

// Protocol types alias
type D = blake2::Blake2b;
pub type ProtocolPartyId = PartyId;
pub type ProtocolStake = Stake;
pub type ProtocolStakeDistribution = Vec<(ProtocolPartyId, ProtocolStake)>;
pub type ProtocolParameters = StmParameters;
pub type ProtocolLotteryIndex = Index;
pub type ProtocolSigner = StmSigner<D>;
pub type ProtocolInitializer = StmInitializer;
pub type ProtocolClerk = StmClerk<D>;
pub type ProtocolKeyRegistration = KeyReg;
pub type ProtocolSingleSignature = StmSig<D>;
pub type ProtocolMultiSignature = StmMultiSig<D>;
pub type ProtocolSignerVerificationKey = MspPk;
pub type ProtocolSignerSecretKey = MspSk;
