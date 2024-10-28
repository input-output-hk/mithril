use mithril_common::entities::{
    Epoch, HexEncodedAggregateVerificationKey, PartyId, SignedEntityType, Stake,
    StakeDistributionParty,
};
use std::collections::BTreeMap;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ExpectedCertificate {
    identifier: String,
    previous_identifier: Option<String>,
    epoch: Epoch,
    signers: BTreeMap<PartyId, Stake>,
    avk: HexEncodedAggregateVerificationKey,
    signed_type: Option<SignedEntityType>,
}

impl ExpectedCertificate {
    pub fn new(
        epoch: Epoch,
        signers: &[StakeDistributionParty],
        avk: HexEncodedAggregateVerificationKey,
        signed_type: SignedEntityType,
        previous_identifier: String,
    ) -> Self {
        Self {
            identifier: Self::identifier(&signed_type),
            previous_identifier: Some(previous_identifier),
            epoch,
            signers: BTreeMap::from_iter(signers.iter().map(|s| (s.party_id.clone(), s.stake))),
            avk,
            signed_type: Some(signed_type),
        }
    }

    pub fn new_genesis(epoch: Epoch, avk: HexEncodedAggregateVerificationKey) -> Self {
        Self {
            identifier: Self::genesis_identifier(epoch),
            previous_identifier: None,
            epoch,
            signers: BTreeMap::new(),
            avk,
            signed_type: None,
        }
    }

    pub fn identifier(signed_types: &SignedEntityType) -> String {
        format!("certificate-{signed_types:?}")
    }

    pub fn genesis_identifier(epoch: Epoch) -> String {
        format!("genesis-{epoch:?}")
    }

    pub fn get_signed_type(&self) -> Option<SignedEntityType> {
        self.signed_type.clone()
    }
}
