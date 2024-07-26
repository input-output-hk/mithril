use mithril_common::entities::{
    CardanoDbBeacon, HexEncodedAgregateVerificationKey, PartyId, SignedEntityType, Stake,
    StakeDistributionParty,
};
use std::collections::BTreeMap;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ExpectedCertificate {
    identifier: String,
    previous_identifier: Option<String>,
    beacon: CardanoDbBeacon,
    signers: BTreeMap<PartyId, Stake>,
    avk: HexEncodedAgregateVerificationKey,
    signed_type: Option<SignedEntityType>,
}

impl ExpectedCertificate {
    pub fn new(
        beacon: CardanoDbBeacon,
        signers: &[StakeDistributionParty],
        avk: HexEncodedAgregateVerificationKey,
        signed_type: SignedEntityType,
        previous_identifier: String,
    ) -> Self {
        Self {
            identifier: Self::identifier(&signed_type),
            previous_identifier: Some(previous_identifier),
            beacon,
            signers: BTreeMap::from_iter(signers.iter().map(|s| (s.party_id.clone(), s.stake))),
            avk,
            signed_type: Some(signed_type),
        }
    }

    pub fn new_genesis(beacon: CardanoDbBeacon, avk: HexEncodedAgregateVerificationKey) -> Self {
        Self {
            identifier: Self::genesis_identifier(&beacon),
            previous_identifier: None,
            beacon,
            signers: BTreeMap::new(),
            avk,
            signed_type: None,
        }
    }

    pub fn identifier(signed_types: &SignedEntityType) -> String {
        format!("certificate-{:?}", signed_types)
    }

    pub fn genesis_identifier(beacon: &CardanoDbBeacon) -> String {
        format!("genesis-{:?}", beacon)
    }

    pub fn get_signed_type(&self) -> Option<SignedEntityType> {
        self.signed_type.clone()
    }
}
