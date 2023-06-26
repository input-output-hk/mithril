use mithril_common::entities::{
    Beacon, HexEncodedAgregateVerificationKey, PartyId, SignedEntityType, SignerWithStake, Stake,
};
use std::collections::BTreeMap;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ExpectedCertificate {
    identifier: String,
    previous_identifier: Option<String>,
    beacon: Beacon,
    signers: BTreeMap<PartyId, Stake>,
    avk: HexEncodedAgregateVerificationKey,
    signed_type: Option<SignedEntityType>,
}

impl ExpectedCertificate {
    pub fn new(
        beacon: Beacon,
        signers: &[SignerWithStake],
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

    pub fn new_genesis(beacon: Beacon, avk: HexEncodedAgregateVerificationKey) -> Self {
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

    pub fn genesis_identifier(beacon: &Beacon) -> String {
        format!("genesis-{:?}", beacon)
    }
}
