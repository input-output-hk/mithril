use std::collections::{BTreeSet, HashMap};

use mithril_common::entities::{Epoch, ProtocolParameters, SignedEntityTypeDiscriminants};

use crate::model::{MithrilNetworkConfiguration, SignedEntityTypeConfiguration};

pub fn mithril_network_configuration(
    epoch: u64,
    k: u64,
    m: u64,
    phi_f: f64,
    available_signed_entity_types: BTreeSet<SignedEntityTypeDiscriminants>,
    signed_entity_types_config: HashMap<
        SignedEntityTypeDiscriminants,
        SignedEntityTypeConfiguration,
    >,
) -> MithrilNetworkConfiguration {
    MithrilNetworkConfiguration {
        epoch: Epoch(epoch),
        signer_registration_protocol_parameters: ProtocolParameters { k, m, phi_f },
        available_signed_entity_types,
        signed_entity_types_config,
    }
}
