use serde::{Deserialize, Serialize};

use crate::{
    entities::{CardanoEra, Epoch, ProtocolParameters, Stake, TotalSPOs},
    era::SupportedEra,
};

/// Message advertised by an aggregator to inform about its status
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct AggregatorStatusMessage {
    /// Current epoch
    pub epoch: Epoch,

    /// Current Cardano era
    pub cardano_era: CardanoEra,

    /// Cardano network
    pub cardano_network: String,

    /// Current Mithril era
    pub mithril_era: SupportedEra,

    /// Cardano node version
    pub cardano_node_version: String,

    /// Aggregator node version
    pub aggregator_node_version: String,

    /// Current Protocol parameters
    #[serde(rename = "protocol")]
    pub protocol_parameters: ProtocolParameters,

    /// Next Protocol parameters
    #[serde(rename = "next_protocol")]
    pub next_protocol_parameters: ProtocolParameters,

    /// The number of signers for the current epoch
    pub total_signers: usize,

    /// The number of signers that will be able to sign on the next epoch
    pub total_next_signers: usize,

    /// The total stakes of the signers for the current epoch
    pub total_stakes_signers: Stake,

    /// The total stakes of the signers that will be able to sign on the next epoch
    pub total_next_stakes_signers: Stake,

    /// The number of Cardano SPOs
    pub total_cardano_spo: TotalSPOs,

    /// The total stake in Cardano
    pub total_cardano_stake: Stake,
}

#[cfg(test)]
mod tests {
    use super::*;

    const ACTUAL_JSON: &str = r#"{
        "epoch": 48,
        "cardano_era": "conway",
        "cardano_network": "mainnet",
        "mithril_era": "pythagoras",
        "cardano_node_version": "1.2.3",
        "aggregator_node_version": "4.5.6",
        "protocol": { "k": 5, "m": 100, "phi_f": 0.65 },
        "next_protocol": { "k": 50, "m": 1000, "phi_f": 0.65 },
        "total_signers": 1234,
        "total_next_signers": 56789,
        "total_stakes_signers": 123456789,
        "total_next_stakes_signers": 987654321,
        "total_cardano_spo": 7777,
        "total_cardano_stake": 888888888
        }"#;

    fn golden_actual_message() -> AggregatorStatusMessage {
        AggregatorStatusMessage {
            epoch: Epoch(48),
            cardano_era: "conway".to_string(),
            cardano_network: "mainnet".to_string(),
            mithril_era: SupportedEra::Pythagoras,
            cardano_node_version: "1.2.3".to_string(),
            aggregator_node_version: "4.5.6".to_string(),
            protocol_parameters: ProtocolParameters {
                k: 5,
                m: 100,
                phi_f: 0.65,
            },
            next_protocol_parameters: ProtocolParameters {
                k: 50,
                m: 1000,
                phi_f: 0.65,
            },
            total_signers: 1234,
            total_next_signers: 56789,
            total_stakes_signers: 123456789,
            total_next_stakes_signers: 987654321,
            total_cardano_spo: 7777,
            total_cardano_stake: 888888888,
        }
    }

    // Test the compatibility with current structure.
    #[test]
    fn test_actual_json_deserialized_into_actual_message() {
        let json = ACTUAL_JSON;
        let message: AggregatorStatusMessage = serde_json::from_str(json).expect(
            "This JSON is expected to be successfully parsed into a AggregatorStatusMessage instance.",
        );

        assert_eq!(golden_actual_message(), message);
    }
}
