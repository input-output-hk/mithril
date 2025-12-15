use std::collections::BTreeSet;

use mithril_stm::AggregateSignatureType;
use serde::{Deserialize, Serialize};

use crate::entities::SignedEntityTypeDiscriminants;

/// Message advertised by an Aggregator to inform about its features
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct AggregatorFeaturesMessage {
    /// Version of the OpenAPI specification
    pub open_api_version: String,

    /// URL of the documentation
    pub documentation_url: String,

    /// Capabilities of the Aggregator
    pub capabilities: AggregatorCapabilities,
}

/// Capabilities of an Aggregator
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct AggregatorCapabilities {
    /// Signed entity types that are signed by the aggregator
    pub signed_entity_types: BTreeSet<SignedEntityTypeDiscriminants>,

    /// Aggregate signature type used by the aggregator
    #[serde(default)]
    pub aggregate_signature_type: AggregateSignatureType,

    /// Cardano transactions prover capabilities
    #[serde(skip_serializing_if = "Option::is_none")]
    pub cardano_transactions_prover: Option<CardanoTransactionsProverCapabilities>,
}

/// Cardano transactions prover capabilities
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct CardanoTransactionsProverCapabilities {
    /// Maximum number of hashes allowed for a single request
    pub max_hashes_allowed_by_request: usize,
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::entities::CardanoTransactionsSigningConfig;

    #[derive(Debug, Serialize, Deserialize, PartialEq)]
    struct AggregatorFeaturesMessageUntilV0_1_45 {
        pub open_api_version: String,
        pub documentation_url: String,
        pub capabilities: AggregatorCapabilitiesUntilV0_1_45,
    }

    #[derive(Debug, Serialize, Deserialize, PartialEq)]
    struct AggregatorCapabilitiesUntilV0_1_45 {
        pub signed_entity_types: BTreeSet<SignedEntityTypeDiscriminants>,
        #[serde(skip_serializing_if = "Option::is_none")]
        pub cardano_transactions_prover: Option<CardanoTransactionsProverCapabilities>,
        #[serde(skip_serializing_if = "Option::is_none")]
        pub cardano_transactions_signing_config: Option<CardanoTransactionsSigningConfig>,
    }

    fn golden_message_until_open_api_0_1_45() -> AggregatorFeaturesMessageUntilV0_1_45 {
        AggregatorFeaturesMessageUntilV0_1_45 {
            open_api_version: "0.0.1".to_string(),
            documentation_url: "https://example.com".to_string(),
            capabilities: AggregatorCapabilitiesUntilV0_1_45 {
                signed_entity_types: BTreeSet::from([
                    SignedEntityTypeDiscriminants::CardanoTransactions,
                ]),
                cardano_transactions_prover: Some(CardanoTransactionsProverCapabilities {
                    max_hashes_allowed_by_request: 100,
                }),
                cardano_transactions_signing_config: None,
            },
        }
    }

    #[derive(Debug, Serialize, Deserialize, PartialEq)]
    struct AggregatorFeaturesMessageUntilV0_6_17 {
        pub open_api_version: String,
        pub documentation_url: String,
        pub capabilities: AggregatorCapabilitiesUntilV0_6_17,
    }

    #[derive(Debug, Serialize, Deserialize, PartialEq)]
    struct AggregatorCapabilitiesUntilV0_6_17 {
        pub signed_entity_types: BTreeSet<SignedEntityTypeDiscriminants>,
        #[serde(skip_serializing_if = "Option::is_none")]
        pub cardano_transactions_prover: Option<CardanoTransactionsProverCapabilities>,
        #[serde(skip_serializing_if = "Option::is_none")]
        pub cardano_transactions_signing_config: Option<CardanoTransactionsSigningConfig>,
    }

    fn golden_message_until_open_api_0_6_17() -> AggregatorFeaturesMessageUntilV0_6_17 {
        AggregatorFeaturesMessageUntilV0_6_17 {
            open_api_version: "0.0.1".to_string(),
            documentation_url: "https://example.com".to_string(),
            capabilities: AggregatorCapabilitiesUntilV0_6_17 {
                signed_entity_types: BTreeSet::from([
                    SignedEntityTypeDiscriminants::CardanoTransactions,
                ]),
                cardano_transactions_prover: Some(CardanoTransactionsProverCapabilities {
                    max_hashes_allowed_by_request: 100,
                }),
                cardano_transactions_signing_config: None,
            },
        }
    }

    fn golden_message_current() -> AggregatorFeaturesMessage {
        AggregatorFeaturesMessage {
            open_api_version: "0.0.1".to_string(),
            documentation_url: "https://example.com".to_string(),
            capabilities: AggregatorCapabilities {
                signed_entity_types: BTreeSet::from([
                    SignedEntityTypeDiscriminants::CardanoTransactions,
                ]),
                aggregate_signature_type: AggregateSignatureType::Concatenation,
                cardano_transactions_prover: Some(CardanoTransactionsProverCapabilities {
                    max_hashes_allowed_by_request: 100,
                }),
            },
        }
    }

    const CURRENT_JSON: &str = r#"{
        "open_api_version": "0.0.1",
        "documentation_url": "https://example.com",
        "capabilities": {
            "signed_entity_types": ["CardanoTransactions"],
            "aggregate_signature_type": "Concatenation",
            "cardano_transactions_prover": {
                "max_hashes_allowed_by_request": 100
            }
        }
    }"#;

    #[test]
    fn test_current_json_deserialized_into_message_supported_until_open_api_0_1_45() {
        let json = CURRENT_JSON;
        let message: AggregatorFeaturesMessageUntilV0_1_45 = serde_json::from_str(json).unwrap();

        assert_eq!(golden_message_until_open_api_0_1_45(), message);
    }

    #[test]
    fn test_current_json_deserialized_into_message_supported_until_open_api_0_6_17() {
        let json = CURRENT_JSON;
        let message: AggregatorFeaturesMessageUntilV0_6_17 = serde_json::from_str(json).unwrap();

        assert_eq!(golden_message_until_open_api_0_6_17(), message);
    }

    #[test]
    fn test_current_json_deserialized_into_current_message() {
        let json = CURRENT_JSON;
        let message: AggregatorFeaturesMessage = serde_json::from_str(json).unwrap();

        assert_eq!(golden_message_current(), message);
    }
}
