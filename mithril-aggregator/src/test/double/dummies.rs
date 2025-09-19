use chrono::Utc;
use uuid::Uuid;

use mithril_common::test::double::{Dummy, fake_data};

mod record {
    use mithril_common::entities::{ProtocolMessage, SignedEntityType};

    use crate::database::record::{ImmutableFileDigestRecord, OpenMessageRecord};

    use super::*;

    impl Dummy for ImmutableFileDigestRecord {
        /// Create a dumb ImmutableFileDigestRecord instance mainly for test purposes
        fn dummy() -> Self {
            Self {
                immutable_file_name: "123.chunk".to_string(),
                digest: "dummy_digest".to_string(),
            }
        }
    }

    impl Dummy for OpenMessageRecord {
        /// Create a dumb OpenMessage instance mainly for test purposes
        fn dummy() -> Self {
            let beacon = fake_data::beacon();
            let epoch = beacon.epoch;
            let signed_entity_type = SignedEntityType::CardanoImmutableFilesFull(beacon);

            Self {
                open_message_id: Uuid::parse_str("193d1442-e89b-43cf-9519-04d8db9a12ff").unwrap(),
                epoch,
                signed_entity_type,
                protocol_message: ProtocolMessage::new(),
                is_certified: false,
                is_expired: false,
                created_at: Utc::now(),
                expires_at: None,
            }
        }
    }
}

mod entities {
    use mithril_common::entities::{
        CardanoTransactionsSigningConfig, ProtocolMessage, SignedEntityType,
    };

    use crate::entities::{AggregatorEpochSettings, LeaderAggregatorEpochSettings, OpenMessage};

    use super::*;

    impl Dummy for AggregatorEpochSettings {
        /// Create a dummy `AggregatorEpochSettings`
        fn dummy() -> Self {
            let protocol_parameters = fake_data::protocol_parameters();
            let cardano_transactions_signing_config = CardanoTransactionsSigningConfig::dummy();

            // Aggregator Epoch settings
            AggregatorEpochSettings {
                protocol_parameters,
                cardano_transactions_signing_config,
            }
        }
    }

    impl Dummy for LeaderAggregatorEpochSettings {
        /// Create a dummy `LeaderAggregatorEpochSettings`
        fn dummy() -> Self {
            // Beacon
            let beacon = fake_data::beacon();

            // Registration protocol parameters
            let registration_protocol_parameters = fake_data::protocol_parameters();

            // Signers
            let signers = fake_data::signers(5);
            let current_signers = signers[1..3].to_vec();
            let next_signers = signers[2..5].to_vec();

            // Cardano transactions signing configuration
            let cardano_transactions_signing_config =
                Some(CardanoTransactionsSigningConfig::dummy());

            // Signer Epoch settings
            LeaderAggregatorEpochSettings {
                epoch: beacon.epoch,
                registration_protocol_parameters,
                current_signers,
                next_signers,
                cardano_transactions_signing_config,
            }
        }
    }

    impl Dummy for OpenMessage {
        fn dummy() -> Self {
            let beacon = fake_data::beacon();
            let epoch = beacon.epoch;
            let signed_entity_type = SignedEntityType::CardanoImmutableFilesFull(beacon);

            Self {
                epoch,
                signed_entity_type,
                protocol_message: ProtocolMessage::new(),
                is_certified: false,
                is_expired: false,
                single_signatures: vec![
                    fake_data::single_signature(vec![1, 4, 5]),
                    fake_data::single_signature(vec![2, 3, 8]),
                ],
                created_at: Utc::now(),
                expires_at: None,
            }
        }
    }
}
