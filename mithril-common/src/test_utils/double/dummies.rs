use chrono::{DateTime, Utc};

use crate::test_utils::double::{Dummy, fake_data, fake_keys};

mod entities {
    use crate::crypto_helper::MKTreeStoreInMemory;
    use crate::entities::*;

    use super::*;

    impl Dummy for ChainPoint {
        /// Return a dummy [ChainPoint] (test-only).
        fn dummy() -> Self {
            Self {
                slot_number: SlotNumber(100),
                block_number: BlockNumber(0),
                block_hash: "block_hash-50".to_string(),
            }
        }
    }

    impl Dummy for CardanoTransactionsSetProof {
        /// Return a dummy [CardanoTransactionsSetProof] (test-only).
        fn dummy() -> Self {
            let leaves = vec![
                (BlockNumber(0), "tx-1".to_string()),
                (BlockNumber(1), "tx-2".to_string()),
                (BlockNumber(1), "tx-3".to_string()),
                (BlockNumber(10), "tx-4".to_string()),
                (BlockNumber(20), "tx-5".to_string()),
                (BlockNumber(22), "tx-6".to_string()),
            ];

            Self::from_leaves::<MKTreeStoreInMemory>(&leaves).unwrap()
        }
    }

    impl Dummy for SignedEntityConfig {
        /// Return a dummy [SignedEntityConfig] (test-only).
        fn dummy() -> Self {
            Self {
                allowed_discriminants: SignedEntityTypeDiscriminants::all(),
                cardano_transactions_signing_config: CardanoTransactionsSigningConfig::dummy(),
            }
        }
    }

    impl Dummy for CardanoTransactionsSigningConfig {
        /// Return a dummy [CardanoTransactionsSigningConfig] (test-only).
        fn dummy() -> Self {
            Self {
                security_parameter: BlockNumber(0),
                step: BlockNumber(15),
            }
        }
    }

    impl Dummy for SignedEntityType {
        /// Return a dummy [SignedEntityType] (test-only).
        fn dummy() -> Self {
            Self::MithrilStakeDistribution(Epoch(5))
        }
    }

    impl Dummy for SupportedEra {
        /// Return a dummy [SupportedEra] (test-only).
        fn dummy() -> Self {
            Self::eras().first().unwrap().to_owned()
        }
    }

    impl Dummy for TimePoint {
        /// Return a dummy [TimePoint] (test-only).
        fn dummy() -> Self {
            Self::new(10, 100, ChainPoint::dummy())
        }
    }
}

mod messages {
    use chrono::Duration;
    use std::collections::BTreeSet;

    use crate::entities::{
        AncillaryLocation, BlockNumber, CardanoDbBeacon, CardanoTransactionsSetProof,
        CardanoTransactionsSigningConfig, CompressionAlgorithm, DigestLocation, Epoch,
        ImmutablesLocation, MultiFilesUri, ProtocolMessage, ProtocolMessagePartKey,
        ProtocolParameters, SignedEntityType, SignedEntityTypeDiscriminants, StakeDistribution,
        StakeDistributionParty, TemplateUri,
    };
    use crate::messages::*;

    use super::*;

    impl Dummy for CardanoTransactionsSetProofMessagePart {
        /// Return a dummy [CardanoTransactionsSetProofMessagePart] (test-only).
        fn dummy() -> Self {
            CardanoTransactionsSetProof::dummy().try_into().unwrap()
        }
    }

    impl Dummy for CertificateMetadataMessagePart {
        /// Return a dummy [CertificateMetadataMessagePart] (test-only).
        fn dummy() -> Self {
            let initiated_at = DateTime::parse_from_rfc3339("2024-02-12T13:11:47Z")
                .unwrap()
                .with_timezone(&Utc);

            Self {
                network: "testnet".to_string(),
                protocol_version: "0.1.0".to_string(),
                protocol_parameters: ProtocolParameters::new(1000, 100, 0.123),
                initiated_at,
                sealed_at: initiated_at + Duration::try_seconds(100).unwrap(),
                signers: vec![
                    StakeDistributionParty {
                        party_id: "1".to_string(),
                        stake: 10,
                    },
                    StakeDistributionParty {
                        party_id: "2".to_string(),
                        stake: 20,
                    },
                ],
            }
        }
    }

    impl Dummy for SignerWithStakeMessagePart {
        /// Return a dummy [SignerWithStakeMessagePart] (test-only).
        fn dummy() -> Self {
            Self {
                party_id: "pool1m8crhnqj5k2kyszf5j2scshupystyxc887zdfrpzh6ty6eun4fx".to_string(),
                verification_key: fake_keys::signer_verification_key()[0].to_string(),
                verification_key_signature: Some(
                    fake_keys::signer_verification_key_signature()[0].to_string(),
                ),
                operational_certificate: Some(fake_keys::operational_certificate()[0].to_string()),
                kes_period: Some(6),
                stake: 234,
            }
        }
    }

    impl Dummy for SignerMessagePart {
        /// Return a dummy [SignerMessagePart] (test-only).
        fn dummy() -> Self {
            Self {
                party_id: "pool1m8crhnqj5k2kyszf5j2scshupystyxc887zdfrpzh6ty6eun4fx".to_string(),
                verification_key: fake_keys::signer_verification_key()[0].to_string(),
                verification_key_signature: Some(
                    fake_keys::signer_verification_key_signature()[0].to_string(),
                ),
                operational_certificate: Some(fake_keys::operational_certificate()[0].to_string()),
                kes_period: Some(6),
            }
        }
    }

    impl Dummy for AggregatorFeaturesMessage {
        /// Return a dummy [AggregatorFeaturesMessage] (test-only).
        fn dummy() -> Self {
            AggregatorFeaturesMessage {
                open_api_version: "0.0.1".to_string(),
                documentation_url: "https://example.com".to_string(),
                capabilities: AggregatorCapabilities {
                    signed_entity_types: BTreeSet::from([
                        SignedEntityTypeDiscriminants::MithrilStakeDistribution,
                    ]),
                    cardano_transactions_prover: None,
                },
            }
        }
    }

    impl Dummy for CardanoDatabaseSnapshotMessage {
        /// Return a dummy [CardanoDatabaseSnapshotMessage] (test-only).
        fn dummy() -> Self {
            Self {
                hash: "d4071d518a3ace0f6c04a9c0745b9e9560e3e2af1b373bafc4e0398423e9abfb"
                    .to_string(),
                merkle_root: "c8224920b9f5ad7377594eb8a15f34f08eb3103cc5241d57cafc5638403ec7c6"
                    .to_string(),
                network: "preview".to_string(),
                beacon: CardanoDbBeacon {
                    epoch: Epoch(123),
                    immutable_file_number: 2345,
                },
                certificate_hash:
                    "f6c01b373bafc4e039844071d5da3ace4a9c0745b9e9560e3e2af01823e9abfb".to_string(),
                total_db_size_uncompressed: 800796318,
                created_at: DateTime::parse_from_rfc3339("2023-01-19T13:43:05.618857482Z")
                    .unwrap()
                    .with_timezone(&Utc),
                digests: DigestsMessagePart {
                    size_uncompressed: 1024,
                    locations: vec![DigestLocation::Aggregator {
                        uri: "https://host-1/digest-1".to_string(),
                    }],
                },
                immutables: ImmutablesMessagePart {
                    average_size_uncompressed: 512,
                    locations: vec![
                        ImmutablesLocation::CloudStorage {
                            uri: MultiFilesUri::Template(TemplateUri(
                                "https://host-1/immutables-2".to_string(),
                            )),
                            compression_algorithm: Some(CompressionAlgorithm::Gzip),
                        },
                        ImmutablesLocation::CloudStorage {
                            uri: MultiFilesUri::Template(TemplateUri(
                                "https://host-2/immutables-2".to_string(),
                            )),
                            compression_algorithm: Some(CompressionAlgorithm::Gzip),
                        },
                    ],
                },
                ancillary: AncillaryMessagePart {
                    size_uncompressed: 2048,
                    locations: vec![AncillaryLocation::CloudStorage {
                        uri: "https://host-1/ancillary-3".to_string(),
                        compression_algorithm: Some(CompressionAlgorithm::Gzip),
                    }],
                },
                cardano_node_version: "0.0.1".to_string(),
            }
        }
    }

    impl Dummy for CardanoDatabaseDigestListItemMessage {
        /// Return a dummy [CardanoDatabaseDigestListItemMessage] (test-only).
        fn dummy() -> Self {
            Self {
                immutable_file_name: "06685.chunk".to_string(),
                digest: "0af556ab2620dd9363bf76963a231abe8948a500ea6be31b131d87907ab09b1e"
                    .to_string(),
            }
        }
    }

    impl Dummy for CardanoDatabaseImmutableFilesRestoredMessage {
        /// Return a dummy [CardanoDatabaseImmutableFilesRestoredMessage] (test-only).
        fn dummy() -> Self {
            Self {
                nb_immutable_files: 34,
            }
        }
    }

    impl Dummy for CardanoDatabaseSnapshotListItemMessage {
        /// Return a dummy [CardanoDatabaseSnapshotListItemMessage] (test-only).
        fn dummy() -> Self {
            Self {
                hash: "d4071d518a3ace0f6c04a9c0745b9e9560e3e2af1b373bafc4e0398423e9abfb"
                    .to_string(),
                merkle_root: "c8224920b9f5ad7377594eb8a15f34f08eb3103cc5241d57cafc5638403ec7c6"
                    .to_string(),
                beacon: CardanoDbBeacon {
                    epoch: Epoch(123),
                    immutable_file_number: 2345,
                },
                certificate_hash:
                    "f6c01b373bafc4e039844071d5da3ace4a9c0745b9e9560e3e2af01823e9abfb".to_string(),
                total_db_size_uncompressed: 800796318,
                created_at: DateTime::parse_from_rfc3339("2023-01-19T13:43:05.618857482Z")
                    .unwrap()
                    .with_timezone(&Utc),
                cardano_node_version: "0.0.1".to_string(),
            }
        }
    }

    impl Dummy for CardanoStakeDistributionMessage {
        /// Return a dummy [CardanoStakeDistributionMessage] (test-only).
        fn dummy() -> Self {
            Self {
                epoch: Epoch(1),
                hash: "hash-123".to_string(),
                certificate_hash: "cert-hash-123".to_string(),
                stake_distribution: StakeDistribution::from([("pool-123".to_string(), 1000)]),
                created_at: DateTime::parse_from_rfc3339("2024-07-29T16:15:05.618857482Z")
                    .unwrap()
                    .with_timezone(&Utc),
            }
        }
    }

    impl Dummy for CardanoStakeDistributionListItemMessage {
        /// Return a dummy [CardanoStakeDistributionListItemMessage] (test-only).
        fn dummy() -> Self {
            Self {
                epoch: Epoch(1),
                hash: "hash-123".to_string(),
                certificate_hash: "certificate-hash-123".to_string(),
                created_at: DateTime::parse_from_rfc3339("2024-07-29T16:15:05.618857482Z")
                    .unwrap()
                    .with_timezone(&Utc),
            }
        }
    }

    impl Dummy for CardanoTransactionSnapshotMessage {
        /// Return a dummy [CertificateMessage] (test-only).
        fn dummy() -> Self {
            Self {
                merkle_root: "mkroot-123".to_string(),
                epoch: Epoch(10),
                block_number: BlockNumber(100),
                hash: "hash-123".to_string(),
                certificate_hash: "cert-hash-123".to_string(),
                created_at: DateTime::parse_from_rfc3339("2023-01-19T13:43:05.618857482Z")
                    .unwrap()
                    .with_timezone(&Utc),
            }
        }
    }

    impl Dummy for CardanoTransactionSnapshotListItemMessage {
        /// Return a dummy [CertificateMessage] (test-only).
        fn dummy() -> Self {
            Self {
                merkle_root: "mkroot-123".to_string(),
                epoch: Epoch(10),
                block_number: BlockNumber(100),
                hash: "hash-123".to_string(),
                certificate_hash: "cert-hash-123".to_string(),
                created_at: DateTime::parse_from_rfc3339("2023-01-19T13:43:05.618857482Z")
                    .unwrap()
                    .with_timezone(&Utc),
            }
        }
    }

    impl Dummy for CertificateMessage {
        /// Return a dummy [CertificateMessage] (test-only).
        fn dummy() -> Self {
            let mut protocol_message = ProtocolMessage::new();
            protocol_message.set_message_part(
                ProtocolMessagePartKey::SnapshotDigest,
                "snapshot-digest-123".to_string(),
            );
            protocol_message.set_message_part(
                ProtocolMessagePartKey::NextAggregateVerificationKey,
                fake_keys::aggregate_verification_key()[1].to_owned(),
            );
            let epoch = Epoch(10);

            Self {
                hash: "hash".to_string(),
                previous_hash: "previous_hash".to_string(),
                epoch,
                signed_entity_type: SignedEntityType::MithrilStakeDistribution(epoch),
                metadata: CertificateMetadataMessagePart::dummy(),
                protocol_message: protocol_message.clone(),
                signed_message: "signed_message".to_string(),
                aggregate_verification_key: fake_keys::aggregate_verification_key()[0].to_owned(),
                multi_signature: fake_keys::multi_signature()[0].to_owned(),
                genesis_signature: String::new(),
            }
        }
    }

    impl Dummy for CertificateListItemMessage {
        /// Return a dummy [CertificateListItemMessage] (test-only).
        fn dummy() -> Self {
            let mut protocol_message = ProtocolMessage::new();
            protocol_message.set_message_part(
                ProtocolMessagePartKey::SnapshotDigest,
                "snapshot-digest-123".to_string(),
            );
            protocol_message.set_message_part(
                ProtocolMessagePartKey::NextAggregateVerificationKey,
                "next-avk-123".to_string(),
            );
            let epoch = Epoch(10);

            Self {
                hash: "hash".to_string(),
                previous_hash: "previous_hash".to_string(),
                epoch,
                signed_entity_type: SignedEntityType::MithrilStakeDistribution(epoch),
                metadata: CertificateListItemMessageMetadata {
                    network: "testnet".to_string(),
                    protocol_version: "0.1.0".to_string(),
                    protocol_parameters: ProtocolParameters::new(1000, 100, 0.123),
                    initiated_at: DateTime::parse_from_rfc3339("2024-02-12T13:11:47Z")
                        .unwrap()
                        .with_timezone(&Utc),
                    sealed_at: DateTime::parse_from_rfc3339("2024-02-12T13:12:57Z")
                        .unwrap()
                        .with_timezone(&Utc),
                    total_signers: 2,
                },
                protocol_message: protocol_message.clone(),
                signed_message: "signed_message".to_string(),
                aggregate_verification_key: "aggregate_verification_key".to_string(),
            }
        }
    }

    impl Dummy for EpochSettingsMessage {
        /// Return a dummy [EpochSettingsMessage] (test-only).
        fn dummy() -> Self {
            Self {
                epoch: Epoch(10),
                signer_registration_protocol_parameters: ProtocolParameters {
                    k: 5,
                    m: 100,
                    phi_f: 0.65,
                },
                current_signers: [SignerMessagePart::dummy()].to_vec(),
                next_signers: [SignerMessagePart::dummy()].to_vec(),
                cardano_transactions_signing_config: Some(CardanoTransactionsSigningConfig::dummy()),
                next_cardano_transactions_signing_config: Some(
                    CardanoTransactionsSigningConfig::dummy(),
                ),
            }
        }
    }

    impl Dummy for MithrilStakeDistributionMessage {
        /// Return a dummy [MithrilStakeDistributionMessage] (test-only).
        fn dummy() -> Self {
            Self {
                epoch: Epoch(1),
                signers_with_stake: vec![SignerWithStakeMessagePart::dummy()],
                hash: "hash-123".to_string(),
                certificate_hash: "cert-hash-123".to_string(),
                created_at: DateTime::parse_from_rfc3339("2023-01-19T13:43:05.618857482Z")
                    .unwrap()
                    .with_timezone(&Utc),
                protocol_parameters: fake_data::protocol_parameters(),
            }
        }
    }

    impl Dummy for MithrilStakeDistributionListItemMessage {
        /// Return a dummy [MithrilStakeDistributionListItemMessage] (test-only).
        fn dummy() -> Self {
            Self {
                epoch: Epoch(1),
                hash: "hash-123".to_string(),
                certificate_hash: "certificate-hash-123".to_string(),
                created_at: DateTime::parse_from_rfc3339("2023-01-19T13:43:05.618857482Z")
                    .unwrap()
                    .with_timezone(&Utc),
            }
        }
    }

    impl Dummy for RegisterSignatureMessageHttp {
        /// Return a dummy [RegisterSignatureMessageHttp] (test-only).
        fn dummy() -> Self {
            use crate::entities::Epoch;
            Self {
                signed_entity_type: SignedEntityType::MithrilStakeDistribution(Epoch(5)),
                party_id: "party_id".to_string(),
                signature: fake_keys::single_signature()[0].to_string(),
                won_indexes: vec![1, 3],
                signed_message: "6a7e737c312972d2346b65ac3075696e04286d046dddaf8004121e3d5e27cc0d"
                    .to_string(),
            }
        }
    }

    impl Dummy for RegisterSignatureMessageDmq {
        /// Return a dummy [RegisterSignatureMessageDmq] (test-only).
        fn dummy() -> Self {
            use crate::entities::Epoch;
            Self {
                signed_entity_type: SignedEntityType::MithrilStakeDistribution(Epoch(5)),
                signature: fake_keys::single_signature()[0].try_into().unwrap(),
            }
        }
    }

    impl Dummy for RegisterSignerMessage {
        /// Return a dummy [RegisterSignerMessage] (test-only).
        fn dummy() -> Self {
            Self {
                epoch: Epoch(1),
                party_id: "pool1m8crhnqj5k2kyszf5j2scshupystyxc887zdfrpzh6ty6eun4fx".to_string(),
                verification_key: fake_keys::signer_verification_key()[0].to_string(),
                verification_key_signature: Some(
                    fake_keys::signer_verification_key_signature()[0].to_string(),
                ),
                operational_certificate: Some(fake_keys::operational_certificate()[0].to_string()),
                kes_period: Some(6),
            }
        }
    }

    impl Dummy for SnapshotMessage {
        /// Return a dummy [SnapshotMessage] (test-only).
        fn dummy() -> Self {
            Self {
                digest: "0b9f5ad7f33cc523775c82249294eb8a1541d54f08eb3107cafc5638403ec7c6"
                    .to_string(),
                network: "preview".to_string(),
                beacon: CardanoDbBeacon {
                    epoch: Epoch(86),
                    immutable_file_number: 1728,
                },
                certificate_hash:
                    "d5daf6c03ace4a9c074e951844075b9b373bafc4e039160e3e2af01823e9abfb".to_string(),
                size: 807803196,
                ancillary_size: Some(123456789),
                created_at: DateTime::parse_from_rfc3339("2023-01-19T13:43:05.618857482Z")
                    .unwrap()
                    .with_timezone(&Utc),
                locations: vec!["https://host/certificate.tar.gz".to_string()],
                ancillary_locations: Some(vec!["https://host/ancillary.tar.gz".to_string()]),
                compression_algorithm: CompressionAlgorithm::Gzip,
                cardano_node_version: "0.0.1".to_string(),
            }
        }
    }

    impl Dummy for SnapshotDownloadMessage {
        /// Return a dummy [SnapshotDownloadMessage] (test-only).
        fn dummy() -> Self {
            Self {
                digest: "0b9f5ad7f33cc523775c82249294eb8a1541d54f08eb3107cafc5638403ec7c6"
                    .to_string(),
                network: "preview".to_string(),
                beacon: CardanoDbBeacon {
                    epoch: Epoch(86),
                    immutable_file_number: 1728,
                },
                size: 807803196,
                ancillary_size: Some(123456789),
                locations: vec!["https://host/certificate.tar.gz".to_string()],
                ancillary_locations: Some(vec!["https://host/ancillary.tar.gz".to_string()]),
                compression_algorithm: CompressionAlgorithm::Gzip,
                cardano_node_version: "0.0.1".to_string(),
            }
        }
    }

    impl Dummy for SnapshotListItemMessage {
        /// Return a dummy [SnapshotListItemMessage] (test-only).
        fn dummy() -> Self {
            Self {
                digest: "0b9f5ad7f33cc523775c82249294eb8a1541d54f08eb3107cafc5638403ec7c6"
                    .to_string(),
                network: "preview".to_string(),
                beacon: CardanoDbBeacon {
                    epoch: Epoch(86),
                    immutable_file_number: 1728,
                },
                certificate_hash:
                    "d5daf6c03ace4a9c074e951844075b9b373bafc4e039160e3e2af01823e9abfb".to_string(),
                size: 807803196,
                ancillary_size: Some(123456789),
                created_at: DateTime::parse_from_rfc3339("2023-01-19T13:43:05.618857482Z")
                    .unwrap()
                    .with_timezone(&Utc),
                locations: vec!["https://host/certificate.tar.gz".to_string()],
                ancillary_locations: Some(vec!["https://host/ancillary.tar.gz".to_string()]),
                compression_algorithm: CompressionAlgorithm::default(),
                cardano_node_version: "0.0.1".to_string(),
            }
        }
    }
}

mod signable_builder {
    use crate::entities::{
        CardanoDbBeacon, CardanoStakeDistribution, CardanoTransactionsSnapshot, Epoch,
        MithrilStakeDistribution, SignedEntityType, Snapshot,
    };
    use crate::signable_builder::SignedEntity;

    use super::*;

    impl Dummy for SignedEntity<Snapshot> {
        /// Create a dummy [SignedEntity] for [Snapshot] entity
        fn dummy() -> Self {
            SignedEntity {
                signed_entity_id: "snapshot-id-123".to_string(),
                signed_entity_type: SignedEntityType::CardanoImmutableFilesFull(
                    CardanoDbBeacon::default(),
                ),
                certificate_id: "certificate-hash-123".to_string(),
                artifact: fake_data::snapshots(1)[0].to_owned(),
                created_at: DateTime::parse_from_rfc3339("2023-01-19T13:43:05.618857482Z")
                    .unwrap()
                    .with_timezone(&Utc),
            }
        }
    }

    impl Dummy for SignedEntity<MithrilStakeDistribution> {
        /// Create a dummy [SignedEntity] for [MithrilStakeDistribution] entity
        fn dummy() -> Self {
            SignedEntity {
                signed_entity_id: "mithril-stake-distribution-id-123".to_string(),
                signed_entity_type: SignedEntityType::MithrilStakeDistribution(Epoch(1)),
                certificate_id: "certificate-hash-123".to_string(),
                artifact: fake_data::mithril_stake_distributions(1)[0].to_owned(),
                created_at: DateTime::parse_from_rfc3339("2023-01-19T13:43:05.618857482Z")
                    .unwrap()
                    .with_timezone(&Utc),
            }
        }
    }

    impl Dummy for SignedEntity<CardanoTransactionsSnapshot> {
        /// Create a dummy [SignedEntity] for [CardanoTransactionsSnapshot] entity
        fn dummy() -> Self {
            let block_number = crate::entities::BlockNumber(50);
            SignedEntity {
                signed_entity_id: "snapshot-id-123".to_string(),
                signed_entity_type: SignedEntityType::CardanoTransactions(Epoch(5), block_number),
                certificate_id: "certificate-hash-123".to_string(),
                artifact: CardanoTransactionsSnapshot::new("mkroot123".to_string(), block_number),
                created_at: DateTime::parse_from_rfc3339("2023-01-19T13:43:05.618857482Z")
                    .unwrap()
                    .with_timezone(&Utc),
            }
        }
    }

    impl Dummy for SignedEntity<CardanoStakeDistribution> {
        /// Create a dummy [SignedEntity] for [CardanoStakeDistribution] entity
        fn dummy() -> Self {
            SignedEntity {
                signed_entity_id: "cardano-stake-distribution-id-123".to_string(),
                signed_entity_type: SignedEntityType::CardanoStakeDistribution(Epoch(1)),
                certificate_id: "certificate-hash-123".to_string(),
                artifact: fake_data::cardano_stake_distributions(1)[0].to_owned(),
                created_at: DateTime::parse_from_rfc3339("2024-07-29T16:15:05.618857482Z")
                    .unwrap()
                    .with_timezone(&Utc),
            }
        }
    }
}
