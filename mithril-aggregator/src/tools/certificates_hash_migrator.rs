use std::{collections::HashMap, sync::Arc};

use anyhow::{anyhow, Context};
use slog_scope::{debug, info, trace};

use mithril_common::{entities::Certificate, StdResult};

use crate::database::repository::{CertificateRepository, SignedEntityStorer};

/// Tools to recompute all the certificates hashes in a aggregator database.
pub struct CertificatesHashMigrator {
    certificate_repository: CertificateRepository,
    signed_entity_storer: Arc<dyn SignedEntityStorer>,
}

impl CertificatesHashMigrator {
    /// [CertificatesHashMigrator] factory
    pub fn new(
        certificate_repository: CertificateRepository,
        signed_entity_storer: Arc<dyn SignedEntityStorer>,
    ) -> Self {
        Self {
            certificate_repository,
            signed_entity_storer,
        }
    }

    /// Recompute all the certificates hashes the database.
    pub async fn migrate(&self) -> StdResult<()> {
        info!("🔧 Certificate Hash Migrator: starting");
        let (old_certificates, old_and_new_hashes) =
            self.create_certificates_with_updated_hash().await?;

        self.update_signed_entities_certificate_hash(old_and_new_hashes)
            .await?;

        self.cleanup(old_certificates).await?;

        info!("🔧 Certificate Hash Migrator: all certificates have been migrated successfully");
        Ok(())
    }

    /// Load all certificates from the database, compute their new hash, returns a list with
    /// all old certificates joined with their new hash string.
    async fn create_certificates_with_updated_hash(
        &self,
    ) -> StdResult<(Vec<Certificate>, HashMap<String, String>)> {
        info!("🔧 Certificate Hash Migrator: recomputing all certificates hash");
        let old_certificates = self
            .certificate_repository
            // arbitrary high value to get all existing certificates
            .get_latest_certificates::<Certificate>(usize::MAX)
            .await?;
        let mut certificates_to_remove = vec![];

        let mut migrated_certificates = vec![];
        let mut old_and_new_hashes: HashMap<String, String> = HashMap::new();

        // 1 - Recompute all certificates hashes
        // Note: get_latest_certificates retrieve certificates from the earliest to the older,
        // in order to have a strong guarantee that when inserting a certificate in the db its
        // previous_hash exist we have to work in the reverse order.
        debug!("🔧 Certificate Hash Migrator: computing new hash for all certificates");
        for mut certificate in old_certificates.into_iter().rev() {
            let old_previous_hash = if certificate.is_genesis() {
                certificate.previous_hash.clone()
            } else {
                let old_previous_hash = certificate.previous_hash.clone();
                old_and_new_hashes
                    .get(&certificate.previous_hash)
                    .ok_or(anyhow!(
                        "Could not migrate certificate previous_hash: The hash '{}' doesn't exist in the certificate table",
                        &certificate.previous_hash
                    ))?.clone_into(&mut certificate.previous_hash);

                old_previous_hash
            };

            if let Some(new_hash) = {
                let computed_hash = certificate.compute_hash();
                // return none if the hash did not change
                (computed_hash != certificate.hash).then_some(computed_hash)
            } {
                old_and_new_hashes.insert(certificate.hash.clone(), new_hash.clone());

                if certificate.is_genesis() {
                    trace!(
                        "🔧 Certificate Hash Migrator: new hash computed for genesis certificate {:?}",
                        certificate.signed_entity_type();
                        "old_hash" => &certificate.hash,
                        "new_hash" => &new_hash,
                    );
                } else {
                    trace!(
                        "🔧 Certificate Hash Migrator: new hash computed for certificate {:?}",
                        certificate.signed_entity_type();
                        "old_hash" => &certificate.hash,
                        "new_hash" => &new_hash,
                        "old_previous_hash" => &old_previous_hash,
                        "new_previous_hash" => &certificate.previous_hash
                    );
                }

                certificates_to_remove.push(certificate.clone());
                certificate.hash = new_hash;
                migrated_certificates.push(certificate);
            } else {
                old_and_new_hashes.insert(certificate.hash.clone(), certificate.hash);
            }
        }

        // 2 - Certificates migrated, we can insert them in the db
        // (we do this by chunks in order to avoid reaching the limit of 32766 variables in a single query)
        debug!("🔧 Certificate Hash Migrator: inserting migrated certificates in the database");
        let migrated_certificates_chunk_size = 250;
        for migrated_certificates_chunk in
            migrated_certificates.chunks(migrated_certificates_chunk_size)
        {
            self.certificate_repository
            .create_many_certificates(migrated_certificates_chunk.to_owned())
            .await
            .with_context(|| {
                "Certificates Hash Migrator can not insert migrated certificates in the database"
            })?;
        }

        Ok((certificates_to_remove, old_and_new_hashes))
    }

    async fn update_signed_entities_certificate_hash(
        &self,
        old_and_new_certificate_hashes: HashMap<String, String>,
    ) -> StdResult<()> {
        info!("🔧 Certificate Hash Migrator: updating signed entities certificate ids");
        let old_hashes: Vec<&str> = old_and_new_certificate_hashes
            .keys()
            .map(|k| k.as_str())
            .collect();
        let mut records_to_migrate = self
            .signed_entity_storer
            .get_signed_entities_by_certificates_ids(&old_hashes)
            .await
            .with_context(||
                format!(
                    "Certificates Hash Migrator can not get signed entities by certificates ids with hashes: '{:?}'", old_hashes
                )
            )?;

        debug!("🔧 Certificate Hash Migrator: updating signed entities certificate_ids to new computed hash");
        for signed_entity_record in records_to_migrate.iter_mut() {
            let new_certificate_hash =
                old_and_new_certificate_hashes
                    .get(&signed_entity_record.certificate_id)
                    .ok_or( anyhow!(
                        "Migration Error: no migrated hash found for signed entity with certificate id: {}",
                        signed_entity_record.certificate_id
                    ))?
                    .to_owned();

            trace!(
                "🔧 Certificate Hash Migrator: migrating signed entity {} certificate hash computed for certificate",
                signed_entity_record.signed_entity_id;
                "old_certificate_hash" => &signed_entity_record.certificate_id,
                "new_certificate_hash" => &new_certificate_hash
            );
            signed_entity_record.certificate_id = new_certificate_hash;
        }

        debug!("🔧 Certificate Hash Migrator: updating migrated signed entities in the database");
        self.signed_entity_storer
            .update_signed_entities(records_to_migrate)
            .await
            .with_context(|| "Certificates Hash Migrator can not update signed entities")?;

        Ok(())
    }

    async fn cleanup(&self, old_certificates: Vec<Certificate>) -> StdResult<()> {
        info!("🔧 Certificate Hash Migrator: deleting old certificates in the database");
        self.certificate_repository
            .delete_certificates(&old_certificates.iter().collect::<Vec<_>>())
            .await
            .with_context(|| {
                "Certificates Hash Migrator can not delete old certificates in the database"
            })?;

        Ok(())
    }
}

#[cfg(test)]
mod test {
    use mithril_common::entities::{
        Epoch, ImmutableFileNumber, SignedEntityConfig, SignedEntityType,
        SignedEntityTypeDiscriminants as Type, TimePoint,
    };
    use mithril_persistence::sqlite::{ConnectionBuilder, ConnectionOptions, SqliteConnection};

    use crate::database::record::{CertificateRecord, SignedEntityRecord};
    use crate::database::repository::SignedEntityStore;

    use super::*;

    fn connection_with_foreign_key_support() -> SqliteConnection {
        ConnectionBuilder::open_memory()
            .with_migrations(crate::database::migration::get_migrations())
            .with_options(&[ConnectionOptions::EnableForeignKeys])
            .build()
            .unwrap()
    }

    fn connection_without_foreign_key_support() -> SqliteConnection {
        ConnectionBuilder::open_memory()
            .with_migrations(crate::database::migration::get_migrations())
            .with_options(&[ConnectionOptions::ForceDisableForeignKeys])
            .build()
            .unwrap()
    }

    /// Note: If we want to create CardanoTransaction test certificate then another method
    /// that take a ChainPoint as parameter should be created.
    fn time_at(epoch: u64, immutable_file_number: ImmutableFileNumber) -> TimePoint {
        TimePoint {
            epoch: Epoch(epoch),
            immutable_file_number,
            ..TimePoint::dummy()
        }
    }

    fn dummy_genesis(certificate_hash: &str, time_point: TimePoint) -> Certificate {
        let certificate = CertificateRecord::dummy_genesis(
            certificate_hash,
            time_point.epoch,
            time_point.immutable_file_number,
        );

        certificate.into()
    }

    fn dummy_certificate(
        certificate_hash: &str,
        previous_hash: &str,
        time_point: TimePoint,
        signed_entity_type: Type,
    ) -> Certificate {
        let certificate = CertificateRecord::dummy(
            certificate_hash,
            previous_hash,
            time_point.epoch,
            time_point.immutable_file_number,
            SignedEntityConfig::dummy()
                .time_point_to_signed_entity(signed_entity_type, &time_point),
        );

        certificate.into()
    }

    fn signed_entity_for_certificate(certificate: &Certificate) -> Option<SignedEntityRecord> {
        match certificate.is_genesis() {
            true => None,
            false => {
                let signed_entity_type = certificate.signed_entity_type();
                // Note: we don't need to have real artifacts for those tests
                let artifact = format!("{signed_entity_type:?}");
                let id = match &signed_entity_type {
                    SignedEntityType::MithrilStakeDistribution(epoch) => {
                        format!("mithril-stake-distribution-{epoch}")
                    }
                    SignedEntityType::CardanoStakeDistribution(epoch) => {
                        format!("cardano-stake-distribution-{epoch}")
                    }
                    SignedEntityType::CardanoImmutableFilesFull(beacon) => {
                        format!("snapshot-{}-{}", beacon.epoch, beacon.immutable_file_number)
                    }
                    SignedEntityType::CardanoTransactions(epoch, block_number) => {
                        format!("cardano-transactions-{epoch}-{block_number}",)
                    }
                };

                let signed_entity_record = SignedEntityRecord {
                    signed_entity_id: format!("signed-entity-{id}",),
                    certificate_id: certificate.hash.clone(),
                    signed_entity_type,
                    artifact,
                    created_at: Default::default(),
                };

                Some(signed_entity_record)
            }
        }
    }

    async fn fill_certificates_and_signed_entities_in_db(
        connection: Arc<SqliteConnection>,
        certificates: &[Certificate],
    ) -> StdResult<Vec<(Certificate, Option<SignedEntityRecord>)>> {
        let certificate_repository: CertificateRepository =
            CertificateRepository::new(connection.clone());
        let signed_entity_store = SignedEntityStore::new(connection.clone());
        let mut result = vec![];

        for certificate in certificates.iter().cloned() {
            certificate_repository
                .create_certificate(certificate.clone())
                .await
                .with_context(|| {
                    format!(
                        "Certificates Hash Migrator can not create certificate with hash: '{}'",
                        certificate.hash
                    )
                })?;

            let signed_entity_maybe = signed_entity_for_certificate(&certificate);
            if let Some(record) = &signed_entity_maybe {
                signed_entity_store
                    .store_signed_entity(record)
                    .await
                    .with_context(|| "Certificates Hash Migrator can not store signed entity")?;
            }

            result.push((certificate.clone(), signed_entity_maybe));
        }

        Ok(result)
    }

    fn recompute_hashes(
        certificates_and_signed_entity: Vec<(Certificate, Option<SignedEntityRecord>)>,
    ) -> Vec<(Certificate, Option<SignedEntityRecord>)> {
        let mut old_and_new_hashes: HashMap<String, String> = HashMap::new();
        let mut result = vec![];

        for (mut certificate, signed_entity_maybe) in certificates_and_signed_entity {
            if let Some(hash) = old_and_new_hashes.get(&certificate.previous_hash) {
                certificate.previous_hash.clone_from(hash);
            }

            let new_hash = certificate.compute_hash();
            old_and_new_hashes.insert(certificate.hash.clone(), new_hash.clone());
            certificate.hash = new_hash;

            let signed_entity_maybe = match signed_entity_maybe {
                None => None,
                Some(mut signed_entity) => {
                    signed_entity.certificate_id.clone_from(&certificate.hash);
                    Some(signed_entity)
                }
            };

            result.push((certificate, signed_entity_maybe));
        }

        result
    }

    #[test]
    fn ensure_test_framework_recompute_correct_hashes() {
        let old_certificates: Vec<(Certificate, Option<SignedEntityRecord>)> = vec![
            dummy_genesis("genesis", time_at(1, 1)),
            dummy_certificate(
                "cert1",
                "genesis",
                time_at(1, 2),
                Type::MithrilStakeDistribution,
            ),
            dummy_certificate(
                "cert2",
                "cert1",
                time_at(2, 3),
                Type::MithrilStakeDistribution,
            ),
        ]
        .into_iter()
        .map(|cert| (cert.clone(), signed_entity_for_certificate(&cert)))
        .collect();

        let expected: Vec<(Certificate, Option<SignedEntityRecord>)> = vec![
            dummy_genesis(
                "328b1ac75ef18fe09ff542ea1997ee512cd62c886a260463034e551255ad39e0",
                time_at(1, 1),
            ),
            dummy_certificate(
                "007286af724bb132dab1f13f9cda8a86d0cd82173f0b4a91124cc7bff63b1562",
                "328b1ac75ef18fe09ff542ea1997ee512cd62c886a260463034e551255ad39e0",
                time_at(1, 2),
                Type::MithrilStakeDistribution,
            ),
            dummy_certificate(
                "98fb51c4588293acec548c4d35e499fe77e6eb2eb75c67d64a1026a6f88bad7b",
                "007286af724bb132dab1f13f9cda8a86d0cd82173f0b4a91124cc7bff63b1562",
                time_at(2, 3),
                Type::MithrilStakeDistribution,
            ),
        ]
        .into_iter()
        .map(|cert| (cert.clone(), signed_entity_for_certificate(&cert)))
        .collect();
        let recomputed = recompute_hashes(old_certificates);

        assert_eq!(expected, recomputed);
    }

    async fn get_certificates_and_signed_entities(
        connection: Arc<SqliteConnection>,
    ) -> StdResult<Vec<(Certificate, Option<SignedEntityRecord>)>> {
        let mut result = vec![];
        let certificate_repository: CertificateRepository =
            CertificateRepository::new(connection.clone());
        let signed_entity_store = SignedEntityStore::new(connection.clone());

        let certificates = certificate_repository
            .get_latest_certificates::<Certificate>(usize::MAX)
            .await?;

        for certificate in certificates {
            if certificate.is_genesis() {
                result.push((certificate, None));
            } else {
                let record = signed_entity_store
                    .get_signed_entity_by_certificate_id(&certificate.hash)
                    .await
                    .with_context(|| format!("Certificates Hash Migrator can not get signed entity type by certificate id with hash: '{}'", certificate.hash))?;
                result.push((certificate, record));
            }
        }

        Ok(result)
    }

    async fn run_migration_test(
        sqlite_connection: Arc<SqliteConnection>,
        certificates: Vec<Certificate>,
    ) {
        // Arrange
        let old_certificates =
            fill_certificates_and_signed_entities_in_db(sqlite_connection.clone(), &certificates)
                .await
                .unwrap();

        // Note: data retrieved from the database will be in the earliest to the oldest order, the
        // reverse of our insert order.
        let expected_certificates_and_signed_entities = recompute_hashes(old_certificates)
            .into_iter()
            .rev()
            .collect();

        // Act
        let migrator = CertificatesHashMigrator::new(
            CertificateRepository::new(sqlite_connection.clone()),
            Arc::new(SignedEntityStore::new(sqlite_connection.clone())),
        );
        migrator
            .migrate()
            .await
            .expect("Certificates hash migration should not fail");

        // Assert
        let migrated_certificates_and_signed_entities =
            get_certificates_and_signed_entities(sqlite_connection)
                .await
                .unwrap();

        let extract_human_readable_data =
            |entries: Vec<(Certificate, Option<SignedEntityRecord>)>| {
                entries
                    .into_iter()
                    .map(|(cert, signed_entity)| {
                        (
                            cert.hash,
                            cert.previous_hash,
                            cert.epoch,
                            signed_entity.map(|s| (s.signed_entity_type, s.certificate_id)),
                        )
                    })
                    .collect::<Vec<_>>()
            };
        assert_eq!(
            extract_human_readable_data(expected_certificates_and_signed_entities),
            extract_human_readable_data(migrated_certificates_and_signed_entities)
        );
    }

    #[tokio::test]
    async fn migrate_genesis_certificate() {
        let connection = Arc::new(connection_without_foreign_key_support());
        run_migration_test(connection, vec![dummy_genesis("old_hash", time_at(1, 1))]).await;
    }

    #[tokio::test]
    async fn migrate_a_chain_of_one_genesis_and_one_mithril_stake_distribution() {
        let connection = Arc::new(connection_without_foreign_key_support());
        run_migration_test(
            connection,
            vec![
                dummy_genesis("old_genesis", time_at(1, 1)),
                dummy_certificate(
                    "old_hash_1",
                    "old_genesis",
                    time_at(1, 2),
                    Type::MithrilStakeDistribution,
                ),
            ],
        )
        .await;
    }

    #[tokio::test]
    async fn migrate_a_chain_with_one_genesis_spanning_multiple_epochs_and_multiple_signed_entities(
    ) {
        let connection = Arc::new(connection_with_foreign_key_support());
        run_migration_test(
            connection,
            vec![
                dummy_genesis("old_genesis", time_at(1, 1)),
                dummy_certificate(
                    "old_hash_1",
                    "old_genesis",
                    time_at(1, 2),
                    Type::MithrilStakeDistribution,
                ),
                dummy_certificate(
                    "old_hash_2",
                    "old_genesis",
                    time_at(1, 3),
                    Type::CardanoImmutableFilesFull,
                ),
                dummy_certificate(
                    "old_hash_3",
                    "old_hash_2",
                    time_at(2, 3),
                    Type::MithrilStakeDistribution,
                ),
                dummy_certificate(
                    "old_hash_4",
                    "old_hash_3",
                    time_at(2, 4),
                    Type::CardanoImmutableFilesFull,
                ),
                dummy_certificate(
                    "old_hash_5",
                    "old_hash_3",
                    time_at(3, 5),
                    Type::CardanoImmutableFilesFull,
                ),
                dummy_certificate(
                    "old_hash_6",
                    "old_hash_5",
                    time_at(4, 6),
                    Type::CardanoImmutableFilesFull,
                ),
            ],
        )
        .await;
    }

    #[tokio::test]
    async fn migrate_a_chain_with_multiple_genesis_spanning_multiple_epochs() {
        let connection = Arc::new(connection_with_foreign_key_support());
        run_migration_test(
            connection,
            vec![
                dummy_genesis("old_genesis", time_at(1, 1)),
                dummy_certificate(
                    "old_hash_1",
                    "old_genesis",
                    time_at(1, 2),
                    Type::MithrilStakeDistribution,
                ),
                dummy_certificate(
                    "old_hash_2",
                    "old_genesis",
                    time_at(1, 3),
                    Type::CardanoImmutableFilesFull,
                ),
                dummy_genesis("old_genesis_2", time_at(3, 5)),
                dummy_certificate(
                    "old_hash_3",
                    "old_genesis_2",
                    time_at(4, 6),
                    Type::MithrilStakeDistribution,
                ),
                dummy_certificate(
                    "old_hash_4",
                    "old_hash_3",
                    time_at(5, 7),
                    Type::CardanoImmutableFilesFull,
                ),
                dummy_genesis("old_genesis_3", time_at(5, 7)),
                dummy_certificate(
                    "old_hash_5",
                    "old_genesis_3",
                    time_at(5, 8),
                    Type::MithrilStakeDistribution,
                ),
            ],
        )
        .await;
    }

    #[tokio::test]
    async fn should_not_fail_if_some_hash_dont_change() {
        let connection = Arc::new(connection_without_foreign_key_support());
        let certificate = {
            let mut cert = dummy_genesis("whatever", time_at(1, 2));
            cert.hash = cert.compute_hash();
            cert
        };
        fill_certificates_and_signed_entities_in_db(connection.clone(), &[certificate])
            .await
            .unwrap();

        let migrator = CertificatesHashMigrator::new(
            CertificateRepository::new(connection.clone()),
            Arc::new(SignedEntityStore::new(connection.clone())),
        );
        migrator
            .migrate()
            .await
            .expect("Migration should not fail if a hash doesn't change");
    }
}
