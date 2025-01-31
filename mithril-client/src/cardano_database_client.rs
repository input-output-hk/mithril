//! A client to retrieve Cardano databases data from an Aggregator.
//!
//! In order to do so it defines a [CardanoDatabaseClient] which exposes the following features:
//!  - [get][CardanoDatabaseClient::get]: get a Cardano database data from its hash
//!  - [list][CardanoDatabaseClient::list]: get the list of available Cardano database
//!
//! # Get a Cardano database
//!
//! To get a Cardano database using the [ClientBuilder][crate::client::ClientBuilder].
//!
//! ```no_run
//! # async fn run() -> mithril_client::MithrilResult<()> {
//! use mithril_client::ClientBuilder;
//!
//! let client = ClientBuilder::aggregator("YOUR_AGGREGATOR_ENDPOINT", "YOUR_GENESIS_VERIFICATION_KEY").build()?;
//! let cardano_database = client.cardano_database().get("CARDANO_DATABASE_HASH").await?.unwrap();
//!
//! println!(
//!     "Cardano database hash={}, merkle_root={}, immutable_file_number={:?}",
//!     cardano_database.hash,
//!     cardano_database.merkle_root,
//!     cardano_database.beacon.immutable_file_number
//! );
//! #    Ok(())
//! # }
//! ```
//!
//! # List available Cardano databases
//!
//! To list available Cardano databases using the [ClientBuilder][crate::client::ClientBuilder].
//!
//! ```no_run
//! # async fn run() -> mithril_client::MithrilResult<()> {
//! use mithril_client::ClientBuilder;
//!
//! let client = ClientBuilder::aggregator("YOUR_AGGREGATOR_ENDPOINT", "YOUR_GENESIS_VERIFICATION_KEY").build()?;
//! let cardano_databases = client.cardano_database().list().await?;
//!
//! for cardano_database in cardano_databases {
//!     println!("Cardano database hash={}, immutable_file_number={}", cardano_database.hash, cardano_database.beacon.immutable_file_number);
//! }
//! #    Ok(())
//! # }
//! ```

// TODO: reorganize the imports
#[cfg(feature = "fs")]
use std::fs;
#[cfg(feature = "fs")]
use std::ops::RangeInclusive;
#[cfg(feature = "fs")]
use std::path::{Path, PathBuf};
use std::{collections::HashSet, sync::Arc};

#[cfg(feature = "fs")]
use anyhow::anyhow;
use anyhow::Context;
#[cfg(feature = "fs")]
use slog::Logger;

#[cfg(feature = "fs")]
use mithril_common::{
    entities::{
        AncillaryLocation, CompressionAlgorithm, DigestLocation, ImmutableFileNumber,
        ImmutablesLocation,
    },
    StdResult,
};

use crate::{
    aggregator_client::{AggregatorClient, AggregatorClientError, AggregatorRequest},
    file_downloader::{FileDownloaderResolver, FileDownloaderUri},
};
use crate::{CardanoDatabaseSnapshot, CardanoDatabaseSnapshotListItem, MithrilResult};

cfg_fs! {
    /// Immutable file range representation
    #[derive(Debug)]
    pub enum ImmutableFileRange {
        /// From the first (included) to the last immutable file number (included)
        Full,

        /// From a specific immutable file number (included) to the last immutable file number (included)
        From(ImmutableFileNumber),

        /// From a specific immutable file number (included) to another specific immutable file number (included)
        Range(ImmutableFileNumber, ImmutableFileNumber),

        /// From the first immutable file number (included) up to a specific immutable file number (included)
        UpTo(ImmutableFileNumber),
    }

    impl ImmutableFileRange {
        /// Returns the range of immutable file numbers
        pub fn to_range_inclusive(
            &self,
            last_immutable_file_number: ImmutableFileNumber,
        ) -> StdResult<RangeInclusive<ImmutableFileNumber>> {
            // TODO: first immutable file is 0 on devnet and 1 otherwise. To be handled properly
            let first_immutable_file_number = 0 as ImmutableFileNumber;
            let full_range = first_immutable_file_number..=last_immutable_file_number;

            match self {
                ImmutableFileRange::Full => Ok(full_range),
                ImmutableFileRange::From(from) if full_range.contains(from) => {
                    Ok(*from..=last_immutable_file_number)
                }
                ImmutableFileRange::Range(from, to)
                    if full_range.contains(from)
                        && full_range.contains(to)
                        && !(*from..=*to).is_empty() =>
                {
                    Ok(*from..=*to)
                }
                ImmutableFileRange::UpTo(to) if full_range.contains(to) => {
                    Ok(first_immutable_file_number..=*to)
                }
                _ => Err(anyhow!("Invalid immutable file range: {self:?}")),
            }
        }
    }

    /// Options for downloading and unpacking a Cardano database
    #[derive(Debug,Default)]
    pub struct DownloadUnpackOptions {
        /// Allow overriding the destination directory
        pub allow_override: bool,

        /// Include ancillary files in the download
        pub include_ancillary: bool
    }
}

/// HTTP client for CardanoDatabase API from the Aggregator
pub struct CardanoDatabaseClient {
    aggregator_client: Arc<dyn AggregatorClient>,
    #[cfg(feature = "fs")]
    immutable_file_downloader_resolver: Arc<dyn FileDownloaderResolver<ImmutablesLocation>>,
    #[cfg(feature = "fs")]
    digest_file_downloader_resolver: Arc<dyn FileDownloaderResolver<DigestLocation>>,
    #[cfg(feature = "fs")]
    logger: Logger,
}

impl CardanoDatabaseClient {
    /// Constructs a new `CardanoDatabase`.
    pub fn new(
        aggregator_client: Arc<dyn AggregatorClient>,
        #[cfg(feature = "fs")] immutable_file_downloader_resolver: Arc<
            dyn FileDownloaderResolver<ImmutablesLocation>,
        >,
        #[cfg(feature = "fs")] digest_file_downloader_resolver: Arc<
            dyn FileDownloaderResolver<DigestLocation>,
        >,
        #[cfg(feature = "fs")] logger: Logger,
    ) -> Self {
        Self {
            aggregator_client,
            #[cfg(feature = "fs")]
            immutable_file_downloader_resolver,
            #[cfg(feature = "fs")]
            digest_file_downloader_resolver,
            #[cfg(feature = "fs")]
            logger: mithril_common::logging::LoggerExtensions::new_with_component_name::<Self>(
                &logger,
            ),
        }
    }

    /// Fetch a list of signed CardanoDatabase
    pub async fn list(&self) -> MithrilResult<Vec<CardanoDatabaseSnapshotListItem>> {
        let response = self
            .aggregator_client
            .get_content(AggregatorRequest::ListCardanoDatabaseSnapshots)
            .await
            .with_context(|| "CardanoDatabase client can not get the artifact list")?;
        let items = serde_json::from_str::<Vec<CardanoDatabaseSnapshotListItem>>(&response)
            .with_context(|| "CardanoDatabase client can not deserialize artifact list")?;

        Ok(items)
    }

    /// Get the given Cardano database data by hash.
    pub async fn get(&self, hash: &str) -> MithrilResult<Option<CardanoDatabaseSnapshot>> {
        self.fetch_with_aggregator_request(AggregatorRequest::GetCardanoDatabaseSnapshot {
            hash: hash.to_string(),
        })
        .await
    }

    /// Fetch the given Cardano database data with an aggregator request.
    /// If it cannot be found, a None is returned.
    async fn fetch_with_aggregator_request(
        &self,
        request: AggregatorRequest,
    ) -> MithrilResult<Option<CardanoDatabaseSnapshot>> {
        match self.aggregator_client.get_content(request).await {
            Ok(content) => {
                let cardano_database: CardanoDatabaseSnapshot = serde_json::from_str(&content)
                    .with_context(|| "CardanoDatabase client can not deserialize artifact")?;

                Ok(Some(cardano_database))
            }
            Err(AggregatorClientError::RemoteServerLogical(_)) => Ok(None),
            Err(e) => Err(e.into()),
        }
    }

    cfg_fs! {
        /// Download and unpack the given Cardano database part data by hash.
        // TODO: Add example in module documentation
        pub async fn download_unpack(
            &self,
            hash: &str,
            immutable_file_range: ImmutableFileRange,
            target_dir: &Path,
            download_unpack_options: DownloadUnpackOptions,
        ) -> StdResult<()> {
            let cardano_database_snapshot = self
                .get(hash)
                .await?
                .ok_or_else(|| anyhow!("Cardano database snapshot not found"))?;
            let compression_algorithm = cardano_database_snapshot.compression_algorithm;
            let last_immutable_file_number = cardano_database_snapshot.beacon.immutable_file_number;
            let immutable_file_number_range =
                immutable_file_range.to_range_inclusive(last_immutable_file_number)?;

            self.verify_can_write_to_target_directory(target_dir, &download_unpack_options)?;
            self.create_target_directory_sub_directories_if_not_exist(
                target_dir,
                &download_unpack_options,
            )?;

            let immutable_locations = cardano_database_snapshot.locations.immutables;
            self.download_unpack_immutable_files(
                &immutable_locations,
                immutable_file_number_range,
                &compression_algorithm,
                &Self::immutable_files_target_dir(target_dir),
            )
            .await?;

            let digest_locations = cardano_database_snapshot.locations.digests;
            self.download_unpack_digest_file(&digest_locations, &Self::digest_target_dir())
                .await?;

            Ok(())
        }

        fn digest_target_dir() -> PathBuf {
            std::env::temp_dir()
                .join("mithril")
                .join("cardano_database_client")
                .join("digest")
        }

        fn immutable_files_target_dir(target_dir: &Path) -> PathBuf {
            target_dir.join("immutable")
        }

        fn volatile_target_dir(target_dir: &Path) -> PathBuf {
            target_dir.join("volatile")
        }

        fn ledger_target_dir(target_dir: &Path) -> PathBuf {
            target_dir.join("ledger")
        }

        fn create_directory_if_not_exists(dir: &Path) -> StdResult<()> {
            if dir.exists() {
                return Ok(());
            }

            fs::create_dir_all(dir).map_err(|e| anyhow!("Failed creating directory: {e}"))
        }

        /// Verify if the target directory is writable.
        fn verify_can_write_to_target_directory(
            &self,
            target_dir: &Path,
            download_unpack_options: &DownloadUnpackOptions,
        ) -> StdResult<()> {
            let immutable_files_target_dir = Self::immutable_files_target_dir(target_dir);
            let volatile_target_dir = Self::volatile_target_dir(target_dir);
            let ledger_target_dir = Self::ledger_target_dir(target_dir);
            if !download_unpack_options.allow_override {
                if immutable_files_target_dir.exists() {
                    return Err(anyhow!(
                        "Immutable files target directory already exists in: {target_dir:?}"
                    ));
                }
                if download_unpack_options.include_ancillary {
                    if volatile_target_dir.exists() {
                        return Err(anyhow!(
                            "Volatile target directory already exists in: {target_dir:?}"
                        ));
                    }
                    if ledger_target_dir.exists() {
                        return Err(anyhow!(
                            "Ledger target directory already exists in: {target_dir:?}"
                        ));
                    }
                }
            }

            Ok(())
        }

        /// Create the target directory sub-directories if they do not exist.
        // TODO: is it really needed?
        fn create_target_directory_sub_directories_if_not_exist(
            &self,
            target_dir: &Path,
            download_unpack_options: &DownloadUnpackOptions,
        ) -> StdResult<()> {
            let immutable_files_target_dir = Self::immutable_files_target_dir(target_dir);
            Self::create_directory_if_not_exists(&immutable_files_target_dir)?;
            if download_unpack_options.include_ancillary {
                let volatile_target_dir = Self::volatile_target_dir(target_dir);
                let ledger_target_dir = Self::ledger_target_dir(target_dir);
                Self::create_directory_if_not_exists(&volatile_target_dir)?;
                Self::create_directory_if_not_exists(&ledger_target_dir)?;
            }

            Ok(())
        }

        /// Download and unpack the immutable files of the given range.
        ///
        /// The download is attempted for each location until the full range is downloaded.
        /// An error is returned if not all the files are downloaded.
        // TODO: Add feedback receivers
        async fn download_unpack_immutable_files(
            &self,
            locations: &[ImmutablesLocation],
            range: RangeInclusive<ImmutableFileNumber>,
            compression_algorithm: &CompressionAlgorithm,
            immutable_files_target_dir: &Path,
        ) -> StdResult<()> {
            let mut locations_sorted = locations.to_owned();
            locations_sorted.sort();
            let mut immutable_file_numbers_to_download =
                range.clone().map(|n| n.to_owned()).collect::<HashSet<_>>();
            for location in locations_sorted {
                let file_downloader = self
                    .immutable_file_downloader_resolver
                    .resolve(&location)
                    .ok_or_else(|| {
                        anyhow!("Failed resolving a file downloader for location: {location:?}")
                    })?;
                let file_downloader_uris =
                    FileDownloaderUri::expand_immutable_files_location_to_file_downloader_uris(
                        &location,
                        immutable_file_numbers_to_download
                            .clone()
                            .into_iter()
                            .collect::<Vec<_>>()
                            .as_slice(),
                    )?;
                for (immutable_file_number, file_downloader_uri) in file_downloader_uris {
                    let download_id = format!("{location:?}"); //TODO: check if this is the correct way to format the download_id
                    let downloaded = file_downloader
                        .download_unpack(
                            &file_downloader_uri,
                            immutable_files_target_dir,
                            Some(compression_algorithm.to_owned()),
                            &download_id,
                        )
                        .await;
                    match downloaded {
                        Ok(_) => {
                            immutable_file_numbers_to_download.remove(&immutable_file_number);
                        }
                        Err(e) => {
                            slog::error!(
                                self.logger,
                                "Failed downloading and unpacking immutable files for location {file_downloader_uri:?}"; "error" => e.to_string()
                            );
                        }
                    }
                }
                if immutable_file_numbers_to_download.is_empty() {
                    return Ok(());
                }
            }

            Err(anyhow!(
                "Failed downloading and unpacking immutable files for locations: {locations:?}"
            ))
        }

        async fn download_unpack_digest_file(
            &self,
            locations: &[DigestLocation],
            digest_file_target_dir: &Path,
        ) -> StdResult<()> {
            let mut locations_sorted = locations.to_owned();
            locations_sorted.sort();
            for location in locations_sorted {
                let download_id = format!("{location:?}"); //TODO: check if this is the correct way to format the download_id
                let file_downloader = self
                    .digest_file_downloader_resolver
                    .resolve(&location)
                    .ok_or_else(|| {
                        anyhow!("Failed resolving a file downloader for location: {location:?}")
                    })?;
                let file_downloader_uri: FileDownloaderUri = location.into();
                let downloaded = file_downloader
                    .download_unpack(
                        &file_downloader_uri,
                        digest_file_target_dir,
                        None,
                        &download_id,
                    )
                    .await;
                match downloaded {
                    Ok(_) => return Ok(()),
                    Err(e) => {
                        slog::error!(
                            self.logger,
                            "Failed downloading and unpacking digest for location {file_downloader_uri:?}"; "error" => e.to_string()
                        );
                    }
                }
            }

            Err(anyhow!(
                "Failed downloading and unpacking digests for all locations"
            ))
        }
    }
}

#[cfg(test)]
mod tests {

    use super::*;

    mod cardano_database_client {
        use anyhow::anyhow;
        use chrono::{DateTime, Utc};
        use mithril_common::entities::{
            CardanoDbBeacon, CompressionAlgorithm, DigestLocationDiscriminants, Epoch,
            ImmutablesLocationDiscriminants,
        };
        use mockall::predicate::eq;

        use crate::{
            aggregator_client::MockAggregatorHTTPClient,
            file_downloader::{
                DigestFileDownloaderResolver, FileDownloader, ImmutablesFileDownloaderResolver,
            },
            test_utils,
        };

        use super::*;

        fn fake_messages() -> Vec<CardanoDatabaseSnapshotListItem> {
            vec![
                CardanoDatabaseSnapshotListItem {
                    hash: "hash-123".to_string(),
                    merkle_root: "mkroot-123".to_string(),
                    beacon: CardanoDbBeacon {
                        epoch: Epoch(1),
                        immutable_file_number: 123,
                    },
                    certificate_hash: "cert-hash-123".to_string(),
                    total_db_size_uncompressed: 800796318,
                    created_at: DateTime::parse_from_rfc3339("2025-01-19T13:43:05.618857482Z")
                        .unwrap()
                        .with_timezone(&Utc),
                    compression_algorithm: CompressionAlgorithm::default(),
                    cardano_node_version: "0.0.1".to_string(),
                },
                CardanoDatabaseSnapshotListItem {
                    hash: "hash-456".to_string(),
                    merkle_root: "mkroot-456".to_string(),
                    beacon: CardanoDbBeacon {
                        epoch: Epoch(2),
                        immutable_file_number: 456,
                    },
                    certificate_hash: "cert-hash-456".to_string(),
                    total_db_size_uncompressed: 2960713808,
                    created_at: DateTime::parse_from_rfc3339("2025-01-27T15:22:05.618857482Z")
                        .unwrap()
                        .with_timezone(&Utc),
                    compression_algorithm: CompressionAlgorithm::default(),
                    cardano_node_version: "0.0.1".to_string(),
                },
            ]
        }

        struct CardanoDatabaseClientDependencyInjector {
            http_client: MockAggregatorHTTPClient,
            immutable_file_downloader_resolver: ImmutablesFileDownloaderResolver,
            digest_file_downloader_resolver: DigestFileDownloaderResolver,
        }

        impl CardanoDatabaseClientDependencyInjector {
            fn new() -> Self {
                Self {
                    http_client: MockAggregatorHTTPClient::new(),
                    immutable_file_downloader_resolver: ImmutablesFileDownloaderResolver::new(
                        vec![],
                    ),
                    digest_file_downloader_resolver: DigestFileDownloaderResolver::new(vec![]),
                }
            }

            fn with_http_client_mock_config<F>(mut self, config: F) -> Self
            where
                F: FnOnce(&mut MockAggregatorHTTPClient),
            {
                config(&mut self.http_client);

                self
            }

            fn with_immutable_file_downloaders(
                self,
                file_downloaders: Vec<(ImmutablesLocationDiscriminants, Arc<dyn FileDownloader>)>,
            ) -> Self {
                let immutable_file_downloader_resolver =
                    ImmutablesFileDownloaderResolver::new(file_downloaders);

                Self {
                    immutable_file_downloader_resolver,
                    ..self
                }
            }

            fn with_digest_file_downloaders(
                self,
                file_downloaders: Vec<(DigestLocationDiscriminants, Arc<dyn FileDownloader>)>,
            ) -> Self {
                let digest_file_downloader_resolver =
                    DigestFileDownloaderResolver::new(file_downloaders);

                Self {
                    digest_file_downloader_resolver,
                    ..self
                }
            }

            fn build_cardano_database_client(self) -> CardanoDatabaseClient {
                CardanoDatabaseClient::new(
                    Arc::new(self.http_client),
                    Arc::new(self.immutable_file_downloader_resolver),
                    Arc::new(self.digest_file_downloader_resolver),
                    test_utils::test_logger(),
                )
            }
        }

        mod list {
            use super::*;

            #[tokio::test]
            async fn list_cardano_database_snapshots_returns_messages() {
                let message = fake_messages();
                let client = CardanoDatabaseClientDependencyInjector::new()
                    .with_http_client_mock_config(|http_client| {
                        http_client
                            .expect_get_content()
                            .with(eq(AggregatorRequest::ListCardanoDatabaseSnapshots))
                            .return_once(move |_| Ok(serde_json::to_string(&message).unwrap()));
                    })
                    .build_cardano_database_client();

                let messages = client.list().await.unwrap();

                assert_eq!(2, messages.len());
                assert_eq!("hash-123".to_string(), messages[0].hash);
                assert_eq!("hash-456".to_string(), messages[1].hash);
            }

            #[tokio::test]
            async fn list_cardano_database_snapshots_returns_error_when_invalid_json_structure_in_response(
            ) {
                let client = CardanoDatabaseClientDependencyInjector::new()
                    .with_http_client_mock_config(|http_client| {
                        http_client
                            .expect_get_content()
                            .return_once(move |_| Ok("invalid json structure".to_string()));
                    })
                    .build_cardano_database_client();

                client
                    .list()
                    .await
                    .expect_err("List Cardano databases should return an error");
            }
        }

        mod get {
            use super::*;

            #[tokio::test]
            async fn get_cardano_database_snapshot_returns_message() {
                let expected_cardano_database_snapshot = CardanoDatabaseSnapshot {
                    hash: "hash-123".to_string(),
                    ..CardanoDatabaseSnapshot::dummy()
                };
                let message = expected_cardano_database_snapshot.clone();
                let client = CardanoDatabaseClientDependencyInjector::new()
                    .with_http_client_mock_config(|http_client| {
                        http_client
                            .expect_get_content()
                            .with(eq(AggregatorRequest::GetCardanoDatabaseSnapshot {
                                hash: "hash-123".to_string(),
                            }))
                            .return_once(move |_| Ok(serde_json::to_string(&message).unwrap()));
                    })
                    .build_cardano_database_client();

                let cardano_database = client
                    .get("hash-123")
                    .await
                    .unwrap()
                    .expect("This test returns a Cardano database");

                assert_eq!(expected_cardano_database_snapshot, cardano_database);
            }

            #[tokio::test]
            async fn get_cardano_database_snapshot_returns_error_when_invalid_json_structure_in_response(
            ) {
                let client = CardanoDatabaseClientDependencyInjector::new()
                    .with_http_client_mock_config(|http_client| {
                        http_client
                            .expect_get_content()
                            .return_once(move |_| Ok("invalid json structure".to_string()));
                    })
                    .build_cardano_database_client();

                client
                    .get("hash-123")
                    .await
                    .expect_err("Get Cardano database should return an error");
            }

            #[tokio::test]
            async fn get_cardano_database_snapshot_returns_none_when_not_found_or_remote_server_logical_error(
            ) {
                let client = CardanoDatabaseClientDependencyInjector::new()
                    .with_http_client_mock_config(|http_client| {
                        http_client.expect_get_content().return_once(move |_| {
                            Err(AggregatorClientError::RemoteServerLogical(anyhow!(
                                "not found"
                            )))
                        });
                    })
                    .build_cardano_database_client();

                let result = client.get("hash-123").await.unwrap();

                assert!(result.is_none());
            }

            #[tokio::test]
            async fn get_cardano_database_snapshot_returns_error() {
                let client = CardanoDatabaseClientDependencyInjector::new()
                    .with_http_client_mock_config(|http_client| {
                        http_client.expect_get_content().return_once(move |_| {
                            Err(AggregatorClientError::SubsystemError(anyhow!("error")))
                        });
                    })
                    .build_cardano_database_client();

                client
                    .get("hash-123")
                    .await
                    .expect_err("Get Cardano database should return an error");
            }
        }

        cfg_fs! {
            mod download_unpack {
                use std::fs;
                use std::path::Path;

                use mithril_common::{
                    entities::{FileUri, ImmutablesLocationDiscriminants, MultiFilesUri, TemplateUri},
                    test_utils::TempDir,
                };
                use mockall::predicate;

                use crate::file_downloader::MockFileDownloader;

                use super::*;

                #[tokio::test]
                async fn download_unpack_fails_with_invalid_snapshot() {
                    let immutable_file_range = ImmutableFileRange::Range(1, 10);
                    let download_unpack_options = DownloadUnpackOptions::default();
                    let cardano_db_snapshot_hash = &"hash-123";
                    let target_dir = Path::new(".");
                    let client = CardanoDatabaseClientDependencyInjector::new()
                        .with_http_client_mock_config(|http_client| {
                            http_client.expect_get_content().return_once(move |_| {
                                Err(AggregatorClientError::RemoteServerLogical(anyhow!(
                                    "not found"
                                )))
                            });
                        })
                        .build_cardano_database_client();

                    client
                        .download_unpack(
                            cardano_db_snapshot_hash,
                            immutable_file_range,
                            target_dir,
                            download_unpack_options,
                        )
                        .await
                        .expect_err("download_unpack should fail");
                }

                #[tokio::test]
                async fn download_unpack_fails_with_invalid_immutable_file_range() {
                    let immutable_file_range = ImmutableFileRange::Range(1, 0);
                    let download_unpack_options = DownloadUnpackOptions::default();
                    let cardano_db_snapshot_hash = &"hash-123";
                    let target_dir = Path::new(".");
                    let client = CardanoDatabaseClientDependencyInjector::new()
                        .with_http_client_mock_config(|http_client| {
                            let message = CardanoDatabaseSnapshot {
                                hash: "hash-123".to_string(),
                                ..CardanoDatabaseSnapshot::dummy()
                            };
                            http_client
                                .expect_get_content()
                                .with(eq(AggregatorRequest::GetCardanoDatabaseSnapshot {
                                    hash: "hash-123".to_string(),
                                }))
                                .return_once(move |_| Ok(serde_json::to_string(&message).unwrap()));
                        })
                        .build_cardano_database_client();

                    client
                        .download_unpack(
                            cardano_db_snapshot_hash,
                            immutable_file_range,
                            target_dir,
                            download_unpack_options,
                        )
                        .await
                        .expect_err("download_unpack should fail");
                }

                #[tokio::test]
                async fn download_unpack_fails_when_immutable_files_download_fail() {
                    let total_immutable_files = 10;
                    let immutable_file_range = ImmutableFileRange::Range(1, total_immutable_files);
                    let download_unpack_options = DownloadUnpackOptions::default();
                    let cardano_db_snapshot_hash = &"hash-123";
                    let target_dir = TempDir::new(
                        "cardano_database_client",
                        "download_unpack_fails_when_immutable_files_download_fail",
                    )
                    .build();
                    let client = CardanoDatabaseClientDependencyInjector::new()
                        .with_http_client_mock_config(|http_client| {
                            let mut message = CardanoDatabaseSnapshot {
                                hash: "hash-123".to_string(),
                                ..CardanoDatabaseSnapshot::dummy()
                            };
                            message.locations.immutables = vec![ImmutablesLocation::CloudStorage {
                                uri: MultiFilesUri::Template(TemplateUri(
                                    "http://whatever/{immutable_file_number}.tar.gz".to_string(),
                                )),
                            }];
                            http_client
                                .expect_get_content()
                                .with(eq(AggregatorRequest::GetCardanoDatabaseSnapshot {
                                    hash: "hash-123".to_string(),
                                }))
                                .return_once(move |_| Ok(serde_json::to_string(&message).unwrap()));
                        })
                        .with_immutable_file_downloaders(vec![(
                            ImmutablesLocationDiscriminants::CloudStorage,
                            Arc::new({
                                let mut mock_file_downloader = MockFileDownloader::new();
                                mock_file_downloader
                                    .expect_download_unpack()
                                    .times(total_immutable_files as usize)
                                    .returning(|_, _, _, _| Err(anyhow!("Download failed")));

                                mock_file_downloader
                            }),
                        )])
                        .build_cardano_database_client();

                    client
                        .download_unpack(
                            cardano_db_snapshot_hash,
                            immutable_file_range,
                            &target_dir,
                            download_unpack_options,
                        )
                        .await
                        .expect_err("download_unpack should fail");
                }

                #[tokio::test]
                async fn download_unpack_fails_when_target_target_dir_would_be_overwritten_without_allow_override(
                ) {
                    let immutable_file_range = ImmutableFileRange::Range(1, 10);
                    let download_unpack_options = DownloadUnpackOptions::default();
                    let cardano_db_snapshot_hash = &"hash-123";
                    let target_dir = &TempDir::new(
                                        "cardano_database_client",
                                        "download_unpack_fails_when_target_target_dir_would_be_overwritten_without_allow_override",
                                    )
                                    .build();
                    fs::create_dir_all(target_dir.join("immutable")).unwrap();
                    let client = CardanoDatabaseClientDependencyInjector::new()
                        .with_http_client_mock_config(|http_client| {
                            let message = CardanoDatabaseSnapshot {
                                hash: "hash-123".to_string(),
                                ..CardanoDatabaseSnapshot::dummy()
                            };
                            http_client
                                .expect_get_content()
                                .with(eq(AggregatorRequest::GetCardanoDatabaseSnapshot {
                                    hash: "hash-123".to_string(),
                                }))
                                .return_once(move |_| Ok(serde_json::to_string(&message).unwrap()));
                        })
                        .build_cardano_database_client();

                    client
                        .download_unpack(
                            cardano_db_snapshot_hash,
                            immutable_file_range,
                            target_dir,
                            download_unpack_options,
                        )
                        .await
                        .expect_err("download_unpack should fail");
                }

                #[tokio::test]
                async fn download_unpack_succeeds_with_valid_range() {
                    let immutable_file_range = ImmutableFileRange::Range(1, 2);
                    let download_unpack_options = DownloadUnpackOptions::default();
                    let cardano_db_snapshot_hash = &"hash-123";
                    let target_dir = TempDir::new(
                        "cardano_database_client",
                        "download_unpack_succeeds_with_valid_range",
                    )
                    .build();
                    let mut message = CardanoDatabaseSnapshot {
                        hash: "hash-123".to_string(),
                        ..CardanoDatabaseSnapshot::dummy()
                    };
                    message.locations.immutables = vec![ImmutablesLocation::CloudStorage {
                        uri: MultiFilesUri::Template(TemplateUri(
                            "http://whatever/{immutable_file_number}.tar.gz".to_string(),
                        )),
                    }];
                    message.locations.digests = vec![DigestLocation::CloudStorage {
                        uri: "http://whatever/digest.txt".to_string(),
                    }];
                    let client = CardanoDatabaseClientDependencyInjector::new()
                        .with_http_client_mock_config(|http_client| {
                            http_client
                                .expect_get_content()
                                .with(eq(AggregatorRequest::GetCardanoDatabaseSnapshot {
                                    hash: "hash-123".to_string(),
                                }))
                                .return_once(move |_| Ok(serde_json::to_string(&message).unwrap()));
                        })
                        .with_immutable_file_downloaders(vec![(
                            ImmutablesLocationDiscriminants::CloudStorage,
                            Arc::new({
                                let mut mock_file_downloader = MockFileDownloader::new();
                                mock_file_downloader
                                    .expect_download_unpack()
                                    .with(
                                        eq(FileDownloaderUri::FileUri(FileUri(
                                            "http://whatever/00001.tar.gz".to_string(),
                                        ))),
                                        eq(target_dir.join("immutable")),
                                        eq(Some(CompressionAlgorithm::default())),
                                        predicate::always(),
                                    )
                                    .times(1)
                                    .returning(|_, _, _, _| Ok(()));
                                mock_file_downloader
                                    .expect_download_unpack()
                                    .with(
                                        eq(FileDownloaderUri::FileUri(FileUri(
                                            "http://whatever/00002.tar.gz".to_string(),
                                        ))),
                                        eq(target_dir.join("immutable")),
                                        eq(Some(CompressionAlgorithm::default())),
                                        predicate::always(),
                                    )
                                    .times(1)
                                    .returning(|_, _, _, _| Ok(()));

                                mock_file_downloader
                            }),
                        )])
                        .with_digest_file_downloaders(vec![(
                            DigestLocationDiscriminants::CloudStorage,
                            Arc::new({
                                let mut mock_file_downloader = MockFileDownloader::new();
                                mock_file_downloader
                                    .expect_download_unpack()
                                    .with(
                                        eq(FileDownloaderUri::FileUri(FileUri(
                                            "http://whatever/digest.txt".to_string(),
                                        ))),
                                        predicate::always(),
                                        eq(None),
                                        predicate::always(),
                                    )
                                    .times(1)
                                    .returning(|_, _, _, _| Ok(()));

                                mock_file_downloader
                            }),
                        )])
                        .build_cardano_database_client();

                    client
                        .download_unpack(
                            cardano_db_snapshot_hash,
                            immutable_file_range,
                            &target_dir,
                            download_unpack_options,
                        )
                        .await
                        .unwrap();
                }
            }

            mod verify_can_write_to_target_dir {
                use std::fs;

                use mithril_common::test_utils::TempDir;

                use super::*;

                #[test]
                fn verify_can_write_to_target_dir_always_succeeds_with_allow_overwrite() {
                    let target_dir = TempDir::new(
                        "cardano_database_client",
                        "verify_can_write_to_target_dir_always_succeeds_with_allow_overwrite",
                    )
                    .build();
                    let client =
                        CardanoDatabaseClientDependencyInjector::new().build_cardano_database_client();

                    client
                        .verify_can_write_to_target_directory(
                            &target_dir,
                            &DownloadUnpackOptions {
                                allow_override: true,
                                include_ancillary: false,
                            },
                        )
                        .unwrap();

                    fs::create_dir_all(CardanoDatabaseClient::immutable_files_target_dir(
                        &target_dir,
                    ))
                    .unwrap();
                    fs::create_dir_all(CardanoDatabaseClient::volatile_target_dir(&target_dir))
                        .unwrap();
                    fs::create_dir_all(CardanoDatabaseClient::ledger_target_dir(&target_dir)).unwrap();
                    client
                        .verify_can_write_to_target_directory(
                            &target_dir,
                            &DownloadUnpackOptions {
                                allow_override: true,
                                include_ancillary: false,
                            },
                        )
                        .unwrap();
                    client
                        .verify_can_write_to_target_directory(
                            &target_dir,
                            &DownloadUnpackOptions {
                                allow_override: true,
                                include_ancillary: true,
                            },
                        )
                        .unwrap();
                }

                #[test]
                fn verify_can_write_to_target_dir_fails_without_allow_overwrite_and_non_empty_immutable_target_dir(
                ) {
                    let target_dir = TempDir::new("cardano_database_client", "verify_can_write_to_target_dir_fails_without_allow_overwrite_and_non_empty_immutable_target_dir").build();
                    fs::create_dir_all(CardanoDatabaseClient::immutable_files_target_dir(
                        &target_dir,
                    ))
                    .unwrap();
                    let client =
                        CardanoDatabaseClientDependencyInjector::new().build_cardano_database_client();

                    client
                        .verify_can_write_to_target_directory(
                            &target_dir,
                            &DownloadUnpackOptions {
                                allow_override: false,
                                include_ancillary: false,
                            },
                        )
                        .expect_err("verify_can_write_to_target_dir should fail");

                    client
                        .verify_can_write_to_target_directory(
                            &target_dir,
                            &DownloadUnpackOptions {
                                allow_override: false,
                                include_ancillary: true,
                            },
                        )
                        .expect_err("verify_can_write_to_target_dir should fail");
                }

                #[test]
                fn verify_can_write_to_target_dir_fails_without_allow_overwrite_and_non_empty_ledger_target_dir(
                ) {
                    let target_dir = TempDir::new("cardano_database_client", "verify_can_write_to_target_dir_fails_without_allow_overwrite_and_non_empty_ledger_target_dir").build();
                    fs::create_dir_all(CardanoDatabaseClient::ledger_target_dir(&target_dir)).unwrap();
                    let client =
                        CardanoDatabaseClientDependencyInjector::new().build_cardano_database_client();

                    client
                        .verify_can_write_to_target_directory(
                            &target_dir,
                            &DownloadUnpackOptions {
                                allow_override: false,
                                include_ancillary: true,
                            },
                        )
                        .expect_err("verify_can_write_to_target_dir should fail");

                    client
                        .verify_can_write_to_target_directory(
                            &target_dir,
                            &DownloadUnpackOptions {
                                allow_override: false,
                                include_ancillary: false,
                            },
                        )
                        .unwrap();
                }

                #[test]
                fn verify_can_write_to_target_dir_fails_without_allow_overwrite_and_non_empty_volatile_target_dir(
                ) {
                    let target_dir = TempDir::new("cardano_database_client", "verify_can_write_to_target_dir_fails_without_allow_overwrite_and_non_empty_volatile_target_dir").build();
                    fs::create_dir_all(CardanoDatabaseClient::volatile_target_dir(&target_dir))
                        .unwrap();
                    let client =
                        CardanoDatabaseClientDependencyInjector::new().build_cardano_database_client();

                    client
                        .verify_can_write_to_target_directory(
                            &target_dir,
                            &DownloadUnpackOptions {
                                allow_override: false,
                                include_ancillary: true,
                            },
                        )
                        .expect_err("verify_can_write_to_target_dir should fail");

                    client
                        .verify_can_write_to_target_directory(
                            &target_dir,
                            &DownloadUnpackOptions {
                                allow_override: false,
                                include_ancillary: false,
                            },
                        )
                        .unwrap();
                }
            }

            mod create_target_directory_sub_directories_if_not_exist {
                use mithril_common::test_utils::TempDir;

                use super::*;

                #[test]
                fn create_target_directory_sub_directories_if_not_exist_without_ancillary() {
                    let target_dir = TempDir::new(
                        "cardano_database_client",
                        "create_target_directory_sub_directories_if_not_exist_without_ancillary",
                    )
                    .build();
                    let client =
                        CardanoDatabaseClientDependencyInjector::new().build_cardano_database_client();
                    assert!(!target_dir.join("immutable").exists());
                    assert!(!target_dir.join("volatile").exists());
                    assert!(!target_dir.join("ledger").exists());

                    client
                        .create_target_directory_sub_directories_if_not_exist(
                            &target_dir,
                            &DownloadUnpackOptions {
                                include_ancillary: false,
                                ..Default::default()
                            },
                        )
                        .unwrap();

                    assert!(target_dir.join("immutable").exists());
                    assert!(!target_dir.join("volatile").exists());
                    assert!(!target_dir.join("ledger").exists());
                }

                #[test]
                fn create_target_directory_sub_directories_if_not_exist_with_ancillary() {
                    let target_dir = TempDir::new(
                        "cardano_database_client",
                        "create_target_directory_sub_directories_if_not_exist_with_ancillary",
                    )
                    .build();
                    let client =
                        CardanoDatabaseClientDependencyInjector::new().build_cardano_database_client();
                    assert!(!target_dir.join("immutable").exists());
                    assert!(!target_dir.join("volatile").exists());
                    assert!(!target_dir.join("ledger").exists());

                    client
                        .create_target_directory_sub_directories_if_not_exist(
                            &target_dir,
                            &DownloadUnpackOptions {
                                include_ancillary: true,
                                ..Default::default()
                            },
                        )
                        .unwrap();

                    assert!(target_dir.join("immutable").exists());
                    assert!(target_dir.join("volatile").exists());
                    assert!(target_dir.join("ledger").exists());
                }
            }

            mod download_unpack_immutable_files {
                use mithril_common::{
                    entities::{FileUri, MultiFilesUri, TemplateUri},
                    test_utils::TempDir,
                };
                use mockall::predicate;

                use crate::file_downloader::MockFileDownloader;

                use super::*;

                #[tokio::test]
                async fn download_unpack_immutable_files_fails_if_one_is_not_retrieved() {
                    let total_immutable_files = 2;
                    let immutable_file_range = ImmutableFileRange::Range(1, total_immutable_files);
                    let target_dir = TempDir::new(
                        "cardano_database_client",
                        "download_unpack_immutable_files_succeeds",
                    )
                    .build();
                    let client = CardanoDatabaseClientDependencyInjector::new()
                        .with_immutable_file_downloaders(vec![(
                            ImmutablesLocationDiscriminants::CloudStorage,
                            Arc::new({
                                let mut mock_file_downloader = MockFileDownloader::new();
                                mock_file_downloader
                                    .expect_download_unpack()
                                    .times(1)
                                    .returning(|_, _, _, _| Err(anyhow!("Download failed")));
                                mock_file_downloader
                                    .expect_download_unpack()
                                    .times(1)
                                    .returning(|_, _, _, _| Ok(()));

                                mock_file_downloader
                            }),
                        )])
                        .build_cardano_database_client();

                    client
                        .download_unpack_immutable_files(
                            &[ImmutablesLocation::CloudStorage {
                                uri: MultiFilesUri::Template(TemplateUri(
                                    "http://whatever/{immutable_file_number}.tar.gz".to_string(),
                                )),
                            }],
                            immutable_file_range
                                .to_range_inclusive(total_immutable_files)
                                .unwrap(),
                            &CompressionAlgorithm::default(),
                            &target_dir,
                        )
                        .await
                        .expect_err("download_unpack_immutable_files should fail");
                }

                #[tokio::test]
                async fn download_unpack_immutable_files_succeeds_if_all_are_retrieved_with_same_location(
                ) {
                    let total_immutable_files = 2;
                    let immutable_file_range = ImmutableFileRange::Range(1, total_immutable_files);
                    let target_dir = TempDir::new(
                        "cardano_database_client",
                        "download_unpack_immutable_files_succeeds",
                    )
                    .build();
                    let client = CardanoDatabaseClientDependencyInjector::new()
                        .with_immutable_file_downloaders(vec![(
                            ImmutablesLocationDiscriminants::CloudStorage,
                            Arc::new({
                                let mut mock_file_downloader = MockFileDownloader::new();
                                mock_file_downloader
                                    .expect_download_unpack()
                                    .times(2)
                                    .returning(|_, _, _, _| Ok(()));

                                mock_file_downloader
                            }),
                        )])
                        .build_cardano_database_client();

                    client
                        .download_unpack_immutable_files(
                            &[ImmutablesLocation::CloudStorage {
                                uri: MultiFilesUri::Template(TemplateUri(
                                    "http://whatever-1/{immutable_file_number}.tar.gz".to_string(),
                                )),
                            }],
                            immutable_file_range
                                .to_range_inclusive(total_immutable_files)
                                .unwrap(),
                            &CompressionAlgorithm::default(),
                            &target_dir,
                        )
                        .await
                        .unwrap();
                }

                #[tokio::test]
                async fn download_unpack_immutable_files_succeeds_if_all_are_retrieved_with_different_locations(
                ) {
                    let total_immutable_files = 2;
                    let immutable_file_range = ImmutableFileRange::Range(1, total_immutable_files);
                    let target_dir = TempDir::new(
                        "cardano_database_client",
                        "download_unpack_immutable_files_succeeds",
                    )
                    .build();
                    let client = CardanoDatabaseClientDependencyInjector::new()
                        .with_immutable_file_downloaders(vec![(
                            ImmutablesLocationDiscriminants::CloudStorage,
                            Arc::new({
                                let mut mock_file_downloader = MockFileDownloader::new();
                                mock_file_downloader
                                    .expect_download_unpack()
                                    .with(
                                        eq(FileDownloaderUri::FileUri(FileUri(
                                            "http://whatever-1/00001.tar.gz".to_string(),
                                        ))),
                                        eq(target_dir.clone()),
                                        eq(Some(CompressionAlgorithm::default())),
                                        predicate::always(),
                                    )
                                    .times(1)
                                    .returning(|_, _, _, _| Err(anyhow!("Download failed")));
                                mock_file_downloader
                                    .expect_download_unpack()
                                    .with(
                                        eq(FileDownloaderUri::FileUri(FileUri(
                                            "http://whatever-1/00002.tar.gz".to_string(),
                                        ))),
                                        eq(target_dir.clone()),
                                        eq(Some(CompressionAlgorithm::default())),
                                        predicate::always(),
                                    )
                                    .times(1)
                                    .returning(|_, _, _, _| Ok(()));
                                mock_file_downloader
                                    .expect_download_unpack()
                                    .with(
                                        eq(FileDownloaderUri::FileUri(FileUri(
                                            "http://whatever-2/00001.tar.gz".to_string(),
                                        ))),
                                        eq(target_dir.clone()),
                                        eq(Some(CompressionAlgorithm::default())),
                                        predicate::always(),
                                    )
                                    .times(1)
                                    .returning(|_, _, _, _| Ok(()));

                                mock_file_downloader
                            }),
                        )])
                        .build_cardano_database_client();

                    client
                        .download_unpack_immutable_files(
                            &[
                                ImmutablesLocation::CloudStorage {
                                    uri: MultiFilesUri::Template(TemplateUri(
                                        "http://whatever-1/{immutable_file_number}.tar.gz".to_string(),
                                    )),
                                },
                                ImmutablesLocation::CloudStorage {
                                    uri: MultiFilesUri::Template(TemplateUri(
                                        "http://whatever-2/{immutable_file_number}.tar.gz".to_string(),
                                    )),
                                },
                            ],
                            immutable_file_range
                                .to_range_inclusive(total_immutable_files)
                                .unwrap(),
                            &CompressionAlgorithm::default(),
                            &target_dir,
                        )
                        .await
                        .unwrap();
                }
            }

            mod download_unpack_digest_file {

                use crate::file_downloader::MockFileDownloader;

                use super::*;

                #[tokio::test]
                async fn download_unpack_digest_file_fails_if_no_location_is_retrieved() {
                    let target_dir = Path::new(".");
                    let client = CardanoDatabaseClientDependencyInjector::new()
                        .with_digest_file_downloaders(vec![
                            (
                                DigestLocationDiscriminants::CloudStorage,
                                Arc::new({
                                    let mut mock_file_downloader = MockFileDownloader::new();
                                    mock_file_downloader
                                        .expect_download_unpack()
                                        .times(1)
                                        .returning(|_, _, _, _| Err(anyhow!("Download failed")));

                                    mock_file_downloader
                                }),
                            ),
                            (
                                DigestLocationDiscriminants::Aggregator,
                                Arc::new({
                                    let mut mock_file_downloader = MockFileDownloader::new();
                                    mock_file_downloader
                                        .expect_download_unpack()
                                        .times(1)
                                        .returning(|_, _, _, _| Err(anyhow!("Download failed")));

                                    mock_file_downloader
                                }),
                            ),
                        ])
                        .build_cardano_database_client();

                    client
                        .download_unpack_digest_file(
                            &[
                                DigestLocation::CloudStorage {
                                    uri: "http://whatever-1/digest.txt".to_string(),
                                },
                                DigestLocation::Aggregator {
                                    uri: "http://whatever-2/digest".to_string(),
                                },
                            ],
                            &target_dir,
                        )
                        .await
                        .expect_err("download_unpack_digest_file should fail");
                }

                #[tokio::test]
                async fn download_unpack_digest_file_succeeds_if_at_least_one_location_is_retrieved() {
                    let target_dir = Path::new(".");
                    let client = CardanoDatabaseClientDependencyInjector::new()
                        .with_digest_file_downloaders(vec![
                            (
                                DigestLocationDiscriminants::CloudStorage,
                                Arc::new({
                                    let mut mock_file_downloader = MockFileDownloader::new();
                                    mock_file_downloader
                                        .expect_download_unpack()
                                        .times(1)
                                        .returning(|_, _, _, _| Err(anyhow!("Download failed")));

                                    mock_file_downloader
                                }),
                            ),
                            (
                                DigestLocationDiscriminants::Aggregator,
                                Arc::new({
                                    let mut mock_file_downloader = MockFileDownloader::new();
                                    mock_file_downloader
                                        .expect_download_unpack()
                                        .times(1)
                                        .returning(|_, _, _, _| Ok(()));

                                    mock_file_downloader
                                }),
                            ),
                        ])
                        .build_cardano_database_client();

                    client
                        .download_unpack_digest_file(
                            &[
                                DigestLocation::CloudStorage {
                                    uri: "http://whatever-1/digest.txt".to_string(),
                                },
                                DigestLocation::Aggregator {
                                    uri: "http://whatever-2/digest".to_string(),
                                },
                            ],
                            &target_dir,
                        )
                        .await
                        .unwrap();
                }

                #[tokio::test]
                async fn download_unpack_digest_file_succeeds_when_first_location_is_retrieved() {
                    let target_dir = Path::new(".");
                    let client = CardanoDatabaseClientDependencyInjector::new()
                        .with_digest_file_downloaders(vec![
                            (
                                DigestLocationDiscriminants::CloudStorage,
                                Arc::new({
                                    let mut mock_file_downloader = MockFileDownloader::new();
                                    mock_file_downloader
                                        .expect_download_unpack()
                                        .times(1)
                                        .returning(|_, _, _, _| Ok(()));

                                    mock_file_downloader
                                }),
                            ),
                            (
                                DigestLocationDiscriminants::Aggregator,
                                Arc::new(MockFileDownloader::new()),
                            ),
                        ])
                        .build_cardano_database_client();

                    client
                        .download_unpack_digest_file(
                            &[
                                DigestLocation::CloudStorage {
                                    uri: "http://whatever-1/digest.txt".to_string(),
                                },
                                DigestLocation::Aggregator {
                                    uri: "http://whatever-2/digest".to_string(),
                                },
                            ],
                            &target_dir,
                        )
                        .await
                        .unwrap();
                }
            }
        }

        mod immutable_file_range {
            use super::*;

            #[test]
            fn to_range_inclusive_with_full() {
                let immutable_file_range = ImmutableFileRange::Full;
                let last_immutable_file_number = 10;

                let result = immutable_file_range
                    .to_range_inclusive(last_immutable_file_number)
                    .unwrap();
                assert_eq!(0..=10, result);
            }

            #[test]
            fn to_range_inclusive_with_from() {
                let immutable_file_range = ImmutableFileRange::From(5);

                let last_immutable_file_number = 10;
                let result = immutable_file_range
                    .to_range_inclusive(last_immutable_file_number)
                    .unwrap();
                assert_eq!(5..=10, result);

                let last_immutable_file_number = 3;
                immutable_file_range
                    .to_range_inclusive(last_immutable_file_number)
                    .expect_err("conversion to range inlusive should fail");
            }

            #[test]
            fn to_range_inclusive_with_range() {
                let immutable_file_range = ImmutableFileRange::Range(5, 8);

                let last_immutable_file_number = 10;
                let result = immutable_file_range
                    .to_range_inclusive(last_immutable_file_number)
                    .unwrap();
                assert_eq!(5..=8, result);

                let last_immutable_file_number = 7;
                immutable_file_range
                    .to_range_inclusive(last_immutable_file_number)
                    .expect_err("conversion to range inlusive should fail");

                let immutable_file_range = ImmutableFileRange::Range(10, 8);
                immutable_file_range
                    .to_range_inclusive(last_immutable_file_number)
                    .expect_err("conversion to range inlusive should fail");
            }

            #[test]
            fn to_range_inclusive_with_up_to() {
                let immutable_file_range = ImmutableFileRange::UpTo(8);

                let last_immutable_file_number = 10;
                let result = immutable_file_range
                    .to_range_inclusive(last_immutable_file_number)
                    .unwrap();
                assert_eq!(0..=8, result);

                let last_immutable_file_number = 7;
                immutable_file_range
                    .to_range_inclusive(last_immutable_file_number)
                    .expect_err("conversion to range inlusive should fail");
            }
        }
    }
}
