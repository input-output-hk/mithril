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
//!
//! # Download a Cardano database snapshot
//!
//! To download a partial or a full Cardano database folder the [ClientBuilder][crate::client::ClientBuilder].
//!
//! ```no_run
//! # #[cfg(feature = "fs")]
//! # async fn run() -> mithril_client::MithrilResult<()> {
//! use mithril_client::ClientBuilder;
//!
//! let client = ClientBuilder::aggregator("YOUR_AGGREGATOR_ENDPOINT", "YOUR_GENESIS_VERIFICATION_KEY").build()?;
//! let cardano_database_snapshot = client.cardano_database().get("CARDANO_DATABASE_HASH").await?.unwrap();
//!
//! // Note: the directory must already exist, and the user running the binary must have read/write access to it.
//! let target_directory = Path::new("/home/user/download/");
//! let immutable_file_range = ImmutableFileRange::Range(3, 6);
//! let download_unpack_options = DownloadUnpackOptions {
//!     allow_override: true,
//!     include_ancillary: true,
//! };
//! client
//!     .cardano_database()
//!     .download_unpack(
//!         &cardano_database_snapshot,
//!         &immutable_file_range,
//!         &target_directory,
//!         download_unpack_options,
//!     )
//!     .await?;
//! #
//! #    Ok(())
//! # }
//! ```
//!
//! # Compute a Merkle proof for a Cardano database snapshot
//!
//! To compute proof of membership of downloaded immutable files in a Cardano database folder the [ClientBuilder][crate::client::ClientBuilder].
//!
//! ```no_run
//! # #[cfg(feature = "fs")]
//! # async fn run() -> mithril_client::MithrilResult<()> {
//! use mithril_client::ClientBuilder;
//!
//! let client = ClientBuilder::aggregator("YOUR_AGGREGATOR_ENDPOINT", "YOUR_GENESIS_VERIFICATION_KEY").build()?;
//! let cardano_database_snapshot = client.cardano_database().get("CARDANO_DATABASE_HASH").await?.unwrap();
//! let certificate = client.certificate().verify_chain(&cardano_database_snapshot.certificate_hash).await?;
//!
//! // Note: the directory must already exist, and the user running the binary must have read/write access to it.
//! let target_directory = Path::new("/home/user/download/");
//! let immutable_file_range = ImmutableFileRange::Full;
//! let download_unpack_options = DownloadUnpackOptions {
//!     allow_override: true,
//!     include_ancillary: true,
//! };
//! client
//!     .cardano_database()
//!     .download_unpack(
//!         &cardano_database_snapshot,
//!         &immutable_file_range,
//!         &target_directory,
//!         download_unpack_options,
//!     )
//!     .await?;
//!
//! let merkle_proof = client
//!     .cardano_database()
//!     .compute_merkle_proof(&certificate, &immutable_file_range, &unpacked_dir)
//!     .await?;
//! #
//! #    Ok(())
//! # }
//! ```

#[cfg(feature = "fs")]
use std::collections::{BTreeMap, BTreeSet};
#[cfg(feature = "fs")]
use std::fs;
#[cfg(feature = "fs")]
use std::ops::RangeInclusive;
#[cfg(feature = "fs")]
use std::path::{Path, PathBuf};
use std::sync::Arc;

#[cfg(feature = "fs")]
use anyhow::anyhow;
use anyhow::Context;
#[cfg(feature = "fs")]
use serde::de::DeserializeOwned;
#[cfg(feature = "fs")]
use slog::Logger;
#[cfg(feature = "fs")]
use tokio::task::JoinSet;

#[cfg(feature = "fs")]
use mithril_common::{
    crypto_helper::{MKProof, MKTree, MKTreeNode, MKTreeStoreInMemory},
    digesters::{CardanoImmutableDigester, ImmutableDigester},
    entities::{
        AncillaryLocation, CompressionAlgorithm, DigestLocation, HexEncodedDigest,
        ImmutableFileName, ImmutableFileNumber, ImmutablesLocation,
    },
    messages::{
        CardanoDatabaseDigestListItemMessage, CardanoDatabaseSnapshotMessage, CertificateMessage,
        SignedEntityTypeMessagePart,
    },
    StdResult,
};

#[cfg(feature = "fs")]
use crate::feedback::{FeedbackSender, MithrilEvent};
use crate::{
    aggregator_client::{AggregatorClient, AggregatorClientError, AggregatorRequest},
    file_downloader::{FileDownloaderResolver, FileDownloaderUri},
    CardanoDatabaseSnapshot, CardanoDatabaseSnapshotListItem, MithrilResult,
};

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
    ancillary_file_downloader_resolver: Arc<dyn FileDownloaderResolver<AncillaryLocation>>,
    #[cfg(feature = "fs")]
    digest_file_downloader_resolver: Arc<dyn FileDownloaderResolver<DigestLocation>>,
    #[cfg(feature = "fs")]
    feedback_sender: FeedbackSender,
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
        #[cfg(feature = "fs")] ancillary_file_downloader_resolver: Arc<
            dyn FileDownloaderResolver<AncillaryLocation>,
        >,
        #[cfg(feature = "fs")] digest_file_downloader_resolver: Arc<
            dyn FileDownloaderResolver<DigestLocation>,
        >,
        #[cfg(feature = "fs")] feedback_sender: FeedbackSender,
        #[cfg(feature = "fs")] logger: Logger,
    ) -> Self {
        Self {
            aggregator_client,
            #[cfg(feature = "fs")]
            immutable_file_downloader_resolver,
            #[cfg(feature = "fs")]
            ancillary_file_downloader_resolver,
            #[cfg(feature = "fs")]
            digest_file_downloader_resolver,
            #[cfg(feature = "fs")]
            feedback_sender,
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
    async fn fetch_with_aggregator_request<T: DeserializeOwned>(
        &self,
        request: AggregatorRequest,
    ) -> MithrilResult<Option<T>> {
        match self.aggregator_client.get_content(request).await {
            Ok(content) => {
                let result: T = serde_json::from_str(&content)
                    .with_context(|| "CardanoDatabase client can not deserialize artifact")?;

                Ok(Some(result))
            }
            Err(AggregatorClientError::RemoteServerLogical(_)) => Ok(None),
            Err(e) => Err(e.into()),
        }
    }

    cfg_fs! {
        /// Download and unpack the given Cardano database parts data by hash.
        pub async fn download_unpack(
            &self,
            cardano_database_snapshot: &CardanoDatabaseSnapshotMessage,
            immutable_file_range: &ImmutableFileRange,
            target_dir: &Path,
            download_unpack_options: DownloadUnpackOptions,
        ) -> StdResult<()> {
            let compression_algorithm = cardano_database_snapshot.compression_algorithm;
            let last_immutable_file_number = cardano_database_snapshot.beacon.immutable_file_number;
            let immutable_file_number_range =
                immutable_file_range.to_range_inclusive(last_immutable_file_number)?;

            self.verify_can_write_to_target_directory(target_dir, &download_unpack_options)?;
            self.create_target_directory_sub_directories_if_not_exist(
                target_dir,
                &download_unpack_options,
            )?;

            let immutable_locations = &cardano_database_snapshot.locations.immutables;
            self.download_unpack_immutable_files(
                immutable_locations,
                immutable_file_number_range,
                &compression_algorithm,
                target_dir,
            )
            .await?;

            let digest_locations = &cardano_database_snapshot.locations.digests;
            self.download_unpack_digest_file(digest_locations, &Self::digest_target_dir(target_dir))
                .await?;

            if download_unpack_options.include_ancillary {
                let ancillary_locations = &cardano_database_snapshot.locations.ancillary;
                self.download_unpack_ancillary_file(
                    ancillary_locations,
                    &compression_algorithm,
                    target_dir,
                )
                .await?;
            }

            Ok(())
        }

        fn digest_target_dir(target_dir: &Path) -> PathBuf {
            target_dir.join("digest")
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

        fn delete_directory(dir: &Path) -> StdResult<()> {
            if dir.exists() {
                fs::remove_dir_all(dir).map_err(|e| anyhow!("Failed deleting directory: {e}"))?;
            }

            Ok(())
        }

        fn read_files_in_directory(dir: &Path) -> StdResult<Vec<PathBuf>> {
            let mut files = vec![];
            for entry in fs::read_dir(dir)? {
                let entry = entry?;
                let path = entry.path();
                if path.is_file() {
                    files.push(path);
                }
            }

            Ok(files)
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
            let digest_target_dir = Self::digest_target_dir(target_dir);
            Self::create_directory_if_not_exists(&digest_target_dir)?;
            if download_unpack_options.include_ancillary {
                let volatile_target_dir = Self::volatile_target_dir(target_dir);
                let ledger_target_dir = Self::ledger_target_dir(target_dir);
                Self::create_directory_if_not_exists(&volatile_target_dir)?;
                Self::create_directory_if_not_exists(&ledger_target_dir)?;
            }

            Ok(())
        }

        fn feedback_event_builder_immutable_download(download_id: String, downloaded_bytes: u64, size: u64) -> Option<MithrilEvent> {
            Some(MithrilEvent::ImmutableDownloadProgress {
                download_id,
                downloaded_bytes,
                size,
            })
        }

        fn feedback_event_builder_ancillary_download(download_id: String, downloaded_bytes: u64, size: u64) -> Option<MithrilEvent> {
            Some(MithrilEvent::AncillaryDownloadProgress {
                download_id,
                downloaded_bytes,
                size,
            })
        }

        fn feedback_event_builder_digest_download(download_id: String, downloaded_bytes: u64, size: u64) -> Option<MithrilEvent> {
            Some(MithrilEvent::DigestDownloadProgress {
                download_id,
                downloaded_bytes,
                size,
            })
        }

        /// Download and unpack the immutable files of the given range.
        ///
        /// The download is attempted for each location until the full range is downloaded.
        /// An error is returned if not all the files are downloaded.
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
                range.clone().map(|n| n.to_owned()).collect::<BTreeSet<_>>();
            for location in locations_sorted {
                let immutable_files_numbers_downloaded = self.download_unpack_immutable_files_for_location(
                    &location,
                    &immutable_file_numbers_to_download,
                    compression_algorithm,
                    immutable_files_target_dir,
                ).await?;
                for immutable_file_number in immutable_files_numbers_downloaded {
                    immutable_file_numbers_to_download.remove(&immutable_file_number);
                }
                if immutable_file_numbers_to_download.is_empty() {
                    return Ok(());
                }
            }

            Err(anyhow!(
                "Failed downloading and unpacking immutable files for locations: {locations:?}"
            ))
        }

        async fn download_unpack_immutable_files_for_location(&self, location: &ImmutablesLocation, immutable_file_numbers_to_download: &BTreeSet<ImmutableFileNumber>, compression_algorithm: &CompressionAlgorithm, immutable_files_target_dir: &Path,) -> StdResult<BTreeSet<ImmutableFileNumber>> {
            let mut immutable_file_numbers_downloaded = BTreeSet::new();
            let file_downloader = self
                .immutable_file_downloader_resolver
                .resolve(location)
                .ok_or_else(|| {
                    anyhow!("Failed resolving a file downloader for location: {location:?}")
                })?;
            let file_downloader_uris =
                FileDownloaderUri::expand_immutable_files_location_to_file_downloader_uris(
                    location,
                    immutable_file_numbers_to_download
                    .clone()
                    .into_iter()
                    .collect::<Vec<_>>()
                    .as_slice(),
                )?;
            let mut join_set: JoinSet<StdResult<ImmutableFileNumber>> = JoinSet::new();
            for (immutable_file_number, file_downloader_uri) in file_downloader_uris {
                let compression_algorithm_clone = compression_algorithm.to_owned();
                let immutable_files_target_dir_clone = immutable_files_target_dir.to_owned();
                let file_downloader_clone = file_downloader.clone();
                let feedback_receiver_clone = self.feedback_sender.clone();
                join_set.spawn(async move {
                    let download_id = MithrilEvent::new_snapshot_download_id();
                    feedback_receiver_clone
                        .send_event(MithrilEvent::ImmutableDownloadStarted { immutable_file_number, download_id: download_id.clone()})
                        .await;
                    let downloaded = file_downloader_clone
                        .download_unpack(
                            &file_downloader_uri,
                            &immutable_files_target_dir_clone,
                            Some(compression_algorithm_clone),
                            &download_id,
                            Self::feedback_event_builder_immutable_download,
                        )
                        .await;
                    match downloaded {
                        Ok(_) => {
                            feedback_receiver_clone
                                .send_event(MithrilEvent::ImmutableDownloadCompleted { download_id })
                                .await;

                            Ok(immutable_file_number)
                        }
                        Err(e) => {
                            Err(e.context(format!("Failed downloading and unpacking immutable file {immutable_file_number} for location {file_downloader_uri:?}")))
                        }
                    }
                });
            }
            while let Some(result) = join_set.join_next().await {
                match result? {
                    Ok(immutable_file_number) => {
                        immutable_file_numbers_downloaded.insert(immutable_file_number);
                    }
                    Err(e) => {
                        slog::error!(
                            self.logger,
                            "Failed downloading and unpacking immutable files"; "error" => e.to_string()
                        );
                    }
                }
            }

            Ok(immutable_file_numbers_downloaded)
        }

        /// Download and unpack the ancillary files.
        async fn download_unpack_ancillary_file(
            &self,
            locations: &[AncillaryLocation],
            compression_algorithm: &CompressionAlgorithm,
            ancillary_file_target_dir: &Path,
        ) -> StdResult<()> {
            let mut locations_sorted = locations.to_owned();
            locations_sorted.sort();
            for location in locations_sorted {
                let download_id = MithrilEvent::new_ancillary_download_id();
                self.feedback_sender
                        .send_event(MithrilEvent::AncillaryDownloadStarted { download_id: download_id.clone()})
                        .await;
                let file_downloader = self
                    .ancillary_file_downloader_resolver
                    .resolve(&location)
                    .ok_or_else(|| {
                        anyhow!("Failed resolving a file downloader for location: {location:?}")
                    })?;
                let file_downloader_uri: FileDownloaderUri = location.into();
                let downloaded = file_downloader
                    .download_unpack(
                        &file_downloader_uri,
                        ancillary_file_target_dir,
                        Some(compression_algorithm.to_owned()),
                        &download_id,
                        Self::feedback_event_builder_ancillary_download
                    )
                    .await;
                match downloaded {
                    Ok(_) => {
                        self.feedback_sender
                                .send_event(MithrilEvent::AncillaryDownloadCompleted { download_id })
                                .await;
                        return Ok(())
                    },
                    Err(e) => {
                        slog::error!(
                            self.logger,
                            "Failed downloading and unpacking ancillaries for location {file_downloader_uri:?}"; "error" => e.to_string()
                        );
                    }
                }
            }

            Err(anyhow!(
                "Failed downloading and unpacking ancillaries for all locations"
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
                let download_id = MithrilEvent::new_digest_download_id();
                self.feedback_sender
                        .send_event(MithrilEvent::DigestDownloadStarted { download_id: download_id.clone()})
                        .await;
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
                        Self::feedback_event_builder_digest_download
                    )
                    .await;
                match downloaded {
                    Ok(_) => {
                        self.feedback_sender
                                .send_event(MithrilEvent::DigestDownloadCompleted { download_id })
                                .await;
                        return Ok(())
                    },
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

        fn read_digest_file(
            &self,
            digest_file_target_dir: &Path,
        ) -> StdResult<BTreeMap<ImmutableFileName, HexEncodedDigest>> {
            let digest_files = Self::read_files_in_directory(digest_file_target_dir)?;
            if digest_files.len() > 1 {
                return Err(anyhow!(
                    "Multiple digest files found in directory: {digest_file_target_dir:?}"
                ));
            }
            if digest_files.is_empty() {
                return Err(anyhow!(
                    "No digest file found in directory: {digest_file_target_dir:?}"
                ));
            }

            let digest_file = &digest_files[0];
            let content = fs::read_to_string(digest_file)
                .with_context(|| format!("Failed reading digest file: {digest_file:?}"))?;
            let digest_messages: Vec<CardanoDatabaseDigestListItemMessage> =
                serde_json::from_str(&content)
                    .with_context(|| format!("Failed deserializing digest file: {digest_file:?}"))?;
            let digest_map = digest_messages
                .into_iter()
                .map(|message| (message.immutable_file_name, message.digest))
                .collect::<BTreeMap<_, _>>();

            Ok(digest_map)
        }

        /// Compute the Merkle proof of membership for the given immutable file range.
        pub async fn compute_merkle_proof(
            &self,
            certificate: &CertificateMessage,
            immutable_file_range: &ImmutableFileRange,
            database_dir: &Path,
        ) -> StdResult<MKProof> {
            let network = certificate.metadata.network.clone();
            let last_immutable_file_number = match &certificate.signed_entity_type {
                SignedEntityTypeMessagePart::CardanoDatabase(beacon) => beacon.immutable_file_number,
                _ => return Err(anyhow!("Invalid signed entity type: {:?}",certificate.signed_entity_type)),
            };
            let immutable_file_number_range =
                immutable_file_range.to_range_inclusive(last_immutable_file_number)?;

            let downloaded_digests = self.read_digest_file(&Self::digest_target_dir(database_dir))?;
            let merkle_tree: MKTree<MKTreeStoreInMemory> =
                MKTree::new(&downloaded_digests.values().cloned().collect::<Vec<_>>())?;
            let immutable_digester = CardanoImmutableDigester::new(network, None, self.logger.clone());
            let computed_digests = immutable_digester
                .compute_digests_for_range(database_dir, &immutable_file_number_range)
                .await?.entries
                .values()
                .map(MKTreeNode::from)
                .collect::<Vec<_>>();
            Self::delete_directory(&Self::digest_target_dir(database_dir))?;

            merkle_tree.compute_proof(&computed_digests)
        }
    }
}

#[cfg(test)]
mod tests {

    use super::*;

    mod cardano_database_client {
        use anyhow::anyhow;
        use chrono::{DateTime, Utc};
        use mockall::predicate::eq;

        use mithril_common::{
            digesters::CardanoImmutableDigester,
            entities::{
                AncillaryLocationDiscriminants, CardanoDbBeacon, CompressionAlgorithm,
                DigestLocationDiscriminants, Epoch, ImmutablesLocationDiscriminants,
            },
        };

        use crate::{
            aggregator_client::MockAggregatorHTTPClient,
            feedback::{FeedbackReceiver, MithrilEvent, StackFeedbackReceiver},
            file_downloader::{
                AncillaryFileDownloaderResolver, DigestFileDownloaderResolver, FileDownloader,
                ImmutablesFileDownloaderResolver, MockFileDownloaderBuilder,
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
            ancillary_file_downloader_resolver: AncillaryFileDownloaderResolver,
            digest_file_downloader_resolver: DigestFileDownloaderResolver,
            feedback_receivers: Vec<Arc<dyn FeedbackReceiver>>,
        }

        impl CardanoDatabaseClientDependencyInjector {
            fn new() -> Self {
                Self {
                    http_client: MockAggregatorHTTPClient::new(),
                    immutable_file_downloader_resolver: ImmutablesFileDownloaderResolver::new(
                        vec![],
                    ),
                    ancillary_file_downloader_resolver: AncillaryFileDownloaderResolver::new(
                        vec![],
                    ),
                    digest_file_downloader_resolver: DigestFileDownloaderResolver::new(vec![]),
                    feedback_receivers: vec![],
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

            fn with_ancillary_file_downloaders(
                self,
                file_downloaders: Vec<(AncillaryLocationDiscriminants, Arc<dyn FileDownloader>)>,
            ) -> Self {
                let ancillary_file_downloader_resolver =
                    AncillaryFileDownloaderResolver::new(file_downloaders);

                Self {
                    ancillary_file_downloader_resolver,
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

            fn with_feedback_receivers(
                self,
                feedback_receivers: &[Arc<dyn FeedbackReceiver>],
            ) -> Self {
                Self {
                    feedback_receivers: feedback_receivers.to_vec(),
                    ..self
                }
            }

            fn build_cardano_database_client(self) -> CardanoDatabaseClient {
                CardanoDatabaseClient::new(
                    Arc::new(self.http_client),
                    Arc::new(self.immutable_file_downloader_resolver),
                    Arc::new(self.ancillary_file_downloader_resolver),
                    Arc::new(self.digest_file_downloader_resolver),
                    FeedbackSender::new(&self.feedback_receivers),
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
                    entities::{ImmutablesLocationDiscriminants, MultiFilesUri, TemplateUri},
                    messages::ArtifactsLocationsMessagePart,
                    test_utils::TempDir,
                };

                use super::*;

                #[tokio::test]
                async fn download_unpack_fails_with_invalid_immutable_file_range() {
                    let immutable_file_range = ImmutableFileRange::Range(1, 0);
                    let download_unpack_options = DownloadUnpackOptions::default();
                    let cardano_db_snapshot = CardanoDatabaseSnapshot {
                        hash: "hash-123".to_string(),
                        ..CardanoDatabaseSnapshot::dummy()
                    };
                    let target_dir = Path::new(".");
                    let client = CardanoDatabaseClientDependencyInjector::new()
                        .build_cardano_database_client();

                    client
                        .download_unpack(
                            &cardano_db_snapshot,
                            &immutable_file_range,
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
                    let cardano_db_snapshot = CardanoDatabaseSnapshot {
                        hash: "hash-123".to_string(),
                        locations: ArtifactsLocationsMessagePart {
                            immutables: vec![ImmutablesLocation::CloudStorage {
                                uri: MultiFilesUri::Template(TemplateUri(
                                    "http://whatever/{immutable_file_number}.tar.gz".to_string(),
                                )),
                            }],
                            ..ArtifactsLocationsMessagePart::default()
                        },
                        ..CardanoDatabaseSnapshot::dummy()
                    };
                    let target_dir = TempDir::new(
                        "cardano_database_client",
                        "download_unpack_fails_when_immutable_files_download_fail",
                    )
                    .build();
                    let client = CardanoDatabaseClientDependencyInjector::new()
                        .with_immutable_file_downloaders(vec![(
                            ImmutablesLocationDiscriminants::CloudStorage,
                            Arc::new({
                                MockFileDownloaderBuilder::default()
                                    .with_times(total_immutable_files as usize)
                                    .with_failure()
                                    .build()
                            }),
                        )])
                        .build_cardano_database_client();

                    client
                        .download_unpack(
                            &cardano_db_snapshot,
                            &immutable_file_range,
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
                    let cardano_db_snapshot = CardanoDatabaseSnapshot {
                        hash: "hash-123".to_string(),
                        ..CardanoDatabaseSnapshot::dummy()
                    };
                    let target_dir = &TempDir::new(
                                        "cardano_database_client",
                                        "download_unpack_fails_when_target_target_dir_would_be_overwritten_without_allow_override",
                                    )
                                    .build();
                    fs::create_dir_all(target_dir.join("immutable")).unwrap();
                    let client = CardanoDatabaseClientDependencyInjector::new()
                        .build_cardano_database_client();

                    client
                        .download_unpack(
                            &cardano_db_snapshot,
                            &immutable_file_range,
                            target_dir,
                            download_unpack_options,
                        )
                        .await
                        .expect_err("download_unpack should fail");
                }

                #[tokio::test]
                async fn download_unpack_succeeds_with_valid_range() {
                    let immutable_file_range = ImmutableFileRange::Range(1, 2);
                    let download_unpack_options = DownloadUnpackOptions {
                        include_ancillary: true,
                        ..DownloadUnpackOptions::default()
                    };
                    let cardano_db_snapshot = CardanoDatabaseSnapshot {
                        hash: "hash-123".to_string(),
                        locations: ArtifactsLocationsMessagePart {
                            immutables: vec![ImmutablesLocation::CloudStorage {
                                uri: MultiFilesUri::Template(TemplateUri(
                                    "http://whatever/{immutable_file_number}.tar.gz".to_string(),
                                )),
                            }],
                            ancillary: vec![AncillaryLocation::CloudStorage {
                                uri: "http://whatever/ancillary.tar.gz".to_string(),
                            }],
                            digests: vec![DigestLocation::CloudStorage {
                                uri: "http://whatever/digests.json".to_string(),
                            }],
                        },
                        ..CardanoDatabaseSnapshot::dummy()
                    };
                    let target_dir = TempDir::new(
                        "cardano_database_client",
                        "download_unpack_succeeds_with_valid_range",
                    )
                    .build();
                    let client = CardanoDatabaseClientDependencyInjector::new()
                        .with_immutable_file_downloaders(vec![(
                            ImmutablesLocationDiscriminants::CloudStorage,
                            Arc::new({
                                let mock_file_downloader = MockFileDownloaderBuilder::default()
                                    .with_file_uri("http://whatever/00001.tar.gz")
                                    .with_target_dir(target_dir.clone())
                                    .with_success()
                                    .build();

                                MockFileDownloaderBuilder::from_mock(mock_file_downloader)
                                    .with_file_uri("http://whatever/00002.tar.gz")
                                    .with_target_dir(target_dir.clone())
                                    .with_success()
                                    .build()
                            }),
                        )])
                        .with_ancillary_file_downloaders(vec![(
                            AncillaryLocationDiscriminants::CloudStorage,
                            Arc::new(
                                MockFileDownloaderBuilder::default()
                                    .with_file_uri("http://whatever/ancillary.tar.gz")
                                    .with_target_dir(target_dir.clone())
                                    .with_compression(Some(CompressionAlgorithm::default()))
                                    .with_success()
                                    .build(),
                            ),
                        )])
                        .with_digest_file_downloaders(vec![(
                            DigestLocationDiscriminants::CloudStorage,
                            Arc::new({
                                MockFileDownloaderBuilder::default()
                                    .with_file_uri("http://whatever/digests.json")
                                    .with_target_dir(target_dir.join("digest"))
                                    .with_compression(None)
                                    .with_success()
                                    .build()
                            }),
                        )])
                        .build_cardano_database_client();

                    client
                        .download_unpack(
                            &cardano_db_snapshot,
                            &immutable_file_range,
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
                    assert!(!target_dir.join("digest").exists());
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

                    assert!(target_dir.join("digest").exists());
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
                    assert!(!target_dir.join("digest").exists());
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

                    assert!(target_dir.join("digest").exists());
                    assert!(target_dir.join("immutable").exists());
                    assert!(target_dir.join("volatile").exists());
                    assert!(target_dir.join("ledger").exists());
                }
            }

            mod download_unpack_immutable_files {
                use mithril_common::{
                    entities::{MultiFilesUri, TemplateUri},
                    test_utils::TempDir,
                };

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
                                let mock_file_downloader =
                                    MockFileDownloaderBuilder::default().with_failure().build();

                                MockFileDownloaderBuilder::from_mock(mock_file_downloader)
                                    .with_success()
                                    .build()
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
                            Arc::new(
                                MockFileDownloaderBuilder::default()
                                    .with_times(2)
                                    .with_success()
                                    .build(),
                            ),
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
                                let mock_file_downloader = MockFileDownloaderBuilder::default()
                                    .with_file_uri("http://whatever-1/00001.tar.gz")
                                    .with_target_dir(target_dir.clone())
                                    .with_failure()
                                    .build();
                                let mock_file_downloader =
                                    MockFileDownloaderBuilder::from_mock(mock_file_downloader)
                                        .with_file_uri("http://whatever-1/00002.tar.gz")
                                        .with_target_dir(target_dir.clone())
                                        .with_success()
                                        .build();

                                MockFileDownloaderBuilder::from_mock(mock_file_downloader)
                                    .with_file_uri("http://whatever-2/00001.tar.gz")
                                    .with_target_dir(target_dir.clone())
                                    .with_success()
                                    .build()
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

                #[tokio::test]
                async fn download_unpack_immutable_files_sends_feedbacks() {
                    let total_immutable_files = 1;
                    let immutable_file_range = ImmutableFileRange::Range(1, total_immutable_files);
                    let target_dir = Path::new(".");
                    let feedback_receiver = Arc::new(StackFeedbackReceiver::new());
                    let client = CardanoDatabaseClientDependencyInjector::new()
                        .with_immutable_file_downloaders(vec![(
                            ImmutablesLocationDiscriminants::CloudStorage,
                            Arc::new({
                                MockFileDownloaderBuilder::default()
                                    .with_success()
                                    .build()
                            }),
                        )])
                        .with_feedback_receivers(&[feedback_receiver.clone()])
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
                            target_dir,
                        )
                        .await
                        .unwrap();

                    let sent_events = feedback_receiver.stacked_events();
                    let id = sent_events[0].event_id();
                    let expected_events = vec![
                        MithrilEvent::ImmutableDownloadStarted {
                            immutable_file_number: 1,
                            download_id: id.to_string(),

                        },
                        MithrilEvent::ImmutableDownloadCompleted {
                            download_id: id.to_string(),
                        },
                    ];
                    assert_eq!(expected_events, sent_events);
                }
            }

            mod download_unpack_ancillary_file {

                use super::*;

                #[tokio::test]
                async fn download_unpack_ancillary_file_fails_if_no_location_is_retrieved() {
                    let target_dir = Path::new(".");
                    let client = CardanoDatabaseClientDependencyInjector::new()
                        .with_ancillary_file_downloaders(vec![(
                            AncillaryLocationDiscriminants::CloudStorage,
                            Arc::new(MockFileDownloaderBuilder::default().with_failure().build()),
                        )])
                        .build_cardano_database_client();

                    client
                        .download_unpack_ancillary_file(
                            &[AncillaryLocation::CloudStorage {
                                uri: "http://whatever-1/ancillary.tar.gz".to_string(),
                            }],
                            &CompressionAlgorithm::default(),
                            target_dir,
                        )
                        .await
                        .expect_err("download_unpack_ancillary_file should fail");
                }

                #[tokio::test]
                async fn download_unpack_ancillary_file_succeeds_if_at_least_one_location_is_retrieved()
                {
                    let target_dir = Path::new(".");
                    let client = CardanoDatabaseClientDependencyInjector::new()
                        .with_ancillary_file_downloaders(vec![(
                            AncillaryLocationDiscriminants::CloudStorage,
                            Arc::new({
                                let mock_file_downloader = MockFileDownloaderBuilder::default()
                                    .with_file_uri("http://whatever-1/ancillary.tar.gz")
                                    .with_target_dir(target_dir.to_path_buf())
                                    .with_failure()
                                    .build();

                                MockFileDownloaderBuilder::from_mock(mock_file_downloader)
                                    .with_file_uri("http://whatever-2/ancillary.tar.gz")
                                    .with_target_dir(target_dir.to_path_buf())
                                    .with_success()
                                    .build()
                            }),
                        )])
                        .build_cardano_database_client();

                    client
                        .download_unpack_ancillary_file(
                            &[
                                AncillaryLocation::CloudStorage {
                                    uri: "http://whatever-1/ancillary.tar.gz".to_string(),
                                },
                                AncillaryLocation::CloudStorage {
                                    uri: "http://whatever-2/ancillary.tar.gz".to_string(),
                                },
                            ],
                            &CompressionAlgorithm::default(),
                            target_dir,
                        )
                        .await
                        .unwrap();
                }

                #[tokio::test]
                async fn download_unpack_ancillary_file_succeeds_when_first_location_is_retrieved() {
                    let target_dir = Path::new(".");
                    let client = CardanoDatabaseClientDependencyInjector::new()
                        .with_ancillary_file_downloaders(vec![(
                            AncillaryLocationDiscriminants::CloudStorage,
                            Arc::new(
                                MockFileDownloaderBuilder::default()
                                    .with_file_uri("http://whatever-1/ancillary.tar.gz")
                                    .with_target_dir(target_dir.to_path_buf())
                                    .with_success()
                                    .build(),
                            ),
                        )])
                        .build_cardano_database_client();

                    client
                        .download_unpack_ancillary_file(
                            &[
                                AncillaryLocation::CloudStorage {
                                    uri: "http://whatever-1/ancillary.tar.gz".to_string(),
                                },
                                AncillaryLocation::CloudStorage {
                                    uri: "http://whatever-2/ancillary.tar.gz".to_string(),
                                },
                            ],
                            &CompressionAlgorithm::default(),
                            target_dir,
                        )
                        .await
                        .unwrap();
                }

                #[tokio::test]
                async fn download_unpack_ancillary_files_sends_feedbacks() {
                    let target_dir = Path::new(".");
                    let feedback_receiver = Arc::new(StackFeedbackReceiver::new());
                    let client = CardanoDatabaseClientDependencyInjector::new()
                        .with_ancillary_file_downloaders(vec![(
                            AncillaryLocationDiscriminants::CloudStorage,
                            Arc::new(
                                MockFileDownloaderBuilder::default()
                                    .with_success()
                                    .build(),
                            ),
                        )])
                        .with_feedback_receivers(&[feedback_receiver.clone()])
                        .build_cardano_database_client();

                    client
                        .download_unpack_ancillary_file(
                            &[
                                AncillaryLocation::CloudStorage {
                                    uri: "http://whatever-1/ancillary.tar.gz".to_string(),
                                },
                            ],
                            &CompressionAlgorithm::default(),
                            target_dir,
                        )
                        .await
                        .unwrap();

                    let sent_events = feedback_receiver.stacked_events();
                    let id = sent_events[0].event_id();
                    let expected_events = vec![
                        MithrilEvent::AncillaryDownloadStarted {
                            download_id: id.to_string(),

                        },
                        MithrilEvent::AncillaryDownloadCompleted {
                            download_id: id.to_string(),
                        },
                    ];
                    assert_eq!(expected_events, sent_events);
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
                                Arc::new(
                                    MockFileDownloaderBuilder::default()
                                        .with_compression(None)
                                        .with_failure()
                                        .build(),
                                ),
                            ),
                            (
                                DigestLocationDiscriminants::Aggregator,
                                Arc::new(
                                    MockFileDownloaderBuilder::default()
                                        .with_compression(None)
                                        .with_failure()
                                        .build(),
                                ),
                            ),
                        ])
                        .build_cardano_database_client();

                    client
                        .download_unpack_digest_file(
                            &[
                                DigestLocation::CloudStorage {
                                    uri: "http://whatever-1/digests.json".to_string(),
                                },
                                DigestLocation::Aggregator {
                                    uri: "http://whatever-2/digest".to_string(),
                                },
                            ],
                            target_dir,
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
                                Arc::new(
                                    MockFileDownloaderBuilder::default()
                                        .with_compression(None)
                                        .with_failure()
                                        .build(),
                                ),
                            ),
                            (
                                DigestLocationDiscriminants::Aggregator,
                                Arc::new(
                                    MockFileDownloaderBuilder::default()
                                        .with_compression(None)
                                        .with_success()
                                        .build(),
                                ),
                            ),
                        ])
                        .build_cardano_database_client();

                    client
                        .download_unpack_digest_file(
                            &[
                                DigestLocation::CloudStorage {
                                    uri: "http://whatever-1/digests.json".to_string(),
                                },
                                DigestLocation::Aggregator {
                                    uri: "http://whatever-2/digest".to_string(),
                                },
                            ],
                            target_dir,
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
                                Arc::new(
                                    MockFileDownloaderBuilder::default()
                                        .with_compression(None)
                                        .with_success()
                                        .build(),
                                ),
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
                                    uri: "http://whatever-1/digests.json".to_string(),
                                },
                                DigestLocation::Aggregator {
                                    uri: "http://whatever-2/digest".to_string(),
                                },
                            ],
                            target_dir,
                        )
                        .await
                        .unwrap();
                }

                #[tokio::test]
                async fn download_unpack_digest_file_sends_feedbacks() {
                    let target_dir = Path::new(".");
                    let feedback_receiver = Arc::new(StackFeedbackReceiver::new());
                    let client = CardanoDatabaseClientDependencyInjector::new()
                        .with_digest_file_downloaders(vec![
                            (
                                DigestLocationDiscriminants::CloudStorage,
                                Arc::new(
                                    MockFileDownloaderBuilder::default()
                                        .with_compression(None)
                                        .with_success()
                                        .build(),
                                ),
                            ),
                        ])
                        .with_feedback_receivers(&[feedback_receiver.clone()])
                        .build_cardano_database_client();

                    client
                        .download_unpack_digest_file(
                            &[
                                DigestLocation::CloudStorage {
                                    uri: "http://whatever-1/digests.json".to_string(),
                                },
                            ],
                            target_dir,
                        )
                        .await
                        .unwrap();

                    let sent_events = feedback_receiver.stacked_events();
                    let id = sent_events[0].event_id();
                    let expected_events = vec![
                        MithrilEvent::DigestDownloadStarted {
                            download_id: id.to_string(),

                        },
                        MithrilEvent::DigestDownloadCompleted {
                            download_id: id.to_string(),
                        },
                    ];
                    assert_eq!(expected_events, sent_events);
                }
            }

            mod read_digest_file {
                use std::io::Write;

                use mithril_common::test_utils::TempDir;

                use super::*;

                fn create_valid_fake_digest_file(
                    file_path: &Path,
                    digest_messages: &[CardanoDatabaseDigestListItemMessage],
                ) {
                    let mut file = fs::File::create(file_path).unwrap();
                    let digest_json = serde_json::to_string(&digest_messages).unwrap();
                    file.write_all(digest_json.as_bytes()).unwrap();
                }

                fn create_invalid_fake_digest_file(file_path: &Path) {
                    let mut file = fs::File::create(file_path).unwrap();
                    file.write_all(b"incorrect-digest").unwrap();
                }

                #[test]
                fn read_digest_file_fails_when_no_digest_file() {
                    let target_dir = TempDir::new(
                        "cardano_database_client",
                        "read_digest_file_fails_when_no_digest_file",
                    )
                    .build();
                    let client =
                        CardanoDatabaseClientDependencyInjector::new().build_cardano_database_client();

                    client
                        .read_digest_file(&target_dir)
                        .expect_err("read_digest_file should fail");
                }

                #[test]
                fn read_digest_file_fails_when_multiple_digest_files() {
                    let target_dir = TempDir::new(
                        "cardano_database_client",
                        "read_digest_file_fails_when_multiple_digest_files",
                    )
                    .build();
                    create_valid_fake_digest_file(&target_dir.join("digests.json"), &[]);
                    create_valid_fake_digest_file(&target_dir.join("digests-2.json"), &[]);
                    let client =
                        CardanoDatabaseClientDependencyInjector::new().build_cardano_database_client();

                    client
                        .read_digest_file(&target_dir)
                        .expect_err("read_digest_file should fail");
                }

                #[test]
                fn read_digest_file_fails_when_invalid_unique_digest_file() {
                    let target_dir = TempDir::new(
                        "cardano_database_client",
                        "read_digest_file_fails_when_invalid_unique_digest_file",
                    )
                    .build();
                    create_invalid_fake_digest_file(&target_dir.join("digests.json"));
                    let client =
                        CardanoDatabaseClientDependencyInjector::new().build_cardano_database_client();

                    client
                        .read_digest_file(&target_dir)
                        .expect_err("read_digest_file should fail");
                }

                #[test]
                fn read_digest_file_succeeds_when_valid_unique_digest_file() {
                    let target_dir = TempDir::new(
                        "cardano_database_client",
                        "read_digest_file_succeeds_when_valid_unique_digest_file",
                    )
                    .build();
                    let digest_messages = vec![
                        CardanoDatabaseDigestListItemMessage {
                            immutable_file_name: "00001.chunk".to_string(),
                            digest: "digest-1".to_string(),
                        },
                        CardanoDatabaseDigestListItemMessage {
                            immutable_file_name: "00002.chunk".to_string(),
                            digest: "digest-2".to_string(),
                        },
                    ];
                    create_valid_fake_digest_file(&target_dir.join("digests.json"), &digest_messages);
                    let client =
                        CardanoDatabaseClientDependencyInjector::new().build_cardano_database_client();

                    let digests = client.read_digest_file(&target_dir).unwrap();
                    assert_eq!(
                        BTreeMap::from([
                            ("00001.chunk".to_string(), "digest-1".to_string()),
                            ("00002.chunk".to_string(), "digest-2".to_string())
                        ]),
                        digests
                    )
                }
            }
        }

        mod compute_merkle_proof {
            use mithril_common::{
                digesters::{DummyCardanoDbBuilder, ImmutableDigester, ImmutableFile},
                messages::SignedEntityTypeMessagePart,
            };

            use crate::test_utils::test_logger;

            use super::*;

            async fn write_digest_file(
                digest_dir: &Path,
                digests: BTreeMap<ImmutableFile, HexEncodedDigest>,
            ) {
                let digest_file_path = digest_dir.join("digests.json");
                if !digest_dir.exists() {
                    fs::create_dir_all(digest_dir).unwrap();
                }

                let immutable_digest_messages = digests
                    .into_iter()
                    .map(
                        |(immutable_file, digest)| CardanoDatabaseDigestListItemMessage {
                            immutable_file_name: immutable_file.filename,
                            digest,
                        },
                    )
                    .collect::<Vec<_>>();
                serde_json::to_writer(
                    fs::File::create(digest_file_path).unwrap(),
                    &immutable_digest_messages,
                )
                .unwrap();
            }

            #[tokio::test]
            async fn compute_merkle_proof_fails_if_mismatching_certificate() {
                let immutable_file_range = 1..=5;
                let immutable_file_range_to_prove = ImmutableFileRange::Range(2, 4);
                let certificate = CertificateMessage {
                    hash: "cert-hash-123".to_string(),
                    signed_entity_type: SignedEntityTypeMessagePart::MithrilStakeDistribution(
                        Epoch(123),
                    ),
                    ..CertificateMessage::dummy()
                };
                let cardano_db = DummyCardanoDbBuilder::new(
                    "compute_merkle_proof_fails_if_mismatching_certificate",
                )
                .with_immutables(&immutable_file_range.clone().collect::<Vec<_>>())
                .append_immutable_trio()
                .build();
                let database_dir = cardano_db.get_dir();
                let client =
                    CardanoDatabaseClientDependencyInjector::new().build_cardano_database_client();

                client
                    .compute_merkle_proof(
                        &certificate,
                        &immutable_file_range_to_prove,
                        database_dir,
                    )
                    .await
                    .expect_err("compute_merkle_proof should fail");
            }

            #[tokio::test]
            async fn compute_merkle_proof_succeeds() {
                let beacon = CardanoDbBeacon {
                    epoch: Epoch(123),
                    immutable_file_number: 5,
                };
                let immutable_file_range = 1..=5;
                let immutable_file_range_to_prove = ImmutableFileRange::Range(2, 4);
                let certificate = CertificateMessage {
                    hash: "cert-hash-123".to_string(),
                    signed_entity_type: SignedEntityTypeMessagePart::CardanoDatabase(
                        beacon.clone(),
                    ),
                    ..CertificateMessage::dummy()
                };
                let cardano_db = DummyCardanoDbBuilder::new("compute_merkle_proof_succeeds")
                    .with_immutables(&immutable_file_range.clone().collect::<Vec<_>>())
                    .append_immutable_trio()
                    .build();
                let database_dir = cardano_db.get_dir();
                let immutable_digester = CardanoImmutableDigester::new(
                    certificate.metadata.network.clone(),
                    None,
                    test_logger(),
                );
                let client =
                    CardanoDatabaseClientDependencyInjector::new().build_cardano_database_client();
                let computed_digests = immutable_digester
                    .compute_digests_for_range(database_dir, &immutable_file_range)
                    .await
                    .unwrap();
                write_digest_file(&database_dir.join("digest"), computed_digests.entries).await;
                let merkle_tree = immutable_digester
                    .compute_merkle_tree(database_dir, &beacon)
                    .await
                    .unwrap();
                let expected_merkle_root = merkle_tree.compute_root().unwrap();

                let merkle_proof = client
                    .compute_merkle_proof(
                        &certificate,
                        &immutable_file_range_to_prove,
                        database_dir,
                    )
                    .await
                    .unwrap();
                let merkle_proof_root = merkle_proof.root().to_owned();

                merkle_proof.verify().unwrap();
                assert_eq!(expected_merkle_root, merkle_proof_root);

                assert!(!database_dir.join("digest").exists());
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
