use anyhow::Context;
use async_trait::async_trait;
use slog::{Logger, info, warn};
use std::{
    fs,
    path::{Path, PathBuf},
};

use mithril_common::{
    StdResult,
    entities::{CardanoDbBeacon, ProtocolMessage, ProtocolMessagePartKey},
    logging::LoggerExtensions,
    signable_builder::SignableBuilder,
};

use crate::entities::AncillaryFilesManifest;
use crate::{LEDGER_DIR, entities::LedgerStateSnapshot};

/// This structure is responsible for calculating the message for Cardano node ledger state snapshot.
pub struct CardanoNodeLedgerStateSignableBuilder {
    logger: Logger,
    dirpath: PathBuf,
}

impl CardanoNodeLedgerStateSignableBuilder {
    /// Constructor
    pub fn new(dirpath: &Path, logger: Logger) -> Self {
        Self {
            logger: logger.new_with_component_name::<Self>(),
            dirpath: dirpath.to_owned(),
        }
    }

    //TODO: copied and adapted from CompressedArchiveSnapshotter, mutualize with it ?
    async fn get_files_and_directories_for_ledger_snapshot(
        &self,
        target_folder: &Path,
    ) -> StdResult<Vec<PathBuf>> {
        //List ledger files to be signed
        let db_ledger_dir = self.dirpath.join(LEDGER_DIR);
        let ledger_states = LedgerStateSnapshot::list_all_in_dir(&db_ledger_dir)?;
        let latest_ledger_state = ledger_states.iter().last();
        let latest_ledger_files: Vec<PathBuf> = latest_ledger_state
            .map(|ledger_state_snapshot| {
                ledger_state_snapshot
                    .get_files_relative_path()
                    .into_iter()
                    .map(|path| PathBuf::from(LEDGER_DIR).join(path))
                    .collect()
            })
            .unwrap_or_default();

        // fs::create_dir(target_folder.join(IMMUTABLE_DIR))
        //     .with_context(|| format!("Can not create folder: `{}`", target_folder.display()))?;
        // fs::create_dir(target_folder.join(LEDGER_DIR))
        //     .with_context(|| format!("Can not create folder: `{}`", target_folder.display()))?;

        for file in &latest_ledger_files {
            // Some files to snapshot are in subfolders (i.e.: in-memory ledger snapshots files)
            if let Some(parent_dir) = file.parent() {
                let target_parent_dir = target_folder.join(parent_dir);
                if !target_parent_dir.exists() {
                    fs::create_dir_all(&target_parent_dir).with_context(|| {
                        format!("Can not create folder: `{}`", target_parent_dir.display())
                    })?;
                }
            }

            let source = self.dirpath.join(file);
            let target = target_folder.join(file);
            tokio::fs::copy(&source, &target).await.with_context(|| {
                format!(
                    "Failed to copy ledger file `{}` to `{}`",
                    source.display(),
                    target.display()
                )
            })?;
        }

        Ok(latest_ledger_files)
    }
}

#[async_trait]
impl SignableBuilder<CardanoDbBeacon> for CardanoNodeLedgerStateSignableBuilder {
    async fn compute_protocol_message(
        &self,
        beacon: CardanoDbBeacon,
    ) -> StdResult<ProtocolMessage> {
        info!(
            self.logger,
            "Computing Cardano node ledger state for epoch = '{}' and immutable file number = '{}' at directory '{}'",
            beacon.epoch,
            beacon.immutable_file_number,
            self.dirpath.display()
        );

        //Create a tmp ledger directory
        let temp_ledger_directory = self.dirpath.join(format!("temp-ledger-{}", beacon.epoch));
        fs::create_dir(&temp_ledger_directory).with_context(|| {
            format!(
                "Can not create temporary ledger directory: '{}'",
                temp_ledger_directory.display()
            )
        })?;

        //List ledger files to be signed
        let paths_to_include = self
            .get_files_and_directories_for_ledger_snapshot(&temp_ledger_directory)
            .await?;

        info!(
            self.logger,
            "Gonna Hash ledger states files {} from directory '{}'",
            paths_to_include
                .iter()
                .map(|path| path.display().to_string())
                .collect::<Vec<String>>()
                .join(", "),
            temp_ledger_directory.display()
        );

        //Hash ledger files
        let manifest =
            AncillaryFilesManifest::from_paths(&temp_ledger_directory, paths_to_include).await?;

        if let Err(e) = fs::remove_dir_all(&temp_ledger_directory) {
            warn!(
                self.logger, "Failed to remove temporary ledger directory '{}'", temp_ledger_directory.display();
                "error" => ?e
            );
        }

        //TODO: do we have to compute_ancillary_manifest_signature to sign ?

        let hash = hex::encode(manifest.compute_hash());

        info!(
            self.logger,
            "hash-'{}'-epoch-'{}'-immutable-number-'{}-tmp-ledger-dir-'{}'",
            hash,
            beacon.epoch,
            beacon.immutable_file_number,
            temp_ledger_directory.display()
        );

        let mut protocol_message = ProtocolMessage::new();
        //TODO: do we have to create a proper ProtocolMessagePartKey for that ? -> yes LedgerSnapshotDigest
        //TODO: do we also want to add Epoch in message_parts ?
        protocol_message.set_message_part(ProtocolMessagePartKey::SnapshotDigest, hash);

        Ok(protocol_message)
    }
}
