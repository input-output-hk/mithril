use anyhow::Context;
use flate2::read::GzDecoder;
use flume::Receiver;
use human_bytes::human_bytes;
use std::{
    fs::{create_dir_all, remove_dir},
    path::{Path, PathBuf},
};
use tar::Archive;
use thiserror::Error;

use crate::utils::StreamReader;
use mithril_common::{entities::CompressionAlgorithm, StdError, StdResult};

/// Check and unpack a downloaded archive in a given directory.
#[derive(Default)]
pub struct SnapshotUnpacker;

/// Errors tied with the SnapshotUnpacker.
#[derive(Debug, Error)]
pub enum SnapshotUnpackerError {
    /// Not enough space on the disk. There should be at least the ratio given for the
    /// used algorithm (see [CompressionAlgorithm::free_space_snapshot_ratio]) times
    /// the size of the archive to download to ensure it could be unpacked safely.
    #[error("There is only {} remaining in directory '{}' to store and unpack a {} large archive.", human_bytes(*left_space), pathdir.display(), human_bytes(*archive_size))]
    NotEnoughSpace {
        /// Left space on device
        left_space: f64,

        /// Specified location
        pathdir: PathBuf,

        /// Packed snapshot size
        archive_size: f64,
    },

    /// The directory where the files from snapshot are expanded already exists.
    /// An error is raised because it lets the user a chance to preserve a
    /// previous work.
    #[error("Unpack directory '{0}' already exists, please move or delete it.")]
    UnpackDirectoryAlreadyExists(PathBuf),

    /// Cannot write in the given directory.
    #[error("Unpack directory '{0}' is not writable. (underlying error: « {1} »).")]
    UnpackDirectoryIsNotWritable(PathBuf, StdError),

    /// Unpacking error
    #[error("Could not unpack from streamed data snapshot to directory '{dirpath}'. Error: « {error:?} ».")]
    UnpackFailed {
        /// Location where the archive is to be extracted.
        dirpath: PathBuf,

        /// Subsystem error
        error: StdError,
    },
}

impl SnapshotUnpacker {
    /// Check all prerequisites are met before starting to download and unpack
    /// big snapshot archive.
    pub fn check_prerequisites(
        &self,
        pathdir: &Path,
        size: u64,
        compressionn_algorithm: CompressionAlgorithm,
    ) -> StdResult<()> {
        if pathdir.exists() {
            return Err(
                SnapshotUnpackerError::UnpackDirectoryAlreadyExists(pathdir.to_owned()).into(),
            );
        }
        create_dir_all(pathdir).map_err(|e| {
            SnapshotUnpackerError::UnpackDirectoryIsNotWritable(pathdir.to_owned(), e.into())
        })?;
        let free_space = fs2::available_space(pathdir)? as f64;
        remove_dir(pathdir)?;

        if free_space < compressionn_algorithm.free_space_snapshot_ratio() * size as f64 {
            return Err(SnapshotUnpackerError::NotEnoughSpace {
                left_space: free_space,
                pathdir: pathdir.to_owned(),
                archive_size: size as f64,
            }
            .into());
        }

        Ok(())
    }

    /// Unpack the snapshot from the given stream into the given directory.
    pub fn unpack_snapshot(
        &self,
        stream: Receiver<Vec<u8>>,
        compression_algorithm: CompressionAlgorithm,
        unpack_dir: &Path,
    ) -> StdResult<()> {
        let input = StreamReader::new(stream);

        match compression_algorithm {
            CompressionAlgorithm::Gzip => {
                let gzip_decoder = GzDecoder::new(input);
                let mut snapshot_archive = Archive::new(gzip_decoder);
                snapshot_archive.unpack(unpack_dir).map_err(|e| {
                    SnapshotUnpackerError::UnpackFailed {
                        dirpath: unpack_dir.to_owned(),
                        error: e.into(),
                    }
                })?;
            }
            CompressionAlgorithm::Zstandard => {
                let zstandard_decoder = zstd::Decoder::new(input)
                    .with_context(|| "Unpack failed: Create Zstandard decoder error")?;
                let mut snapshot_archive = Archive::new(zstandard_decoder);
                snapshot_archive.unpack(unpack_dir).map_err(|e| {
                    SnapshotUnpackerError::UnpackFailed {
                        dirpath: unpack_dir.to_owned(),
                        error: e.into(),
                    }
                })?;
            }
        };

        Ok(())
    }
}
