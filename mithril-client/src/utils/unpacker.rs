use anyhow::Context;
use flate2::read::GzDecoder;
use flume::Receiver;
use std::path::Path;
use tar::Archive;

use crate::common::CompressionAlgorithm;
use crate::utils::StreamReader;
use crate::MithrilResult;

/// Check and unpack a downloaded archive in a given directory.
#[derive(Default)]
pub struct SnapshotUnpacker;

impl SnapshotUnpacker {
    /// Unpack the snapshot from the given stream into the given directory.
    pub fn unpack_snapshot(
        &self,
        stream: Receiver<Vec<u8>>,
        compression_algorithm: CompressionAlgorithm,
        unpack_dir: &Path,
    ) -> MithrilResult<()> {
        let input = StreamReader::new(stream);

        match compression_algorithm {
            CompressionAlgorithm::Gzip => {
                let gzip_decoder = GzDecoder::new(input);
                let mut snapshot_archive = Archive::new(gzip_decoder);
                snapshot_archive.unpack(unpack_dir).with_context(|| {
                    format!(
                        "Could not unpack from streamed data snapshot to directory '{}'",
                        unpack_dir.display()
                    )
                })?;
            }
            CompressionAlgorithm::Zstandard => {
                let zstandard_decoder = zstd::Decoder::new(input)
                    .with_context(|| "Unpack failed: Create Zstandard decoder error")?;
                let mut snapshot_archive = Archive::new(zstandard_decoder);
                snapshot_archive.unpack(unpack_dir).with_context(|| {
                    format!(
                        "Could not unpack from streamed data snapshot to directory '{}'",
                        unpack_dir.display()
                    )
                })?;
            }
        };

        Ok(())
    }
}
