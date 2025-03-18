use std::fs::File;
use std::path::{Path, PathBuf};

use mithril_common::digesters::{ComputedImmutablesDigests, DummyCardanoDb};
use mithril_common::entities::CompressionAlgorithm;
use mithril_common::messages::CardanoDatabaseDigestListItemMessage;

/// Compress the given db into a zstd archive in the given target directory.
///
/// return the path to the compressed archive.
pub fn build_fake_immutable_files_full_snapshot(
    cardano_db: &DummyCardanoDb,
    target_dir: &Path,
) -> PathBuf {
    use std::fs::File;

    let snapshot_name = format!(
        "db-i{}.{}",
        cardano_db.get_immutable_files().len(),
        CompressionAlgorithm::Zstandard.tar_file_extension()
    );
    let target_file = target_dir.join(snapshot_name);
    let tar_file = File::create(&target_file).unwrap();
    let enc = zstd::Encoder::new(tar_file, 3).unwrap();
    let mut tar = tar::Builder::new(enc);

    tar.append_dir_all(".", cardano_db.get_immutable_dir().parent().unwrap())
        .unwrap();

    let zstd = tar.into_inner().unwrap();
    zstd.finish().unwrap();

    target_file
}

pub fn build_cardano_db_v2_snapshot_archives(
    cardano_db: &DummyCardanoDb,
    target_dir: &Path,
    computed_immutables_digests: ComputedImmutablesDigests,
) -> PathBuf {
    let target_dir = target_dir.join("archives");
    std::fs::create_dir_all(&target_dir).unwrap();

    build_immutable_files_archives(cardano_db, &target_dir);
    build_ancillary_files_archive(cardano_db, &target_dir);
    build_digests_json_file(&target_dir, computed_immutables_digests);
    target_dir
}

pub fn build_immutable_files_archives(cardano_db: &DummyCardanoDb, target_dir: &Path) {
    let last_immutable_number = cardano_db.last_immutable_number().unwrap();

    let immutable_dir = cardano_db.get_immutable_dir();
    for immutable_number in 1..=last_immutable_number {
        let immutable_archive_name = format!(
            "{:05}.{}",
            immutable_number,
            CompressionAlgorithm::Zstandard.tar_file_extension()
        );
        let target_file = target_dir.join(immutable_archive_name);
        let tar_file = File::create(&target_file).unwrap();
        let enc = zstd::Encoder::new(tar_file, 3).unwrap();
        let mut tar = tar::Builder::new(enc);

        for extension in &[".chunk", ".primary", ".secondary"] {
            let file_name = format!("{:05}{}", immutable_number, extension);
            let file_path = immutable_dir.join(&file_name);

            let archive_path = format!("immutable/{file_name}");
            tar.append_path_with_name(&file_path, &archive_path)
                .unwrap();
        }

        let zstd = tar.into_inner().unwrap();
        zstd.finish().unwrap();
    }
}

pub fn build_ancillary_files_archive(cardano_db: &DummyCardanoDb, target_dir: &Path) {
    let ancillary_immutable_number = cardano_db.last_immutable_number().unwrap() + 1;
    let immutable_dir = cardano_db.get_immutable_dir();
    let volatile_dir = cardano_db.get_volatile_dir();
    let ledger_dir = cardano_db.get_ledger_dir();
    let archive_name = format!(
        "ancillary.{}",
        CompressionAlgorithm::Zstandard.tar_file_extension()
    );
    let target_file = target_dir.join(archive_name);
    let tar_file = File::create(&target_file).unwrap();
    let enc = zstd::Encoder::new(tar_file, 3).unwrap();
    let mut tar = tar::Builder::new(enc);

    tar.append_path_with_name(&volatile_dir, "volatile")
        .unwrap();

    tar.append_path_with_name(&ledger_dir, "ledger").unwrap();

    for extension in &[".chunk", ".primary", ".secondary"] {
        let file_name = format!("{:05}{}", ancillary_immutable_number, extension);
        let file_path = immutable_dir.join(&file_name);

        let archive_path = format!("immutable/{file_name}");
        tar.append_path_with_name(&file_path, &archive_path)
            .unwrap();
    }

    let zstd = tar.into_inner().unwrap();
    zstd.finish().unwrap();
}

pub fn build_digests_json_file(
    target_dir: &Path,
    computed_immutables_digests: ComputedImmutablesDigests,
) {
    let target_file = target_dir.join("digests.json");

    let immutable_digest_messages = computed_immutables_digests
        .entries
        .iter()
        .map(
            |(immutable_file, digest)| CardanoDatabaseDigestListItemMessage {
                immutable_file_name: immutable_file.filename.clone(),
                digest: digest.to_string(),
            },
        )
        .collect::<Vec<_>>();

    serde_json::to_writer(
        File::create(target_file).unwrap(),
        &immutable_digest_messages,
    )
    .unwrap();
}
