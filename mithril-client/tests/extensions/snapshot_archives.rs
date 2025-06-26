use std::fs::File;
use std::path::{Path, PathBuf};

use mithril_cardano_node_internal_database::digesters::ComputedImmutablesDigests;
use mithril_cardano_node_internal_database::entities::AncillaryFilesManifest;
use mithril_cardano_node_internal_database::test::DummyCardanoDb;
use mithril_cardano_node_internal_database::{immutable_trio_names, IMMUTABLE_DIR, LEDGER_DIR};

use mithril_common::crypto_helper::{ManifestSigner, ManifestVerifierSecretKey};
use mithril_common::entities::{CompressionAlgorithm, ImmutableFileNumber};
use mithril_common::messages::CardanoDatabaseDigestListItemMessage;

pub async fn build_cardano_db_v1_snapshot_archives(
    cardano_db: &DummyCardanoDb,
    target_dir: &Path,
    ancillary_manifest_signing_key: ManifestVerifierSecretKey,
) -> PathBuf {
    let target_dir = target_dir.join("archives");
    std::fs::create_dir_all(&target_dir).unwrap();

    build_all_completed_immutables_snapshot(cardano_db, &target_dir);
    build_ancillary_files_archive(cardano_db, &target_dir, ancillary_manifest_signing_key).await;
    target_dir
}

pub fn build_all_completed_immutables_snapshot(
    cardano_db: &DummyCardanoDb,
    target_dir: &Path,
) -> PathBuf {
    use std::fs::File;

    let snapshot_name = format!(
        "completed_immutables.{}",
        CompressionAlgorithm::Zstandard.tar_file_extension()
    );
    let target_file = target_dir.join(snapshot_name);
    let tar_file = File::create(&target_file).unwrap();
    let enc = zstd::Encoder::new(tar_file, 3).unwrap();
    let mut tar = tar::Builder::new(enc);

    let last_immutable_number = cardano_db.last_immutable_number().unwrap();

    let immutable_dir = cardano_db.get_immutable_dir();
    for immutable_number in 1..=last_immutable_number {
        append_immutable_trio(&mut tar, immutable_number, immutable_dir);
    }

    let zstd = tar.into_inner().unwrap();
    zstd.finish().unwrap();

    target_file
}

pub async fn build_cardano_db_v2_snapshot_archives(
    cardano_db: &DummyCardanoDb,
    target_dir: &Path,
    computed_immutables_digests: ComputedImmutablesDigests,
    ancillary_manifest_signing_key: ManifestVerifierSecretKey,
) -> PathBuf {
    let target_dir = target_dir.join("archives");
    std::fs::create_dir_all(&target_dir).unwrap();

    build_immutable_files_archives(cardano_db, &target_dir);
    build_ancillary_files_archive(cardano_db, &target_dir, ancillary_manifest_signing_key).await;
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

        append_immutable_trio(&mut tar, immutable_number, immutable_dir);

        let zstd = tar.into_inner().unwrap();
        zstd.finish().unwrap();
    }
}

fn append_immutable_trio<T: std::io::Write>(
    tar: &mut tar::Builder<T>,
    immutable_file_number: ImmutableFileNumber,
    immutables_dir: &Path,
) {
    for file_name in immutable_trio_names(immutable_file_number) {
        let file_path = immutables_dir.join(&file_name);

        let archive_path = format!("immutable/{file_name}");
        tar.append_path_with_name(&file_path, &archive_path).unwrap();
    }
}

pub async fn build_ancillary_files_archive(
    cardano_db: &DummyCardanoDb,
    target_dir: &Path,
    ancillary_manifest_signing_key: ManifestVerifierSecretKey,
) {
    let db_dir = cardano_db.get_dir();
    let ancillary_immutable_number = cardano_db.last_immutable_number().unwrap() + 1;
    let last_ledger_state_snapshot = cardano_db
        .get_ledger_state_snapshots()
        .last()
        .expect("Given db should have at least one ledger file");
    let archive_name = format!(
        "ancillary.{}",
        CompressionAlgorithm::Zstandard.tar_file_extension()
    );
    let target_file = target_dir.join(archive_name);
    let tar_file = File::create(&target_file).unwrap();
    let enc = zstd::Encoder::new(tar_file, 3).unwrap();
    let mut tar = tar::Builder::new(enc);

    let mut files_to_include = vec![
        PathBuf::from(IMMUTABLE_DIR).join(format!("{ancillary_immutable_number:05}.chunk")),
        PathBuf::from(IMMUTABLE_DIR).join(format!("{ancillary_immutable_number:05}.primary")),
        PathBuf::from(IMMUTABLE_DIR).join(format!("{ancillary_immutable_number:05}.secondary")),
    ];
    files_to_include.extend(
        last_ledger_state_snapshot
            .get_files_relative_path()
            .into_iter()
            .map(|p| PathBuf::from(LEDGER_DIR).join(p)),
    );

    let ancillary_manifest = build_ancillary_manifest(
        db_dir,
        files_to_include.clone(),
        ancillary_manifest_signing_key,
    )
    .await;

    for file in &files_to_include {
        tar.append_path_with_name(db_dir.join(file), file).unwrap();
    }
    tar.append_path_with_name(
        &ancillary_manifest,
        AncillaryFilesManifest::ANCILLARY_MANIFEST_FILE_NAME,
    )
    .unwrap();

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

async fn build_ancillary_manifest(
    cardano_db_dir: &Path,
    files_in_manifest: Vec<PathBuf>,
    ancillary_manifest_signing_key: ManifestVerifierSecretKey,
) -> PathBuf {
    let signer = ManifestSigner::from_secret_key(ancillary_manifest_signing_key);

    let mut manifest = AncillaryFilesManifest::from_paths(cardano_db_dir, files_in_manifest)
        .await
        .unwrap();
    manifest.set_signature(signer.sign(&manifest.compute_hash()));

    let target_file = cardano_db_dir.join(AncillaryFilesManifest::ANCILLARY_MANIFEST_FILE_NAME);
    serde_json::to_writer(File::create(&target_file).unwrap(), &manifest).unwrap();
    target_file
}
