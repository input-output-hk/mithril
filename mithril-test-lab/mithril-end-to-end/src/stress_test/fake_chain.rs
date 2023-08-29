use slog_scope::debug;
use std::{fs::File, io::Write, path::Path};

use mithril_common::{entities::Epoch, test_utils::MithrilFixture};

pub fn set_stake_distribution(
    mock_stake_distribution_file_path: &Path,
    signers_fixture: &MithrilFixture,
) {
    let mock_stake_distribution_file = File::create(mock_stake_distribution_file_path).unwrap();
    serde_json::to_writer(
        &mock_stake_distribution_file,
        &signers_fixture.cardano_cli_stake_distribution(),
    )
    .expect("Writing the stake distribution into a file for the mock cardano cli failed");
}

pub fn set_epoch(mock_epoch_file_path: &Path, epoch: Epoch) {
    let mock_epoch_file = File::create(mock_epoch_file_path).unwrap();
    write!(&mock_epoch_file, "{}", *epoch)
        .expect("Writing the epoch into a file for the mock cardano cli failed");
    debug!("New Epoch: {epoch}");
}
