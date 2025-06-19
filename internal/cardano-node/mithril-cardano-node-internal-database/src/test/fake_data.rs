//! Fake data builders for testing.
use std::path::PathBuf;

use mithril_common::entities::ImmutableFileNumber;

use crate::entities::*;

/// Fake [ImmutableFile], bypass the checks done by [ImmutableFile::new].
pub fn immutable_file<T: Into<String>>(
    path: PathBuf,
    number: ImmutableFileNumber,
    filename: T,
) -> ImmutableFile {
    ImmutableFile {
        path,
        number,
        filename: filename.into(),
    }
}
