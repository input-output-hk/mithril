use sqlite::Row;

use mithril_common::entities::{HexEncodedDigest, ImmutableFileName};
use mithril_persistence::sqlite::{HydrationError, Projection, SourceAlias, SqLiteEntity};

/// ImmutableFileDigestRecord is the record that stores the digest of an immutable file.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct ImmutableFileDigestRecord {
    /// Immutable file name
    pub immutable_file_name: ImmutableFileName,

    /// Digest of an immutable file
    pub digest: HexEncodedDigest,
}

impl ImmutableFileDigestRecord {
    /// Construct a [Projection] that will allow to hydrate this `CertificatePendingRecord` and expend table alias.
    pub fn expand_projection(table: &str) -> String {
        let aliases = SourceAlias::new(&[("{:immutable_file_digest:}", table)]);
        Self::get_projection().expand(aliases)
    }

    #[cfg(test)]
    /// Create a dumb ImmutableFileDigestRecord instance mainly for test purposes
    pub fn dummy() -> Self {
        Self {
            immutable_file_name: "123.chunk".to_string(),
            digest: "dummy_digest".to_string(),
        }
    }
}

impl SqLiteEntity for ImmutableFileDigestRecord {
    fn hydrate(row: Row) -> Result<Self, HydrationError>
    where
        Self: Sized,
    {
        let immutable_file_name = row.read::<&str, _>(0).to_string();
        let digest = row.read::<&str, _>(1).to_string();

        Ok(Self {
            immutable_file_name,
            digest,
        })
    }

    fn get_projection() -> Projection {
        Projection::from(&[
            (
                "immutable_file_name",
                "{:immutable_file_digest:}.immutable_file_name",
                "text",
            ),
            ("digest", "{:immutable_file_digest:}.digest", "text"),
        ])
    }
}
