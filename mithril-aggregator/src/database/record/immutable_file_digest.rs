use mithril_common::entities::{HexEncodedDigest, ImmutableFileName};
use mithril_persistence::sqlite::{HydrationError, Projection, SourceAlias, SqLiteEntity};
use sqlite::Row;

/// ImmutableFileDigestRecord is the record that stores the digest of an immutable file.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct ImmutableFileDigestRecord {
    /// Immutable file name
    pub immutable_file_name: ImmutableFileName,

    /// Digest of an immutable file
    pub digest: HexEncodedDigest,
}

impl ImmutableFileDigestRecord {
    /// Construct a [Projection] that will allow to hydrate this `ImmutableFileDigestRecord` and expend table alias.
    pub fn expand_projection(table: &str) -> String {
        let aliases = SourceAlias::new(&[("{:immutable_file_digest:}", table)]);
        Self::get_projection().expand(aliases)
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
