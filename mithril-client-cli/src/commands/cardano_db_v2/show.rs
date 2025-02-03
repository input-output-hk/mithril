use anyhow::{anyhow, Context};
use clap::Parser;
use cli_table::{print_stdout, Cell, CellStruct, Table};

use crate::{
    commands::{client_builder_with_fallback_genesis_key, SharedArgs},
    utils::{CardanoDbUtils, ExpanderUtils},
    CommandContext,
};

use mithril_client::{
    common::{
        AncillaryLocation, AncillaryLocationDiscriminants, DigestLocation,
        DigestLocationDiscriminants, ImmutablesLocation, ImmutablesLocationDiscriminants,
        MultiFilesUri,
    },
    MithrilResult,
};

/// Clap command to show a given cardano db
#[derive(Parser, Debug, Clone)]
pub struct CardanoDbShowCommand {
    #[clap(flatten)]
    shared_args: SharedArgs,

    /// Cardano DB snapshot hash.
    ///
    /// If `latest` is specified as hash, the command will return the latest cardano db snapshot.
    hash: String,
}

impl CardanoDbShowCommand {
    /// Is JSON output enabled
    pub fn is_json_output_enabled(&self) -> bool {
        self.shared_args.json
    }

    /// Cardano DB snapshot Show command
    pub async fn execute(&self, context: CommandContext) -> MithrilResult<()> {
        let params = context.config_parameters()?;
        let client = client_builder_with_fallback_genesis_key(&params)?
            .with_logger(context.logger().clone())
            .build()?;

        let get_list_of_artifact_ids = || async {
            let cardano_dbs = client.cardano_database().list().await.with_context(|| {
                "Can not get the list of artifacts while retrieving the latest cardano db snapshot hash"
            })?;

            Ok(cardano_dbs
                .iter()
                .map(|cardano_db| cardano_db.hash.to_owned())
                .collect::<Vec<String>>())
        };

        let cardano_db_message = client
            .cardano_database()
            .get(
                &ExpanderUtils::expand_eventual_id_alias(&self.hash, get_list_of_artifact_ids())
                    .await?,
            )
            .await?
            .ok_or_else(|| anyhow!("Cardano DB snapshot not found for hash: '{}'", &self.hash))?;

        if self.is_json_output_enabled() {
            println!("{}", serde_json::to_string(&cardano_db_message)?);
        } else {
            let mut cardano_db_table = vec![
                vec![
                    "Epoch".cell(),
                    format!("{}", &cardano_db_message.beacon.epoch).cell(),
                ],
                vec![
                    "Immutable File Number".cell(),
                    format!("{}", &cardano_db_message.beacon.immutable_file_number).cell(),
                ],
                vec!["Hash".cell(), cardano_db_message.hash.cell()],
                vec!["Merkle root".cell(), cardano_db_message.merkle_root.cell()],
                vec![
                    "Database size".cell(),
                    CardanoDbUtils::format_bytes_to_gigabytes(
                        cardano_db_message.total_db_size_uncompressed,
                    )
                    .cell(),
                ],
                vec![
                    "Cardano node version".cell(),
                    cardano_db_message.cardano_node_version.cell(),
                ],
            ];

            let mut digest_location_index = 1;
            for digest_location_type in [
                DigestLocationDiscriminants::Aggregator,
                DigestLocationDiscriminants::CloudStorage,
            ] {
                if let Some(digest_location) = digest_location_row(
                    digest_location_index,
                    digest_location_type,
                    &cardano_db_message.locations.digests,
                ) {
                    cardano_db_table.push(digest_location);
                    digest_location_index += 1;
                }
            }

            if let Some(immutables_location) = immutables_location_row(
                ImmutablesLocationDiscriminants::CloudStorage,
                &cardano_db_message.locations.immutables,
            ) {
                cardano_db_table.push(immutables_location);
            }

            if let Some(ancillary_location) = ancillary_location_row(
                AncillaryLocationDiscriminants::CloudStorage,
                &cardano_db_message.locations.ancillary,
            ) {
                cardano_db_table.push(ancillary_location);
            }

            cardano_db_table.push(vec![
                "Created".cell(),
                cardano_db_message.created_at.to_string().cell(),
            ]);
            cardano_db_table.push(vec![
                "Compression Algorithm".cell(),
                format!("{}", &cardano_db_message.compression_algorithm).cell(),
            ]);

            print_stdout(cardano_db_table.table())?;
        }

        Ok(())
    }
}

fn digest_location_row(
    index: usize,
    location_type: DigestLocationDiscriminants,
    locations: &[DigestLocation],
) -> Option<Vec<CellStruct>> {
    let uris = locations
        .iter()
        .filter_map(|location| match (location, location_type) {
            (DigestLocation::Aggregator { uri }, DigestLocationDiscriminants::Aggregator) => {
                Some(format!("uri: \"{}\"", uri))
            }
            (DigestLocation::CloudStorage { uri }, DigestLocationDiscriminants::CloudStorage) => {
                Some(format!("uri: \"{}\"", uri))
            }
            _ => None,
        })
        .collect::<Vec<String>>()
        .join(",");

    if uris.is_empty() {
        None
    } else {
        Some(vec![
            format!("Digest location ({index})").cell(),
            format!("{location_type:?}, {uris}").cell(),
        ])
    }
}

fn immutables_location_row(
    location_type: ImmutablesLocationDiscriminants,
    locations: &[ImmutablesLocation],
) -> Option<Vec<CellStruct>> {
    let uris = locations
        .iter()
        .map(|location| match (location, location_type) {
            (
                ImmutablesLocation::CloudStorage { uri },
                ImmutablesLocationDiscriminants::CloudStorage,
            ) => match uri {
                MultiFilesUri::Template(template_uri) => {
                    format!("template_uri: \"{}\"", template_uri.0)
                }
            },
        })
        .collect::<Vec<String>>()
        .join(",");

    if uris.is_empty() {
        None
    } else {
        Some(vec![
            "Immutables location".to_string().cell(),
            format!(
                "{:?}, {}",
                ImmutablesLocationDiscriminants::CloudStorage,
                uris
            )
            .cell(),
        ])
    }
}

fn ancillary_location_row(
    location_type: AncillaryLocationDiscriminants,
    locations: &[AncillaryLocation],
) -> Option<Vec<CellStruct>> {
    let uris = locations
        .iter()
        .map(|location| match (location, location_type) {
            (
                AncillaryLocation::CloudStorage { uri },
                AncillaryLocationDiscriminants::CloudStorage,
            ) => format!("uri: \"{}\"", uri),
        })
        .collect::<Vec<String>>()
        .join(",");

    if uris.is_empty() {
        None
    } else {
        Some(vec![
            "Ancillary location".to_string().cell(),
            format!(
                "{:?}, {}",
                AncillaryLocationDiscriminants::CloudStorage,
                uris
            )
            .cell(),
        ])
    }
}

#[cfg(test)]
mod tests {
    use mithril_client::common::TemplateUri;

    use super::*;

    #[test]
    fn digest_location_row_returns_none_when_no_uri_found_for_location_type() {
        let locations = vec![DigestLocation::Aggregator {
            uri: "http://aggregator.net/".to_string(),
        }];

        let row = digest_location_row(123, DigestLocationDiscriminants::CloudStorage, &locations);

        assert!(row.is_none());
    }

    #[test]
    fn digest_location_row_returns_some_when_uri_found_for_location_type() {
        let locations = vec![
            DigestLocation::Aggregator {
                uri: "http://aggregator.net/".to_string(),
            },
            DigestLocation::CloudStorage {
                uri: "http://cloudstorage.com/".to_string(),
            },
        ];

        let row = digest_location_row(123, DigestLocationDiscriminants::CloudStorage, &locations);
        assert!(row.is_some());

        let row = digest_location_row(456, DigestLocationDiscriminants::Aggregator, &locations);
        assert!(row.is_some());
    }

    #[test]
    fn immutables_location_row_returns_none_when_no_uri_found_for_location_type() {
        let row = immutables_location_row(ImmutablesLocationDiscriminants::CloudStorage, &[]);

        assert!(row.is_none());
    }

    #[test]
    fn immutables_location_row_returns_some_when_uri_found_for_location_type() {
        let locations = vec![ImmutablesLocation::CloudStorage {
            uri: MultiFilesUri::Template(TemplateUri("http://cloudstorage.com/".to_string())),
        }];

        let row =
            immutables_location_row(ImmutablesLocationDiscriminants::CloudStorage, &locations);

        assert!(row.is_some());
    }

    #[test]
    fn ancillary_location_row_returns_none_when_no_uri_found_for_location_type() {
        let row = ancillary_location_row(AncillaryLocationDiscriminants::CloudStorage, &[]);

        assert!(row.is_none());
    }

    #[test]
    fn ancillary_location_row_returns_some_when_uri_found_for_location_type() {
        let locations = vec![AncillaryLocation::CloudStorage {
            uri: "http://cloudstorage.com/".to_string(),
        }];

        let row = ancillary_location_row(AncillaryLocationDiscriminants::CloudStorage, &locations);

        assert!(row.is_some());
    }
}
