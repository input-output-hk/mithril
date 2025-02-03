use anyhow::{anyhow, Context};
use clap::Parser;
use cli_table::{print_stdout, Cell, CellStruct, Table};

use crate::{
    commands::{client_builder_with_fallback_genesis_key, SharedArgs},
    utils::{CardanoDbUtils, ExpanderUtils},
    CommandContext,
};

use mithril_client::{
    common::{AncillaryLocation, DigestLocation, ImmutablesLocation, MultiFilesUri},
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

            for digest_location in digest_location_rows(&cardano_db_message.locations.digests) {
                cardano_db_table.push(digest_location);
            }

            for immutables_location in
                immutables_location_rows(&cardano_db_message.locations.immutables)
            {
                cardano_db_table.push(immutables_location);
            }

            for ancillary_location in
                ancillary_location_rows(&cardano_db_message.locations.ancillary)
            {
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

fn digest_location_rows(locations: &[DigestLocation]) -> Vec<Vec<CellStruct>> {
    let location_name = "Digest location";

    locations
        .iter()
        .enumerate()
        .map(|(index, location)| match location {
            DigestLocation::Aggregator { uri } => {
                vec![
                    format!("{location_name} ({})", index + 1).cell(),
                    format!("Aggregator, uri: \"{}\"", uri).cell(),
                ]
            }
            DigestLocation::CloudStorage { uri } => {
                vec![
                    format!("{location_name} ({})", index + 1).cell(),
                    format!("CloudStorage, uri: \"{}\"", uri).cell(),
                ]
            }
        })
        .collect()
}

fn immutables_location_rows(locations: &[ImmutablesLocation]) -> Vec<Vec<CellStruct>> {
    let location_name = "Immutables location";

    locations
        .iter()
        .enumerate()
        .map(|(index, location)| match location {
            ImmutablesLocation::CloudStorage { uri } => match uri {
                MultiFilesUri::Template(template_uri) => {
                    vec![
                        format!("{location_name} ({})", index + 1).cell(),
                        format!("CloudStorage, template_uri: \"{}\"", template_uri.0).cell(),
                    ]
                }
            },
        })
        .collect()
}

fn ancillary_location_rows(locations: &[AncillaryLocation]) -> Vec<Vec<CellStruct>> {
    let location_name = "Ancillary location";

    locations
        .iter()
        .enumerate()
        .map(|(index, location)| match location {
            AncillaryLocation::CloudStorage { uri } => {
                vec![
                    format!("{location_name} ({})", index + 1).cell(),
                    format!("CloudStorage, uri: \"{}\"", uri).cell(),
                ]
            }
        })
        .collect()
}

#[cfg(test)]
mod tests {
    use mithril_client::common::TemplateUri;

    use super::*;

    #[test]
    fn digest_location_rows_when_no_uri_found() {
        let rows = digest_location_rows(&[]);

        assert!(rows.is_empty());
    }

    #[test]
    fn digest_location_rows_when_uris_found() {
        let locations = vec![
            DigestLocation::Aggregator {
                uri: "http://aggregator.net/".to_string(),
            },
            DigestLocation::CloudStorage {
                uri: "http://cloudstorage.com/".to_string(),
            },
        ];

        let rows = digest_location_rows(&locations);
        assert!(rows.len() == 2);

        let table = rows.table();
        let rows_rendered = table.display().unwrap().to_string();

        assert!(rows_rendered.contains("Digest location (1)"));
        assert!(rows_rendered.contains("CloudStorage, uri: \"http://cloudstorage.com/\""));
        assert!(rows_rendered.contains("Digest location (2)"));
        assert!(rows_rendered.contains("Aggregator, uri: \"http://aggregator.net/\""));
    }

    #[test]
    fn immutables_location_rows_when_no_uri_found() {
        let rows = immutables_location_rows(&[]);

        assert!(rows.is_empty());
    }

    #[test]
    fn immutables_location_row_returns_some_when_uri_found() {
        let locations = vec![
            ImmutablesLocation::CloudStorage {
                uri: MultiFilesUri::Template(TemplateUri("http://cloudstorage1.com/".to_string())),
            },
            ImmutablesLocation::CloudStorage {
                uri: MultiFilesUri::Template(TemplateUri("http://cloudstorage2.com/".to_string())),
            },
        ];

        let rows = immutables_location_rows(&locations);

        assert!(rows.len() == 2);

        let table = rows.table();
        let rows_rendered = table.display().unwrap().to_string();

        assert!(rows_rendered.contains("Immutables location (1)"));
        assert!(rows_rendered.contains("CloudStorage, template_uri: \"http://cloudstorage1.com/\""));
        assert!(rows_rendered.contains("Immutables location (2)"));
        assert!(rows_rendered.contains("CloudStorage, template_uri: \"http://cloudstorage2.com/\""));
    }

    #[test]
    fn ancillary_location_rows_when_no_uri_found() {
        let rows = ancillary_location_rows(&[]);

        assert!(rows.is_empty());
    }

    #[test]
    fn ancillary_location_rows_when_uris_found() {
        let locations = vec![
            AncillaryLocation::CloudStorage {
                uri: "http://cloudstorage1.com/".to_string(),
            },
            AncillaryLocation::CloudStorage {
                uri: "http://cloudstorage2.com/".to_string(),
            },
        ];

        let rows = ancillary_location_rows(&locations);

        assert!(rows.len() == 2);

        let table = rows.table();
        let rows_rendered = table.display().unwrap().to_string();

        assert!(rows_rendered.contains("Ancillary location (1)"));
        assert!(rows_rendered.contains("CloudStorage, uri: \"http://cloudstorage1.com/\""));
        assert!(rows_rendered.contains("Ancillary location (2)"));
        assert!(rows_rendered.contains("CloudStorage, uri: \"http://cloudstorage2.com/\""));
    }
}
