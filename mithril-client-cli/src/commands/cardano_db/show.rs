use anyhow::{anyhow, Context};
use clap::Parser;
use cli_table::{print_stdout, Cell, CellStruct, Table};

use mithril_client::{
    common::{AncillaryLocation, DigestLocation, ImmutablesLocation, MultiFilesUri},
    Client, MithrilResult,
};

use crate::{
    commands::{
        cardano_db::CardanoDbCommandsBackend, client_builder_with_fallback_genesis_key, SharedArgs,
    },
    utils::{CardanoDbUtils, ExpanderUtils},
    CommandContext,
};

/// Clap command to show a given Cardano db
#[derive(Parser, Debug, Clone)]
pub struct CardanoDbShowCommand {
    #[clap(flatten)]
    shared_args: SharedArgs,

    #[arg(short, long, value_enum, default_value_t)]
    backend: CardanoDbCommandsBackend,

    /// Digest of the Cardano db snapshot to show or `latest` for the latest artifact
    digest: String,
}

impl CardanoDbShowCommand {
    /// Is JSON output enabled
    pub fn is_json_output_enabled(&self) -> bool {
        self.shared_args.json
    }

    /// Cardano DB Show command
    pub async fn execute(&self, context: CommandContext) -> MithrilResult<()> {
        let params = context.config_parameters()?;
        let client = client_builder_with_fallback_genesis_key(&params)?
            .with_logger(context.logger().clone())
            .build()?;

        match self.backend {
            CardanoDbCommandsBackend::V1 => self.print_v1(client).await?,
            CardanoDbCommandsBackend::V2 => self.print_v2(client).await?,
        }

        Ok(())
    }

    async fn print_v1(&self, client: Client) -> MithrilResult<()> {
        let get_list_of_artifact_ids = || async {
            let cardano_dbs = client.cardano_database().list().await.with_context(|| {
                "Can not get the list of artifacts while retrieving the latest cardano db digest"
            })?;

            Ok(cardano_dbs
                .iter()
                .map(|cardano_db| cardano_db.digest.to_owned())
                .collect::<Vec<String>>())
        };

        let cardano_db_message = client
            .cardano_database()
            .get(
                &ExpanderUtils::expand_eventual_id_alias(&self.digest, get_list_of_artifact_ids())
                    .await?,
            )
            .await?
            .ok_or_else(|| anyhow!("Cardano DB not found for digest: '{}'", &self.digest))?;

        if self.is_json_output_enabled() {
            println!("{}", serde_json::to_string(&cardano_db_message)?);
        } else {
            let cardano_db_table = vec![
                vec!["Epoch".cell(), format!("{}", &cardano_db_message.beacon.epoch).cell()],
                vec![
                    "Immutable File Number".cell(),
                    format!("{}", &cardano_db_message.beacon.immutable_file_number).cell(),
                ],
                vec!["Network".cell(), cardano_db_message.network.cell()],
                vec!["Digest".cell(), cardano_db_message.digest.cell()],
                vec![
                    "Size".cell(),
                    CardanoDbUtils::format_bytes_to_gigabytes(cardano_db_message.size).cell(),
                ],
                vec![
                    "Cardano node version".cell(),
                    cardano_db_message.cardano_node_version.cell(),
                ],
                vec!["Location".cell(), cardano_db_message.locations.join(",").cell()],
                vec!["Created".cell(), cardano_db_message.created_at.to_string().cell()],
                vec![
                    "Compression Algorithm".cell(),
                    format!("{}", &cardano_db_message.compression_algorithm).cell(),
                ],
            ]
            .table();

            print_stdout(cardano_db_table)?
        }

        Ok(())
    }

    async fn print_v2(&self, client: Client) -> MithrilResult<()> {
        let get_list_of_artifact_ids = || async {
            let cardano_dbs = client.cardano_database_v2().list().await.with_context(|| {
                "Can not get the list of artifacts while retrieving the latest cardano db snapshot hash"
            })?;

            Ok(cardano_dbs
                .iter()
                .map(|cardano_db| cardano_db.hash.to_owned())
                .collect::<Vec<String>>())
        };

        let cardano_db_message = client
            .cardano_database_v2()
            .get(
                &ExpanderUtils::expand_eventual_id_alias(&self.digest, get_list_of_artifact_ids())
                    .await?,
            )
            .await?
            .ok_or_else(|| anyhow!("Cardano DB snapshot not found for hash: '{}'", &self.digest))?;

        if self.is_json_output_enabled() {
            println!("{}", serde_json::to_string(&cardano_db_message)?);
        } else {
            let mut cardano_db_table = vec![
                vec!["Epoch".cell(), format!("{}", &cardano_db_message.beacon.epoch).cell()],
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

            cardano_db_table.append(&mut digest_location_rows(
                &cardano_db_message.digests.locations,
            ));

            cardano_db_table.append(&mut immutables_location_rows(
                &cardano_db_message.immutables.locations,
            ));

            cardano_db_table.append(&mut ancillary_location_rows(
                &cardano_db_message.ancillary.locations,
            ));

            cardano_db_table.push(vec![
                "Created".cell(),
                cardano_db_message.created_at.to_string().cell(),
            ]);

            print_stdout(cardano_db_table.table())?;
        }

        Ok(())
    }
}

fn digest_location_iter(locations: &[DigestLocation]) -> impl Iterator<Item = String> + use<'_> {
    locations.iter().filter_map(|location| match location {
        DigestLocation::Aggregator { uri } => Some(format!("Aggregator, uri: \"{uri}\"")),
        DigestLocation::CloudStorage {
            uri,
            compression_algorithm: _,
        } => Some(format!("CloudStorage, uri: \"{uri}\"")),
        DigestLocation::Unknown => None,
    })
}

fn digest_location_rows(locations: &[DigestLocation]) -> Vec<Vec<CellStruct>> {
    format_location_rows("Digest location", digest_location_iter(locations))
}

fn immutables_location_iter(
    locations: &[ImmutablesLocation],
) -> impl Iterator<Item = String> + use<'_> {
    locations.iter().filter_map(|location| match location {
        ImmutablesLocation::CloudStorage {
            uri,
            compression_algorithm: _,
        } => match uri {
            MultiFilesUri::Template(template_uri) => Some(format!(
                "CloudStorage, template_uri: \"{}\"",
                template_uri.0
            )),
        },
        ImmutablesLocation::Unknown => None,
    })
}

fn immutables_location_rows(locations: &[ImmutablesLocation]) -> Vec<Vec<CellStruct>> {
    format_location_rows("Immutables location", immutables_location_iter(locations))
}

fn ancillary_location_iter(
    locations: &[AncillaryLocation],
) -> impl Iterator<Item = String> + use<'_> {
    locations.iter().filter_map(|location| match location {
        AncillaryLocation::CloudStorage {
            uri,
            compression_algorithm: _,
        } => Some(format!("CloudStorage, uri: \"{uri}\"")),
        AncillaryLocation::Unknown => None,
    })
}

fn ancillary_location_rows(locations: &[AncillaryLocation]) -> Vec<Vec<CellStruct>> {
    format_location_rows("Ancillary location", ancillary_location_iter(locations))
}

fn format_location_rows(
    location_name: &str,
    locations: impl Iterator<Item = String>,
) -> Vec<Vec<CellStruct>> {
    locations
        .enumerate()
        .map(|(index, cell_content)| {
            vec![format!("{location_name} ({})", index + 1).cell(), cell_content.cell()]
        })
        .collect()
}

#[cfg(test)]
mod tests {
    use mithril_client::common::{CompressionAlgorithm, TemplateUri};

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
                compression_algorithm: None,
            },
        ];

        let rows = digest_location_rows(&locations);
        assert_eq!(rows.len(), 2);

        let table = rows.table();
        let rows_rendered = table.display().unwrap().to_string();

        assert!(rows_rendered.contains("Digest location (1)"));
        assert!(rows_rendered.contains("CloudStorage, uri: \"http://cloudstorage.com/\""));
        assert!(rows_rendered.contains("Digest location (2)"));
        assert!(rows_rendered.contains("Aggregator, uri: \"http://aggregator.net/\""));
    }

    #[test]
    fn digest_location_rows_display_and_count_only_known_location() {
        let locations = vec![
            DigestLocation::Unknown,
            DigestLocation::CloudStorage {
                uri: "http://cloudstorage.com/".to_string(),
                compression_algorithm: None,
            },
        ];

        let rows = digest_location_rows(&locations);
        assert_eq!(1, rows.len());

        let rows_rendered = rows.table().display().unwrap().to_string();
        assert!(rows_rendered.contains("Digest location (1)"));
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
                compression_algorithm: Some(CompressionAlgorithm::Gzip),
            },
            ImmutablesLocation::CloudStorage {
                uri: MultiFilesUri::Template(TemplateUri("http://cloudstorage2.com/".to_string())),
                compression_algorithm: Some(CompressionAlgorithm::Gzip),
            },
        ];

        let rows = immutables_location_rows(&locations);

        assert_eq!(rows.len(), 2);

        let table = rows.table();
        let rows_rendered = table.display().unwrap().to_string();

        assert!(rows_rendered.contains("Immutables location (1)"));
        assert!(rows_rendered.contains("CloudStorage, template_uri: \"http://cloudstorage1.com/\""));
        assert!(rows_rendered.contains("Immutables location (2)"));
        assert!(rows_rendered.contains("CloudStorage, template_uri: \"http://cloudstorage2.com/\""));
    }

    #[test]
    fn immutables_location_row_display_and_count_only_known_location() {
        let locations = vec![
            ImmutablesLocation::Unknown {},
            ImmutablesLocation::CloudStorage {
                uri: MultiFilesUri::Template(TemplateUri("http://cloudstorage2.com/".to_string())),
                compression_algorithm: Some(CompressionAlgorithm::Gzip),
            },
        ];

        let rows = immutables_location_rows(&locations);
        assert_eq!(1, rows.len());

        let rows_rendered = rows.table().display().unwrap().to_string();
        assert!(rows_rendered.contains("Immutables location (1)"));
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
                compression_algorithm: Some(CompressionAlgorithm::Gzip),
            },
            AncillaryLocation::CloudStorage {
                uri: "http://cloudstorage2.com/".to_string(),
                compression_algorithm: Some(CompressionAlgorithm::Gzip),
            },
        ];

        let rows = ancillary_location_rows(&locations);

        assert_eq!(rows.len(), 2);

        let table = rows.table();
        let rows_rendered = table.display().unwrap().to_string();

        assert!(rows_rendered.contains("Ancillary location (1)"));
        assert!(rows_rendered.contains("CloudStorage, uri: \"http://cloudstorage1.com/\""));
        assert!(rows_rendered.contains("Ancillary location (2)"));
        assert!(rows_rendered.contains("CloudStorage, uri: \"http://cloudstorage2.com/\""));
    }

    #[test]
    fn ancillary_location_rows_display_and_count_only_known_location() {
        let locations = vec![
            AncillaryLocation::Unknown {},
            AncillaryLocation::CloudStorage {
                uri: "http://cloudstorage2.com/".to_string(),
                compression_algorithm: Some(CompressionAlgorithm::Gzip),
            },
        ];

        let rows = ancillary_location_rows(&locations);
        assert_eq!(1, rows.len());

        let rows_rendered = rows.table().display().unwrap().to_string();
        assert!(rows_rendered.contains("Ancillary location (1)"));
    }
}
