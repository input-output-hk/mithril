use anyhow::{anyhow, Context};
use mithril_client::Client;
use mithril_common::StdResult;

pub struct MithrilStakeDistributionUtils;

impl MithrilStakeDistributionUtils {
    pub async fn expand_eventual_artifact_hash_alias(
        client: &Client,
        hash: &str,
    ) -> StdResult<String> {
        if hash.to_lowercase() == "latest" {
            let last_mithril_stake_distribution = client.mithril_stake_distribution().list().await.with_context(|| {
                "Can not get the list of artifacts while retrieving the latest stake distribution hash"
            })?;
            let last_mithril_stake_distribution =
                last_mithril_stake_distribution.first().ok_or_else(|| {
                    anyhow!(
                        "Mithril stake distribution '{}' not found",
                        hash.to_string()
                    )
                })?;
            Ok(last_mithril_stake_distribution.hash.to_owned())
        } else {
            Ok(hash.to_owned())
        }
    }
}

#[cfg(test)]
mod test {
    use crate::extensions::fake::FakeCertificateVerifier;

    use super::*;

    use mithril_client::ClientBuilder;
    use mithril_common::messages::MithrilStakeDistributionListItemMessage;
    use mithril_common::test_utils::{
        fake_keys,
        test_http_server::{test_http_server, TestHttpServer},
    };
    use warp::Filter;

    fn get_stake_distributions(hashes: Vec<&str>) -> String {
        serde_json::to_string(
            &hashes
                .iter()
                .map(|h| MithrilStakeDistributionListItemMessage {
                    hash: h.to_string(),
                    ..MithrilStakeDistributionListItemMessage::dummy()
                })
                .collect::<Vec<_>>(),
        )
        .unwrap()
    }

    fn create_client_with_stake_distributions(
        stake_distributions: String,
    ) -> (Client, TestHttpServer) {
        let server = test_http_server(
            warp::path!("artifact" / "mithril-stake-distributions")
                .map(move || stake_distributions.clone()),
        );
        let genesis_verification_key = fake_keys::genesis_verification_key()[0];
        let client = ClientBuilder::aggregator(&server.url(), genesis_verification_key)
            .with_certificate_verifier(
                FakeCertificateVerifier::build_that_validate_any_certificate(),
            )
            .build()
            .unwrap();

        (client, server)
    }

    #[tokio::test]
    async fn expand_eventual_artifact_hash_alias_should_returns_id() {
        let stake_distributions = get_stake_distributions(vec!["hash-123", "hash-234", "hash-345"]);
        let (client, _server) = create_client_with_stake_distributions(stake_distributions);

        let hash =
            MithrilStakeDistributionUtils::expand_eventual_artifact_hash_alias(&client, "hash-234")
                .await
                .unwrap();

        assert_eq!("hash-234", hash);
    }

    #[tokio::test]
    async fn expand_eventual_artifact_hash_alias_latest_lowercase() {
        let stake_distributions = get_stake_distributions(vec!["hash-123", "hash-234", "hash-345"]);
        let (client, _server) = create_client_with_stake_distributions(stake_distributions);

        let hash = MithrilStakeDistributionUtils::expand_eventual_artifact_hash_alias(&client, "latest")
            .await
            .expect(
            "expand_eventual_artifact_hash_alias should not error when latest is passed as parameter.",
        );

        assert_eq!("hash-123".to_string(), hash);
    }

    #[tokio::test]
    async fn expand_eventual_artifact_hash_alias_latest_uppercase() {
        let stake_distributions = get_stake_distributions(vec!["hash-123", "hash-234", "hash-345"]);
        let (client, _server) = create_client_with_stake_distributions(stake_distributions);

        let hash = MithrilStakeDistributionUtils::expand_eventual_artifact_hash_alias(&client, "LATEST")
            .await
            .expect(
            "expand_eventual_artifact_hash_alias should not error when latest is passed as parameter.",
        );

        assert_eq!("hash-123".to_string(), hash);
    }

    #[tokio::test]
    async fn expand_eventual_artifact_hash_alias_should_error() {
        let stake_distributions = "[]";
        let (client, _server) =
            create_client_with_stake_distributions(stake_distributions.to_string());

        let _ = MithrilStakeDistributionUtils::expand_eventual_artifact_hash_alias(
            &client, "LATEST",
        )
        .await
        .expect_err(
            "expand_eventual_artifact_hash_alias should returns an error if there is no latest.",
        );
    }
}
