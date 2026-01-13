use std::collections::HashMap;
use std::time::Duration;

use anyhow::Context;
use blockfrost::{BlockFrostSettings, BlockfrostAPI, Pagination};

use mithril_common::StdResult;
use mithril_common::entities::PartyId;

use super::{PoolTicker, SignersImporterRetriever};

pub struct BlockfrostSignerRetriever {
    client: BlockfrostAPI,
}

impl BlockfrostSignerRetriever {
    /// Creates a new instance of `BlockfrostSignerRetriever`.
    ///
    /// If no custom base URL is provided, the Blockfrost base URL will be determined automatically
    /// from the project ID.
    pub fn new(blockfrost_project_id: &str, blockfrost_base_url: Option<String>) -> Self {
        let mut settings = BlockFrostSettings::new();
        settings.base_url = blockfrost_base_url;
        // Hardcoded retry settings for Blockfrost API, 5 attempts with 1-second delay
        // It should not be triggered as the free tier is enough to handle the load without hitting rate limits
        settings.retry_settings = blockfrost::RetrySettings::new(5, Duration::from_secs(1));

        Self {
            client: BlockfrostAPI::new(blockfrost_project_id, settings),
        }
    }
}

#[async_trait::async_trait]
impl SignersImporterRetriever for BlockfrostSignerRetriever {
    async fn retrieve(&self) -> StdResult<HashMap<PartyId, Option<PoolTicker>>> {
        let pools_extended = self
            .client
            .pools_extended(Pagination::all())
            .await
            .with_context(|| "Failed to retrieve pools extended from Blockfrost API")?;

        let res = pools_extended
            .into_iter()
            .map(|pool| {
                let pool_id = pool.pool_id;
                let (pool_name, pool_ticker) =
                    pool.metadata.map(|m| (m.name, m.ticker)).unwrap_or_default();

                (pool_id, join_name_and_ticker(pool_name, pool_ticker))
            })
            .collect();

        Ok(res)
    }
}

fn join_name_and_ticker(name: Option<String>, ticker: Option<String>) -> Option<String> {
    name.and_then(|n| ticker.map(|t| format!("[{}] {}", t, n)))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_join_name_and_ticker_with_both() {
        let result = join_name_and_ticker(
            Some("pool name".to_string()),
            Some("poolTicker".to_string()),
        );
        assert_eq!(result, Some("[poolTicker] pool name".to_string()));
    }

    #[test]
    fn test_join_name_and_ticker_with_name_only() {
        let result = join_name_and_ticker(Some("pool name".to_string()), None);
        assert_eq!(result, None);
    }

    #[test]
    fn test_join_name_and_ticker_with_ticker_only() {
        let result = join_name_and_ticker(None, Some("poolTicker".to_string()));
        assert_eq!(result, None);
    }

    #[test]
    fn test_join_name_and_ticker_with_none() {
        let result = join_name_and_ticker(None, None);
        assert_eq!(result, None);
    }
}
