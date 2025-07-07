use async_trait::async_trait;

use mithril_client::{
    MithrilResult,
    era::{EraFetcher, FetchedEra},
};

pub struct ForcedEraFetcher {
    era: String,
}

impl ForcedEraFetcher {
    pub fn new(era: String) -> Self {
        Self { era }
    }
}

#[async_trait]
impl EraFetcher for ForcedEraFetcher {
    async fn fetch_current_era(&self) -> MithrilResult<FetchedEra> {
        Ok(FetchedEra {
            era: self.era.clone(),
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn fetch_current_era_returns_fetched_era_with_forced_era_value() {
        let era_fetcher = ForcedEraFetcher::new("forced_era".to_string());

        let fetched_era = era_fetcher.fetch_current_era().await.unwrap();

        assert_eq!(
            FetchedEra {
                era: "forced_era".to_string(),
            },
            fetched_era
        );
    }
}
