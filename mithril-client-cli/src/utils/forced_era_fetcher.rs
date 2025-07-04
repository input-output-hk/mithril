use async_trait::async_trait;

use mithril_client::{EraFetcher, FetchedEra, MithrilResult};

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
