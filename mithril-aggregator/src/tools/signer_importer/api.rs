use std::collections::HashMap;
use std::sync::Arc;
use std::time::Duration;

use anyhow::Context;
use async_trait::async_trait;
use slog::{Logger, info, warn};

use mithril_common::StdResult;
use mithril_common::entities::PartyId;
use mithril_common::logging::LoggerExtensions;

pub type PoolTicker = String;

/// Tool that can import a list of signers
pub struct SignersImporter {
    retriever: Arc<dyn SignersImporterRetriever>,
    persister: Arc<dyn SignersImporterPersister>,
    logger: Logger,
}

impl SignersImporter {
    /// [SignersImporter] factory
    pub fn new(
        retriever: Arc<dyn SignersImporterRetriever>,
        persister: Arc<dyn SignersImporterPersister>,
        logger: Logger,
    ) -> Self {
        Self {
            retriever,
            persister,
            logger: logger.new_with_component_name::<Self>(),
        }
    }

    /// Import and persist the signers
    pub async fn run(&self) -> StdResult<()> {
        info!(self.logger, "Starting import");
        let items = self
            .retriever
            .retrieve()
            .await
            .with_context(|| "Failed to retrieve signers from remote service")?;

        info!(
            self.logger, "Persisting retrieved data in the database";
            "number_of_signer_to_insert" => items.len()
        );
        self.persister
            .persist(items)
            .await
            .with_context(|| "Failed to persist retrieved data into the database")
    }

    /// Start a loop that call [run][Self::run] at the given time interval.
    pub async fn run_forever(&self, run_interval: Duration) {
        let mut interval = tokio::time::interval(run_interval);

        loop {
            interval.tick().await;
            if let Err(error) = self.run().await {
                warn!(self.logger, "Signer retriever failed"; "error" => ?error);
            }
            info!(
                self.logger,
                "Cycle finished, Sleeping for {} min",
                run_interval.as_secs() / 60
            );
        }
    }
}

/// Trait that define how a [SignersImporter] retrieve the signers to import.
#[cfg_attr(test, mockall::automock)]
#[async_trait]
pub trait SignersImporterRetriever: Sync + Send {
    /// Retrieve the signers list.
    async fn retrieve(&self) -> StdResult<HashMap<PartyId, Option<PoolTicker>>>;
}

/// Trait that define how a [SignersImporter] persist the retrieved signers.
#[cfg_attr(test, mockall::automock)]
#[async_trait]
pub trait SignersImporterPersister: Sync + Send {
    /// Persist the given list of signers.
    async fn persist(&self, signers: HashMap<PartyId, Option<PoolTicker>>) -> StdResult<()>;
}
