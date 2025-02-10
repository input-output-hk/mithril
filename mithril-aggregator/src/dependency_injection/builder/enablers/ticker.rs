use std::sync::Arc;

use mithril_common::digesters::{
    DumbImmutableFileObserver, ImmutableFileObserver, ImmutableFileSystemObserver,
};
use mithril_common::{MithrilTickerService, TickerService};

use crate::dependency_injection::{DependenciesBuilder, Result};
use crate::ExecutionEnvironment;

impl DependenciesBuilder {
    /// Create [TickerService] instance.
    pub async fn build_ticker_service(&mut self) -> Result<Arc<dyn TickerService>> {
        let chain_observer = self.get_chain_observer().await?;
        let immutable_observer = self.get_immutable_file_observer().await?;

        Ok(Arc::new(MithrilTickerService::new(
            chain_observer,
            immutable_observer,
        )))
    }

    /// [TickerService] service
    pub async fn get_ticker_service(&mut self) -> Result<Arc<dyn TickerService>> {
        if self.ticker_service.is_none() {
            self.ticker_service = Some(self.build_ticker_service().await?);
        }

        Ok(self.ticker_service.as_ref().cloned().unwrap())
    }

    async fn build_immutable_file_observer(&mut self) -> Result<Arc<dyn ImmutableFileObserver>> {
        let immutable_file_observer: Arc<dyn ImmutableFileObserver> =
            match self.configuration.environment {
                ExecutionEnvironment::Production => Arc::new(ImmutableFileSystemObserver::new(
                    &self.configuration.db_directory,
                )),
                _ => Arc::new(DumbImmutableFileObserver::default()),
            };

        Ok(immutable_file_observer)
    }

    /// Return a [ImmutableFileObserver] instance.
    pub async fn get_immutable_file_observer(&mut self) -> Result<Arc<dyn ImmutableFileObserver>> {
        if self.immutable_file_observer.is_none() {
            self.immutable_file_observer = Some(self.build_immutable_file_observer().await?);
        }

        Ok(self.immutable_file_observer.as_ref().cloned().unwrap())
    }
}
