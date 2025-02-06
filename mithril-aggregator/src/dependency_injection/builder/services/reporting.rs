use std::sync::Arc;

use crate::dependency_injection::DependenciesBuilder;
use crate::event_store::{EventMessage, TransmitterService};
use crate::services::UsageReporter;
use crate::MetricsService;

impl DependenciesBuilder {
    /// Create a [UsageReporter] instance.
    pub async fn create_usage_reporter(
        &mut self,
    ) -> crate::dependency_injection::Result<UsageReporter> {
        let usage_reporter = UsageReporter::new(
            self.get_event_transmitter().await?,
            self.get_metrics_service().await?,
            self.root_logger(),
        );

        Ok(usage_reporter)
    }

    /// Create a [MetricsService] instance.
    async fn build_metrics_service(
        &self,
    ) -> crate::dependency_injection::Result<Arc<MetricsService>> {
        let metrics_service = MetricsService::new(self.root_logger())?;

        Ok(Arc::new(metrics_service))
    }

    /// [MetricsService] service
    pub async fn get_metrics_service(
        &mut self,
    ) -> crate::dependency_injection::Result<Arc<MetricsService>> {
        if self.metrics_service.is_none() {
            self.metrics_service = Some(self.build_metrics_service().await?);
        }

        Ok(self.metrics_service.as_ref().cloned().unwrap())
    }

    async fn build_event_transmitter(
        &mut self,
    ) -> crate::dependency_injection::Result<Arc<TransmitterService<EventMessage>>> {
        let sender = self.get_event_transmitter_sender().await?;
        let event_transmitter = Arc::new(TransmitterService::new(sender, self.root_logger()));

        Ok(event_transmitter)
    }

    /// [TransmitterService] service
    pub async fn get_event_transmitter(
        &mut self,
    ) -> crate::dependency_injection::Result<Arc<TransmitterService<EventMessage>>> {
        if self.event_transmitter.is_none() {
            self.event_transmitter = Some(self.build_event_transmitter().await?);
        }

        Ok(self.event_transmitter.as_ref().cloned().unwrap())
    }
}
