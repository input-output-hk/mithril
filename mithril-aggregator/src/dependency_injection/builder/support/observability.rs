use slog::Logger;
use std::sync::Arc;
use tokio::sync::mpsc::{UnboundedReceiver, UnboundedSender};

use crate::dependency_injection::{DependenciesBuilder, DependenciesBuilderError, Result};
use crate::event_store::{EventMessage, EventStore, TransmitterService};
use crate::services::UsageReporter;
use crate::MetricsService;

impl DependenciesBuilder {
    /// Return a copy of the root logger.
    pub fn root_logger(&self) -> Logger {
        self.root_logger.clone()
    }

    /// Create a [UsageReporter] instance.
    pub async fn create_usage_reporter(&mut self) -> Result<UsageReporter> {
        let usage_reporter = UsageReporter::new(
            self.get_event_transmitter().await?,
            self.get_metrics_service().await?,
            self.root_logger(),
        );

        Ok(usage_reporter)
    }

    /// Create a [MetricsService] instance.
    async fn build_metrics_service(&self) -> Result<Arc<MetricsService>> {
        let metrics_service = MetricsService::new(self.root_logger())?;

        Ok(Arc::new(metrics_service))
    }

    /// [MetricsService] service
    pub async fn get_metrics_service(&mut self) -> Result<Arc<MetricsService>> {
        if self.metrics_service.is_none() {
            self.metrics_service = Some(self.build_metrics_service().await?);
        }

        Ok(self.metrics_service.as_ref().cloned().unwrap())
    }

    /// Create dependencies for the [EventStore] task.
    pub async fn create_event_store(&mut self) -> Result<EventStore> {
        let event_store = EventStore::new(
            self.get_event_transmitter_receiver().await?,
            self.get_event_store_sqlite_connection().await?,
            self.root_logger(),
        );

        Ok(event_store)
    }

    async fn build_event_transmitter_channel(
        &mut self,
    ) -> Result<(
        UnboundedReceiver<EventMessage>,
        UnboundedSender<EventMessage>,
    )> {
        let (tx, rx) = tokio::sync::mpsc::unbounded_channel::<EventMessage>();

        Ok((rx, tx))
    }

    /// Return the EventMessage channel sender.
    pub async fn get_event_transmitter_sender(&mut self) -> Result<UnboundedSender<EventMessage>> {
        if let (_, None) = self.event_transmitter_channel {
            let (rx, tx) = self.build_event_transmitter_channel().await?;
            self.event_transmitter_channel = (Some(rx), Some(tx));
        }

        Ok(self
            .event_transmitter_channel
            .1
            .as_ref()
            .cloned()
            .expect("Transmitter<EventMessage> should be set."))
    }

    /// Return the channel receiver setup for the [EventStore]. Since this
    /// receiver is not clonable, it must be called only once.
    pub async fn get_event_transmitter_receiver(
        &mut self,
    ) -> Result<UnboundedReceiver<EventMessage>> {
        if let (_, None) = self.event_transmitter_channel {
            let (rx, tx) = self.build_event_transmitter_channel().await?;
            self.event_transmitter_channel = (Some(rx), Some(tx));
        }
        let mut receiver: Option<UnboundedReceiver<EventMessage>> = None;
        std::mem::swap(&mut self.event_transmitter_channel.0, &mut receiver);

        receiver.ok_or_else(|| {
            DependenciesBuilderError::InconsistentState(
                "Receiver<EventMessage> has already been given and is not clonable.".to_string(),
            )
        })
    }

    async fn build_event_transmitter(&mut self) -> Result<Arc<TransmitterService<EventMessage>>> {
        let sender = self.get_event_transmitter_sender().await?;
        let event_transmitter = Arc::new(TransmitterService::new(sender, self.root_logger()));

        Ok(event_transmitter)
    }

    /// [TransmitterService] service
    pub async fn get_event_transmitter(&mut self) -> Result<Arc<TransmitterService<EventMessage>>> {
        if self.event_transmitter.is_none() {
            self.event_transmitter = Some(self.build_event_transmitter().await?);
        }

        Ok(self.event_transmitter.as_ref().cloned().unwrap())
    }
}
