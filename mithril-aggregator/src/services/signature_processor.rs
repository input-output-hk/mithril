use std::sync::Arc;

use slog::{error, Logger};

use mithril_common::{
    entities::{SignedEntityType, SingleSignature},
    logging::LoggerExtensions,
    StdResult,
};

use super::CertifierService;

/// A signature consumer which blocks until a messages are available.
#[async_trait::async_trait]
pub trait SignatureConsumer: Sync + Send {
    /// Waits for messages to be available and returns it.
    async fn next_signatures(&self) -> Result<Vec<(SingleSignature, SignedEntityType)>, String>;
}

/// A signature processor receives messages and dispatch them
pub struct SignatureProcessor {
    consumer: Arc<dyn SignatureConsumer>,
    certifier: Arc<dyn CertifierService>,
    logger: Logger,
}

impl SignatureProcessor {
    /// Creates a new `SignatureProcessor` instance.
    pub fn new(
        consumer: Arc<dyn SignatureConsumer>,
        certifier: Arc<dyn CertifierService>,
        logger: Logger,
    ) -> Self {
        Self {
            consumer,
            certifier,
            logger: logger.new_with_component_name::<Self>(),
        }
    }

    /// Starts the dispatcher, which will run indefinitely, processing messages as they arrive.
    pub async fn run(&self) -> StdResult<()> {
        loop {
            match self.consumer.next_signatures().await {
                Ok(signatures) => {
                    for (signature, signed_entity_type) in signatures {
                        if let Err(e) = self
                            .certifier
                            .register_single_signature(&signed_entity_type, &signature)
                            .await
                        {
                            error!(self.logger, "Error dispatching single signature"; "error" => ?e);
                        }
                    }
                }
                Err(e) => {
                    error!(self.logger, "Error consuming single signatures"; "error" => ?e);
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {}
