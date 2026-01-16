use tokio::sync::RwLock;

use mithril_common::StdResult;
use mithril_common::entities::{Epoch, Signer};

use crate::services::SignerRegistrationPublisher;

/// A spy implementation of the `SignerRegistrationPublisher` trait for testing
pub struct SpySignerRegistrationPublisher {
    last_registered_signer: RwLock<Option<Signer>>,
    total_registered_signers: RwLock<u32>,
}

impl SpySignerRegistrationPublisher {
    /// Return the last signer that called with the `register` method.
    pub async fn get_last_registered_signer(&self) -> Option<Signer> {
        self.last_registered_signer.read().await.clone()
    }

    /// Return the total number of signers that called with the `register` method.
    pub async fn get_total_registered_signers(&self) -> u32 {
        *self.total_registered_signers.read().await
    }
}

impl Default for SpySignerRegistrationPublisher {
    fn default() -> Self {
        Self {
            last_registered_signer: RwLock::new(None),
            total_registered_signers: RwLock::new(0),
        }
    }
}

#[async_trait::async_trait]
impl SignerRegistrationPublisher for SpySignerRegistrationPublisher {
    async fn register_signer(&self, _epoch: Epoch, signer: &Signer) -> StdResult<()> {
        let mut last_registered_signer = self.last_registered_signer.write().await;
        let signer = signer.clone();
        *last_registered_signer = Some(signer);

        let mut total_registered_signers = self.total_registered_signers.write().await;
        *total_registered_signers += 1;

        Ok(())
    }
}
