//! # Certificate chain synchronizer
//!
//! Behavior:
//! 1. Check force:
//!    - If false, fetch the latest local genesis certificate in database
//!       - If it's found, fetch the remote Genesis certificate
//!          - If it's different from the local genesis, continue synchronization
//!          - If it's the same, abort with an `Ok`
//!       - If it's not found, continue synchronization
//!    - If true, skip the remote Genesis certificate check and synchronize
//! 2. Fetch then validate the latest remote certificate
//!    - if valid, store it in an in-memory FIFO list
//!    - if invalid, abort with an `Err`
//! 3. Repeat step 2. with each parent of the certificate until the genesis certificate is reached
//! 4. Store the fetched certificates in the database, for each certificate:
//!    - if it exists in the database, it is replaced
//!    - if it doesn't exist, it is inserted
//! 5. End
//!

use async_trait::async_trait;
use slog::Logger;
use std::sync::Arc;

use mithril_common::StdResult;
use mithril_common::entities::Certificate;
use mithril_common::logging::LoggerExtensions;

use super::{
    CertificateChainSynchronizer, RemoteCertificateRetriever, SynchronizedCertificateStorer,
};

/// Service that synchronizes the certificate chain with a remote aggregator
pub struct MithrilCertificateChainSynchronizer {
    remote_certificate_retriever: Arc<dyn RemoteCertificateRetriever>,
    certificate_storer: Arc<dyn SynchronizedCertificateStorer>,
    logger: Logger,
}

impl MithrilCertificateChainSynchronizer {
    /// Create a new `MithrilCertificateChainSynchronizer` instance
    pub fn new(
        remote_certificate_retriever: Arc<dyn RemoteCertificateRetriever>,
        certificate_storer: Arc<dyn SynchronizedCertificateStorer>,
        logger: Logger,
    ) -> Self {
        Self {
            remote_certificate_retriever,
            certificate_storer,
            logger: logger.new_with_component_name::<Self>(),
        }
    }

    async fn should_sync(&self, force: bool) -> StdResult<bool> {
        Ok(force)
    }

    async fn retrieve_remote_certificate_chain(&self) -> StdResult<Vec<Certificate>> {
        Ok(Vec::new())
    }

    async fn store_certificate_chain(&self, certificate_chain: Vec<Certificate>) -> StdResult<()> {
        Ok(())
    }
}

#[async_trait]
impl CertificateChainSynchronizer for MithrilCertificateChainSynchronizer {
    async fn synchronize_certificate_chain(&self, force: bool) -> StdResult<()> {
        if !self.should_sync(force).await? {
            return Ok(());
        }

        let remote_certificate_chain = self.retrieve_remote_certificate_chain().await?;
        self.store_certificate_chain(remote_certificate_chain).await?;

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
}
