use std::ops::Add;

use anyhow::anyhow;
use async_trait::async_trait;
use tokio::sync::RwLock;

use mithril_common::entities::ImmutableFileNumber;
use mithril_common::StdResult;

use crate::{ImmutableFileObserver, ImmutableFileObserverError};

/// An [ImmutableFileObserver] yielding fixed results for test purpose.
pub struct DumbImmutableFileObserver {
    /// The [ImmutableFileNumber] that shall be returned by
    /// [get_last_immutable_number][ImmutableFileObserver::get_last_immutable_number]
    pub shall_return: RwLock<Option<ImmutableFileNumber>>,
}

impl Default for DumbImmutableFileObserver {
    fn default() -> Self {
        let mut observer = Self::new();
        observer.shall_return = RwLock::new(Some(500));

        observer
    }
}

impl DumbImmutableFileObserver {
    /// [DumbImmutableFileObserver] factory.
    pub fn new() -> Self {
        Self {
            shall_return: RwLock::new(None),
        }
    }

    /// Update the stored [immutable file number][DumbImmutableFileObserver::shall_return].
    pub async fn shall_return(&self, what: Option<u64>) -> &Self {
        let mut shall_return = self.shall_return.write().await;
        *shall_return = what;
        self
    }

    /// Increase by one the stored [immutable file number][DumbImmutableFileObserver::shall_return],
    /// return the updated value.
    pub async fn increase(&self) -> StdResult<u64> {
        let new_number = self
            .shall_return
            .write()
            .await
            .ok_or_else(|| anyhow!(ImmutableFileObserverError::Missing()))?
            .add(1);
        self.shall_return(Some(new_number)).await;

        Ok(new_number)
    }
}

#[async_trait]
impl ImmutableFileObserver for DumbImmutableFileObserver {
    async fn get_last_immutable_number(&self) -> StdResult<ImmutableFileNumber> {
        self.shall_return
            .read()
            .await
            .ok_or_else(|| anyhow!(ImmutableFileObserverError::Missing()))
    }
}
