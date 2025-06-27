use std::future;

use async_trait::async_trait;

use mithril_common::{
    StdResult,
    entities::{SignedEntityType, SingleSignature},
};

use super::SignatureConsumer;

/// A no-op implementation of the [SignatureConsumer] trait that will never return signatures.
pub struct SignatureConsumerNoop;

#[async_trait]
impl SignatureConsumer for SignatureConsumerNoop {
    async fn get_signatures(&self) -> StdResult<Vec<(SingleSignature, SignedEntityType)>> {
        future::pending().await
    }

    fn get_origin_tag(&self) -> String {
        "NOOP".to_string()
    }
}

#[cfg(test)]
mod tests {
    use anyhow::anyhow;
    use tokio::time::{Duration, sleep};

    use super::*;

    #[tokio::test]
    async fn signature_consumer_noop_never_returns() {
        let consumer = SignatureConsumerNoop;

        let result = tokio::select!(
            _res = sleep(Duration::from_millis(100)) => {Err(anyhow!("Timeout"))},
            _res =  consumer.get_signatures()  => {Ok(())},
        );

        result.expect_err("Should have timed out");
    }
}
