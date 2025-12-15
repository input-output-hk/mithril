use std::{collections::VecDeque, future};

use async_trait::async_trait;
use mithril_common::{
    StdResult,
    entities::{SignedEntityType, SingleSignature},
};
use tokio::sync::Mutex;

use super::SignatureConsumer;

type SignatureBatchResult = StdResult<Vec<(SingleSignature, SignedEntityType)>>;

/// A fake implementation of the [SignatureConsumer] trait (test only).
pub struct FakeSignatureConsumer {
    signature_batches: Mutex<VecDeque<SignatureBatchResult>>,
}

impl FakeSignatureConsumer {
    /// Creates a new `FakeSignatureConsumer` instance.
    pub fn new(signature_batches: Vec<SignatureBatchResult>) -> Self {
        Self {
            signature_batches: Mutex::new(signature_batches.into()),
        }
    }
}

#[async_trait]
impl SignatureConsumer for FakeSignatureConsumer {
    async fn get_signatures(&self) -> SignatureBatchResult {
        let mut signature_batches = self.signature_batches.lock().await;
        match signature_batches.pop_front() {
            None => future::pending().await,
            Some(signature_batch) => {
                return signature_batch;
            }
        }
    }
    fn get_origin_tag(&self) -> String {
        "FAKE".to_string()
    }
}

#[cfg(test)]
mod tests {
    use mithril_common::{entities::Epoch, test::double::fake_data};

    use super::*;

    #[tokio::test]
    async fn fake_signature_consumer_returns_signature_batches_in_expected_order() {
        let consumer = FakeSignatureConsumer::new(vec![
            Ok(vec![(
                fake_data::single_signature(vec![1, 2, 3]),
                SignedEntityType::MithrilStakeDistribution(Epoch(1)),
            )]),
            Ok(vec![(
                fake_data::single_signature(vec![4, 5, 6]),
                SignedEntityType::MithrilStakeDistribution(Epoch(2)),
            )]),
        ]);

        let result = consumer.get_signatures().await.unwrap();
        assert_eq!(
            vec![(
                fake_data::single_signature(vec![1, 2, 3]),
                SignedEntityType::MithrilStakeDistribution(Epoch(1)),
            )],
            result
        );

        let result = consumer.get_signatures().await.unwrap();
        assert_eq!(
            vec![(
                fake_data::single_signature(vec![4, 5, 6]),
                SignedEntityType::MithrilStakeDistribution(Epoch(2)),
            )],
            result
        );
    }
}
