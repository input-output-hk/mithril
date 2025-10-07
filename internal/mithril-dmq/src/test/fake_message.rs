//! Fake message computation for testing purposes.

use std::sync::Arc;

use mithril_cardano_node_chain::test::double::FakeChainObserver;
use mithril_common::{crypto_helper::TryToBytes, test::crypto_helper::KesSignerFake};

use crate::{
    DmqMessage, DmqMessageBuilder,
    test::{double::FakeUnixTimestampProvider, payload::DmqMessageTestPayload},
};

/// Computes a fake DMQ message for testing purposes.
pub async fn compute_fake_msg(bytes: &[u8], test_directory: &str) -> DmqMessage {
    let dmq_builder = DmqMessageBuilder::new(
        {
            let (kes_signature, operational_certificate) =
                KesSignerFake::dummy_signature(test_directory);
            let kes_signer =
                KesSignerFake::new(vec![Ok((kes_signature, operational_certificate.clone()))]);

            Arc::new(kes_signer)
        },
        Arc::new(FakeChainObserver::default()),
    )
    .set_ttl(100)
    .set_timestamp_provider(Arc::new(FakeUnixTimestampProvider::max_timestamp_for_ttl(
        100,
    )));
    let message = DmqMessageTestPayload::new(bytes);
    dmq_builder.build(&message.to_bytes_vec().unwrap()).await.unwrap()
}
