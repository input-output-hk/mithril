#![cfg(unix)]
use std::sync::Arc;

use tokio::sync::{mpsc::unbounded_channel, watch};

use mithril_cardano_node_chain::test::double::FakeChainObserver;
use mithril_common::{
    CardanoNetwork,
    crypto_helper::TryToBytes,
    current_function,
    test::{TempDir, crypto_helper::KesSignerFake},
};
use mithril_dmq::{
    DmqConsumerClient, DmqConsumerClientPallas, DmqConsumerServer, DmqConsumerServerPallas,
    DmqMessage, DmqMessageBuilder, test::payload::DmqMessageTestPayload,
};

async fn create_fake_msg(bytes: &[u8]) -> DmqMessage {
    let dmq_builder = DmqMessageBuilder::new(
        {
            let (kes_signature, operational_certificate) = KesSignerFake::dummy_signature();
            let kes_signer = KesSignerFake::new(vec![
                Ok((kes_signature, operational_certificate.clone())),
                Ok((kes_signature, operational_certificate.clone())), // TODO: remove this line once the hack of KES signature is removed in DMQ message builder
            ]);

            Arc::new(kes_signer)
        },
        Arc::new(FakeChainObserver::default()),
    )
    .set_ttl(100);
    let message = DmqMessageTestPayload::new(bytes);
    dmq_builder.build(&message.to_bytes_vec().unwrap()).await.unwrap()
}

#[tokio::test]
async fn dmq_consumer_client_server() {
    let cardano_network = CardanoNetwork::TestNet(0);
    let socket_path =
        TempDir::create_with_short_path("dmq_consumer_client_server", current_function!())
            .join("node.socket");
    let (stop_tx, stop_rx) = watch::channel(());

    let (signature_dmq_tx, signature_dmq_rx) = unbounded_channel::<DmqMessage>();
    let server = tokio::spawn({
        let socket_path = socket_path.clone();
        async move {
            let dmq_consumer_server = Arc::new(DmqConsumerServerPallas::new(
                socket_path.to_path_buf(),
                cardano_network,
                stop_rx,
                slog_scope::logger(),
            ));
            dmq_consumer_server.register_receiver(signature_dmq_rx).await.unwrap();
            dmq_consumer_server.run().await.unwrap();
        }
    });

    let client = tokio::spawn({
        let socket_path = socket_path.clone();
        async move {
            let consumer_client = DmqConsumerClientPallas::<DmqMessageTestPayload>::new(
                socket_path,
                cardano_network,
                slog_scope::logger(),
            );
            let mut messages = vec![];
            signature_dmq_tx.send(create_fake_msg(b"msg_1").await).unwrap();
            signature_dmq_tx.send(create_fake_msg(b"msg_2").await).unwrap();
            messages.extend_from_slice(&consumer_client.consume_messages().await.unwrap());
            signature_dmq_tx.send(create_fake_msg(b"msg_3").await).unwrap();
            messages.extend_from_slice(&consumer_client.consume_messages().await.unwrap());
            stop_tx.send(()).unwrap();

            messages.into_iter().map(|(msg, _)| msg).collect::<Vec<_>>()
        }
    });

    let (_, messages) = tokio::try_join!(server, client).unwrap();
    assert_eq!(
        vec![
            DmqMessageTestPayload::new(b"msg_1"),
            DmqMessageTestPayload::new(b"msg_2"),
            DmqMessageTestPayload::new(b"msg_3")
        ],
        messages
    );
}
