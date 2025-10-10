#![cfg(unix)]
use std::sync::Arc;

use tokio::sync::{mpsc::unbounded_channel, watch};

use mithril_common::{current_function, test::TempDir};
use mithril_dmq::{
    DmqConsumerClient, DmqConsumerClientPallas, DmqConsumerServer, DmqConsumerServerPallas,
    DmqMessage, DmqNetwork,
    test::{fake_message::compute_fake_msg, payload::DmqMessageTestPayload},
};

#[tokio::test(flavor = "multi_thread")]
async fn dmq_consumer_client_server() {
    let current_function_name = current_function!();
    let dmq_network = DmqNetwork::TestNet(0);
    let socket_path =
        TempDir::create_with_short_path("dmq_consumer_client_server", current_function_name)
            .join("node.socket");
    let (stop_tx, stop_rx) = watch::channel(());

    // Start the server
    let (signature_dmq_tx, signature_dmq_rx) = unbounded_channel::<DmqMessage>();
    let server = tokio::spawn({
        let socket_path = socket_path.clone();
        async move {
            let dmq_consumer_server = Arc::new(DmqConsumerServerPallas::new(
                socket_path.to_path_buf(),
                dmq_network,
                stop_rx,
                slog_scope::logger(),
            ));
            dmq_consumer_server.register_receiver(signature_dmq_rx).await.unwrap();
            dmq_consumer_server.run().await.unwrap();
        }
    });

    // Start a first client, receive messages and wait for its deconnection
    let client = tokio::spawn({
        let socket_path = socket_path.clone();
        let signature_dmq_tx = signature_dmq_tx.clone();
        async move {
            let consumer_client = DmqConsumerClientPallas::<DmqMessageTestPayload>::new(
                socket_path,
                dmq_network,
                slog_scope::logger(),
            );
            let mut messages = vec![];
            signature_dmq_tx
                .send(compute_fake_msg(b"msg_1", current_function_name).await)
                .unwrap();
            signature_dmq_tx
                .send(compute_fake_msg(b"msg_2", current_function_name).await)
                .unwrap();
            messages.extend_from_slice(&consumer_client.consume_messages().await.unwrap());
            signature_dmq_tx
                .send(compute_fake_msg(b"msg_3", current_function_name).await)
                .unwrap();
            messages.extend_from_slice(&consumer_client.consume_messages().await.unwrap());

            messages.into_iter().map(|(msg, _)| msg).collect::<Vec<_>>()
        }
    });

    let messages = client.await.unwrap();
    assert_eq!(
        vec![
            DmqMessageTestPayload::new(b"msg_1"),
            DmqMessageTestPayload::new(b"msg_2"),
            DmqMessageTestPayload::new(b"msg_3")
        ],
        messages
    );

    // Sleep to avoid refused connection from the server
    tokio::time::sleep(std::time::Duration::from_millis(10)).await;

    // Start a second client, receive messages
    let client = tokio::spawn({
        let socket_path = socket_path.clone();
        let signature_dmq_tx = signature_dmq_tx.clone();
        async move {
            let consumer_client = DmqConsumerClientPallas::<DmqMessageTestPayload>::new(
                socket_path,
                dmq_network,
                slog_scope::logger(),
            );
            let mut messages = vec![];
            signature_dmq_tx
                .send(compute_fake_msg(b"msg_4", current_function_name).await)
                .unwrap();
            messages.extend_from_slice(&consumer_client.consume_messages().await.unwrap());
            stop_tx.send(()).unwrap();

            messages.into_iter().map(|(msg, _)| msg).collect::<Vec<_>>()
        }
    });

    // Check that all messages have been correctly received
    let (_, messages) = tokio::try_join!(server, client).unwrap();
    assert_eq!(vec![DmqMessageTestPayload::new(b"msg_4")], messages);
}
