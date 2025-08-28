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
    DmqMessage, DmqMessageBuilder, DmqPublisherClient, DmqPublisherClientPallas,
    DmqPublisherServer, DmqPublisherServerPallas, test::payload::DmqMessageTestPayload,
};

async fn create_fake_msg(bytes: &[u8], test_directory: &str) -> DmqMessage {
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
    .set_ttl(100);
    let message = DmqMessageTestPayload::new(bytes);
    dmq_builder.build(&message.to_bytes_vec().unwrap()).await.unwrap()
}

#[tokio::test]
async fn dmq_publisher_client_server() {
    let current_function_name = current_function!();
    let cardano_network = CardanoNetwork::TestNet(0);
    let socket_path =
        TempDir::create_with_short_path("dmq_publisher_client_server", current_function_name)
            .join("node.socket");
    let (stop_tx, stop_rx) = watch::channel(());

    // Start the server
    let (signature_dmq_tx, signature_dmq_rx) = unbounded_channel::<DmqMessage>();
    let server = tokio::spawn({
        let socket_path = socket_path.clone();
        async move {
            let dmq_publisher_server = Arc::new(DmqPublisherServerPallas::new(
                socket_path.to_path_buf(),
                cardano_network,
                stop_rx,
                slog_scope::logger(),
            ));
            dmq_publisher_server
                .register_transmitter(signature_dmq_tx)
                .await
                .unwrap();
            dmq_publisher_server.run().await.unwrap();
        }
    });

    // Start a first client, publish messages and wait for its deconnection
    let client = tokio::spawn({
        let socket_path = socket_path.clone();
        async move {
            let dmq_builder = DmqMessageBuilder::new(
                {
                    let (kes_signature, operational_certificate) =
                        KesSignerFake::dummy_signature(current_function_name);
                    let kes_signer = KesSignerFake::new(vec![
                        Ok((kes_signature, operational_certificate.clone())),
                        Ok((kes_signature, operational_certificate.clone())),
                    ]);

                    Arc::new(kes_signer)
                },
                Arc::new(FakeChainObserver::default()),
            )
            .set_ttl(100);
            let publisher_client = DmqPublisherClientPallas::<DmqMessageTestPayload>::new(
                socket_path,
                cardano_network,
                dmq_builder,
                slog_scope::logger(),
            );

            publisher_client
                .publish_message(DmqMessageTestPayload::new(b"msg_1"))
                .await
                .unwrap();
            // Sleep to avoid refused connection from the server
            tokio::time::sleep(std::time::Duration::from_millis(10)).await;
            publisher_client
                .publish_message(DmqMessageTestPayload::new(b"msg_2"))
                .await
                .unwrap();
        }
    });
    client.await.unwrap();

    // Sleep to avoid refused connection from the server
    tokio::time::sleep(std::time::Duration::from_millis(10)).await;

    // Start a second client and publish messages
    let client = tokio::spawn({
        let socket_path = socket_path.clone();
        async move {
            let dmq_builder = DmqMessageBuilder::new(
                {
                    let (kes_signature, operational_certificate) =
                        KesSignerFake::dummy_signature(current_function_name);
                    let kes_signer = KesSignerFake::new(vec![
                        Ok((kes_signature, operational_certificate.clone())),
                        Ok((kes_signature, operational_certificate.clone())),
                    ]);

                    Arc::new(kes_signer)
                },
                Arc::new(FakeChainObserver::default()),
            )
            .set_ttl(100);
            let publisher_client = DmqPublisherClientPallas::<DmqMessageTestPayload>::new(
                socket_path,
                cardano_network,
                dmq_builder,
                slog_scope::logger(),
            );

            publisher_client
                .publish_message(DmqMessageTestPayload::new(b"msg_3"))
                .await
                .unwrap();

            stop_tx
                .send(())
                .expect("Failed to send stop signal to DMQ publisher server");
        }
    });

    // Record messages received by the server
    let recorder = tokio::spawn(async move {
        let messages: Vec<DmqMessage> = {
            let mut messages = vec![];
            let mut signature_dmq_rx = signature_dmq_rx;
            while let Some(message) = signature_dmq_rx.recv().await {
                messages.push(message);
            }

            messages
        };

        messages
    });

    // Check that all messages have been correctly received
    let (_, _, messages) = tokio::try_join!(server, client, recorder).unwrap();
    assert_eq!(
        vec![
            create_fake_msg(b"msg_1", current_function_name).await,
            create_fake_msg(b"msg_2", current_function_name).await,
            create_fake_msg(b"msg_3", current_function_name).await,
        ],
        messages
    );
}
