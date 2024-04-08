use std::{sync::Arc, time::Duration};

use libp2p::{gossipsub, Multiaddr};
use mithril_common::messages::{RegisterSignatureMessage, RegisterSignerMessage};
use mithril_relay::{
    p2p::{BroadcastMessage, PeerBehaviourEvent, PeerEvent},
    PassiveRelay, SignerRelay,
};
use reqwest::StatusCode;
use slog::{Drain, Level, Logger};
use slog_scope::{error, info};

// Launch a relay that connects to P2P network. The relay is a peer in the P2P
// network. The relay sends some signer regsitrations that must be received by other
// relays.
// TODO: this test is not optimal and should be refactored for better performances,
// handling a variable number of peers and with test extensions to avoid code duplication

fn build_logger(log_level: Level) -> Logger {
    let decorator = slog_term::TermDecorator::new().build();
    let drain = slog_term::CompactFormat::new(decorator).build().fuse();
    let drain = slog::LevelFilter::new(drain, log_level).fuse();
    let drain = slog_async::Async::new(drain).build().fuse();

    Logger::root(Arc::new(drain), slog::o!())
}

#[tokio::test]
async fn should_receive_registrations_from_signers_when_subscribed_to_pubsub() {
    let log_level = Level::Info;
    let _guard = slog_scope::set_global_logger(build_logger(log_level));

    let total_p2p_client = 2;
    let total_peers = 1 + total_p2p_client;
    let addr: Multiaddr = "/ip4/0.0.0.0/tcp/0".parse().unwrap();
    let server_port = 0;
    let aggregator_endpoint = "http://0.0.0.0:1234".to_string();
    let signer_repeater_delay = Duration::from_secs(100);
    let mut signer_relay = SignerRelay::start(
        &addr,
        &server_port,
        &aggregator_endpoint,
        &signer_repeater_delay,
    )
    .await
    .expect("Relay start failed");
    let relay_address = signer_relay.address();
    let relay_peer_address = signer_relay.peer_address().unwrap();
    info!("Test: relay_address is '{relay_address:?}'");

    let mut p2p_client1 = PassiveRelay::start(&addr)
        .await
        .expect("P2P client start failed");
    p2p_client1
        .peer
        .dial(relay_peer_address.clone())
        .expect("P2P client dial to the relay should not fail");

    let mut p2p_client2 = PassiveRelay::start(&addr)
        .await
        .expect("P2P client start failed");
    p2p_client2
        .peer
        .dial(relay_peer_address.clone())
        .expect("P2P client dial to the relay should not fail");

    info!("Test: wait for Relay and P2P clients to subscribe to the pubsub topic");
    let mut total_peers_connected = 0;
    loop {
        info!("Test: subscribed peers: {total_peers_connected}/{total_peers}");
        tokio::select! {
            event =  signer_relay.tick_peer() => {
                if let Ok(Some(PeerEvent::Behaviour {
                    event: PeerBehaviourEvent::Gossipsub(gossipsub::Event::Subscribed { .. }),
                })) = event
                {
                    info!("Test: relay has subscribed to gossipsub topic");
                    total_peers_connected += 1;
                }
            },
            event =  p2p_client1.tick_peer() => {
                if let Ok(Some(PeerEvent::Behaviour {
                    event: PeerBehaviourEvent::Gossipsub(gossipsub::Event::Subscribed { .. }),
                })) = event
                {
                    info!("Test: client1 has subscribed to gossipsub topic");
                    total_peers_connected += 1;
                }
            }
            event =  p2p_client2.tick_peer() => {
                if let Ok(Some(PeerEvent::Behaviour {
                    event: PeerBehaviourEvent::Gossipsub(gossipsub::Event::Subscribed { .. }),
                })) = event
                {
                    info!("Test: client2 has subscribed to gossipsub topic");
                    total_peers_connected += 1;
                }
            }
        }
        if total_peers_connected == total_peers {
            info!("Test: all peers are connected to the gossipsub topic");
            break;
        }
    }

    let signer_relay_thread = tokio::spawn(async move {
        loop {
            if let Err(err) = signer_relay.tick().await {
                error!("RelaySigner: tick error"; "error" => format!("{err:#?}"));
            }
        }
    });

    info!("Test: send a signer registration to the relay via HTTP gateway");
    let mut signer_message_sent = RegisterSignerMessage::dummy();
    signer_message_sent.party_id = format!("{}-new", signer_message_sent.party_id);
    let response = reqwest::Client::new()
        .post(format!("http://{}/register-signer", relay_address))
        .json(&signer_message_sent)
        .send()
        .await;
    match response {
        Ok(response) => {
            match response.status() {
                StatusCode::CREATED => {}
                status => {
                    panic!("Post `/register-signer` should have returned a 201 status code, got: {status}")
                }
            }
        }
        Err(err) => panic!("Post `/register-signer` failed: {err:?}"),
    }

    info!("Test: wait for P2P clients to receive the signer registrations");
    let mut total_peers_has_received_message = 0;
    loop {
        tokio::select! {
            event =  p2p_client1.tick_peer() => {
                if let Ok(Some(BroadcastMessage::RegisterSigner(signer_message_received))) = p2p_client1.convert_peer_event_to_message(event.unwrap().unwrap())
                {
                    info!("Test: client1 consumed signer registration: {signer_message_received:#?}");
                    assert_eq!(signer_message_sent, signer_message_received);
                    total_peers_has_received_message += 1
                }
            }
            event =  p2p_client2.tick_peer() => {
                if let Ok(Some(BroadcastMessage::RegisterSigner(signer_message_received))) = p2p_client2.convert_peer_event_to_message(event.unwrap().unwrap())
                {
                    info!("Test: client2 consumed signer registration: {signer_message_received:#?}");
                    assert_eq!(signer_message_sent, signer_message_received);
                    total_peers_has_received_message += 1
                }
            }
        }
        if total_peers_has_received_message == total_p2p_client {
            info!("Test: All P2P clients have consumed the signer registration");
            break;
        }
    }

    info!("Test: send a signature to the relay via HTTP gateway");
    let mut signature_message_sent = RegisterSignatureMessage::dummy();
    signature_message_sent.party_id = format!("{}-new", signature_message_sent.party_id);
    let response = reqwest::Client::new()
        .post(format!("http://{}/register-signatures", relay_address))
        .json(&signature_message_sent)
        .send()
        .await;
    match response {
        Ok(response) => match response.status() {
            StatusCode::CREATED => {}
            status => {
                panic!("Post `/register-signatures` should have returned a 201 status code, got: {status}")
            }
        },
        Err(err) => panic!("Post `/register-signatures` failed: {err:?}"),
    }

    info!("Test: wait for P2P clients to receive the signature");
    let mut total_peers_has_received_message = 0;
    loop {
        tokio::select! {
            event =  p2p_client1.tick_peer() => {
                if let Ok(Some(BroadcastMessage::RegisterSignature(signature_message_received))) = p2p_client1.convert_peer_event_to_message(event.unwrap().unwrap())
                {
                    info!("Test: client1 consumed signature: {signature_message_received:#?}");
                    assert_eq!(signature_message_sent, signature_message_received);
                    total_peers_has_received_message += 1
                }
            }
            event =  p2p_client2.tick_peer() => {
                if let Ok(Some(BroadcastMessage::RegisterSignature(signature_message_received))) = p2p_client2.convert_peer_event_to_message(event.unwrap().unwrap())
                {
                    info!("Test: client2 consumed signature: {signature_message_received:#?}");
                    assert_eq!(signature_message_sent, signature_message_received);
                    total_peers_has_received_message += 1
                }
            }
        }
        if total_peers_has_received_message == total_p2p_client {
            info!("Test: All P2P clients have consumed the signature");
            break;
        }
    }

    signer_relay_thread.abort();
}
