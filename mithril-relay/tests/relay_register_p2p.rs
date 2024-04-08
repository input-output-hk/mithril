use libp2p::Multiaddr;
use mithril_common::{
    messages::{RegisterSignatureMessage, RegisterSignerMessage},
    test_utils::test_http_server::test_http_server_with_socket_address,
    StdResult,
};
use mithril_relay::{p2p::Peer, AggregatorRelay, PassiveRelay, SignerRelay};
use reqwest::StatusCode;
use slog::{Drain, Level, Logger};
use slog_scope::{error, info};
use std::{convert::Infallible, sync::Arc, time::Duration};
use tokio::{
    sync::mpsc::{self, Sender},
    task::JoinSet,
};
use warp::{http::StatusCode as WarpStatusCode, Filter};

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

enum Relay {
    Passive(PassiveRelay),
    Signer(SignerRelay),
    Aggregator(AggregatorRelay),
}

impl Relay {
    async fn tick(&mut self) -> StdResult<()> {
        match self {
            Relay::Passive(passive_relay) => passive_relay.tick().await,
            Relay::Signer(signer_relay) => signer_relay.tick().await,
            Relay::Aggregator(aggregator_relay) => aggregator_relay.tick().await,
        }
    }

    fn peer(&self) -> &Peer {
        match self {
            Relay::Passive(passive_relay) => passive_relay.peer(),
            Relay::Signer(signer_relay) => signer_relay.peer(),
            Relay::Aggregator(aggregator_relay) => aggregator_relay.peer(),
        }
    }

    fn type_name(&self) -> String {
        match self {
            Relay::Passive(_) => "PassiveRelay".to_string(),
            Relay::Signer(_) => "SignerRelay".to_string(),
            Relay::Aggregator(_) => "AggregatorRelay".to_string(),
        }
    }

    async fn tick_loop(
        mut self,
        peer_connected_tx: mpsc::Sender<()>,
        total_peers: usize,
    ) -> StdResult<()> {
        loop {
            if let Err(err) = self.tick().await {
                error!("{}: tick error", self.type_name(); "error" => format!("{err:#?}"));
            }
            if self.peer().connected_peers().len() == total_peers - 1 {
                peer_connected_tx.send(()).await?;
            }
        }
    }
}

#[tokio::test]
async fn should_relay_registration_with_p2p_pubsub() {
    let log_level = Level::Debug;
    let _guard = slog_scope::set_global_logger(build_logger(log_level));
    let mut join_set: JoinSet<StdResult<()>> = JoinSet::new();
    let p2p_addr: Multiaddr = "/ip4/0.0.0.0/tcp/0".parse().unwrap();
    let http_server_port = 0;

    let total_signer_relay = 1;
    let total_aggregator_relay = 1;
    let total_peers = 1 + total_signer_relay + total_aggregator_relay;
    let (peer_connected_tx, peer_connected_rx) = mpsc::channel(total_peers);
    let mut aggregator_endpoints = vec![];
    let mut relays = vec![];

    // Start the callbacks test server
    let (signer_http_tx, signer_http_rx) = mpsc::channel(total_peers * 100);
    let (signature_http_tx, signature_http_rx) = mpsc::channel(total_peers * 100);
    fn with_transmitter<T: Send + Sync>(
        tx: Sender<T>,
    ) -> impl Filter<Extract = (Sender<T>,), Error = Infallible> + Clone {
        warp::any().map(move || tx.clone())
    }
    async fn register_handler(tx: Sender<()>) -> Result<impl warp::Reply, Infallible> {
        match tx.send(()).await {
            Ok(_) => Ok(Box::new(warp::reply::with_status(
                "".to_string(),
                WarpStatusCode::CREATED,
            ))),
            Err(err) => Ok(Box::new(warp::reply::with_status(
                format!("{err:?}"),
                WarpStatusCode::INTERNAL_SERVER_ERROR,
            ))),
        }
    }
    for _ in 0..total_aggregator_relay {
        let callback_http_server = test_http_server_with_socket_address(
            warp::path("register-signatures")
                .and(warp::post())
                .and(with_transmitter(signer_http_tx.clone()))
                .and_then(register_handler)
                .or(warp::path("register-signer")
                    .and(warp::post())
                    .and(with_transmitter(signature_http_tx.clone()))
                    .and_then(register_handler)),
            ([0, 0, 0, 0], http_server_port).into(),
        );
        let callback_address = callback_http_server.address();
        aggregator_endpoints.push(format!("http://{}/", callback_address));
    }

    // Passive relay, the entry peer in the P2P network
    let passive_relay = PassiveRelay::start(&p2p_addr).await.unwrap();
    let passive_relay_peer_address = passive_relay.peer_address().unwrap();
    relays.push(Relay::Passive(passive_relay));

    // Signer relays, which will receive HTTP calls from the signers
    let signer_repeater_delay = Duration::from_secs(100);
    let mut signer_relay_addresses = vec![];
    for i in 0..total_signer_relay {
        let mut signer_relay = SignerRelay::start(
            &p2p_addr,
            &http_server_port,
            &aggregator_endpoints[i],
            &signer_repeater_delay,
        )
        .await
        .unwrap();
        signer_relay
            .dial_peer(passive_relay_peer_address.clone())
            .unwrap();
        signer_relay_addresses.push(signer_relay.address());
        relays.push(Relay::Signer(signer_relay));
    }

    // Aggregator relays, which will make HTTP callbacks to recorder endpoint
    for i in 0..total_aggregator_relay {
        let mut aggregator_relay = AggregatorRelay::start(&p2p_addr, &aggregator_endpoints[i])
            .await
            .unwrap();
        aggregator_relay
            .dial_peer(passive_relay_peer_address.clone())
            .unwrap();
        relays.push(Relay::Aggregator(aggregator_relay));
    }

    // Loop tick all relays
    for relay in relays {
        join_set.spawn(relay.tick_loop(peer_connected_tx.clone(), total_peers));
    }

    // Wait for all peers to connect to the P2P network
    async fn wait_for_all_peers_to_connect(
        mut peer_connected_rx: mpsc::Receiver<()>,
        total_peers: usize,
    ) {
        let mut connected_peers = 0;
        while connected_peers < total_peers {
            peer_connected_rx.recv().await.unwrap();
            connected_peers += 1;
        }
    }
    tokio::select! {
        _ = wait_for_all_peers_to_connect(peer_connected_rx, total_peers) => {
            info!("All peers connected to the P2P network");
        }
        _ = tokio::time::sleep(Duration::from_secs(10)) => {
            panic!("Peers connection timed out");
         }
    }
    tokio::time::sleep(Duration::from_secs(10)).await;

    // Send signer registration messages to the relays
    for signer_relay_address in &signer_relay_addresses {
        let mut signer_message_sent = RegisterSignerMessage::dummy();
        signer_message_sent.party_id = format!("{}-new", signer_message_sent.party_id);
        let response = reqwest::Client::new()
            .post(format!("http://{}/register-signer", signer_relay_address))
            .json(&signer_message_sent)
            .send()
            .await;
        match response {
            Ok(response) => match response.status() {
                StatusCode::CREATED => {}
                status => {
                    panic!("Post `/register-signer` should have returned a 201 status code, got: {status}")
                }
            },
            Err(err) => panic!("Post `/register-signer` failed: {err:?}"),
        }
    }

    // Send signature registration messages to the relays
    for signer_relay_address in &signer_relay_addresses {
        let mut signature_message_sent = RegisterSignatureMessage::dummy();
        signature_message_sent.party_id = format!("{}-new", signature_message_sent.party_id);
        let response = reqwest::Client::new()
            .post(format!(
                "http://{}/register-signatures",
                signer_relay_address
            ))
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
    }

    // Wait for all signer registrations to be received by the callback servers
    async fn wait_for_all_callbacks(mut callback_rx: mpsc::Receiver<()>, total_callbacks: usize) {
        let mut received_callbacks = 0;
        while received_callbacks < total_callbacks {
            callback_rx.recv().await.unwrap();
            received_callbacks += 1;
        }
    }
    tokio::select! {
        _ = wait_for_all_callbacks(signer_http_rx, total_peers) => {
            info!("All signer callbacks received from the P2P network");
        }
        _ = tokio::time::sleep(Duration::from_secs(10)) => {
            panic!("Signer callbacks reception timed out");
         }
    }

    // Wait for all signature registrations to be received by the callback servers
    tokio::select! {
        _ = wait_for_all_callbacks(signature_http_rx, total_peers) => {
            info!("All signature callbacks received from the P2P network");
        }
        _ = tokio::time::sleep(Duration::from_secs(10)) => {
            panic!("Signature callbacks reception timed out");
         }
    }

    join_set.abort_all();
}
