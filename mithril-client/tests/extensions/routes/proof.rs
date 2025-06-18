use axum::extract::State;
use axum::routing::get;
use axum::{Json, Router};

use mithril_client::CardanoTransactionsProofs;

#[derive(Debug, Clone)]
struct ProofRoutesState {
    proof: CardanoTransactionsProofs,
}

pub fn routes(proof: CardanoTransactionsProofs) -> Router {
    let state = ProofRoutesState { proof };

    Router::new()
        .route("/proof/cardano-transaction", get(proof_cardano_transaction))
        .with_state(state)
}

/// Route: /proof/cardano-transaction
async fn proof_cardano_transaction(
    state: State<ProofRoutesState>,
) -> Json<CardanoTransactionsProofs> {
    Json(state.proof.clone())
}
