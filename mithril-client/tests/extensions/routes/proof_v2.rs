use axum::extract::State;
use axum::routing::get;
use axum::{Json, Router};

use mithril_client::CardanoTransactionsProofs;

#[derive(Debug, Clone)]
struct ProofRoutesState {
    transaction_proof: CardanoTransactionsProofs, //TODO transaction proof v2
    block_proof: CardanoTransactionsProofs,       //TODO block proof
}

pub fn routes(
    transaction_proof: CardanoTransactionsProofs,
    block_proof: CardanoTransactionsProofs,
) -> Router {
    let state = ProofRoutesState {
        transaction_proof,
        block_proof,
    };

    Router::new()
        .route(
            "/proof/v2/cardano-transaction",
            get(proof_v2_cardano_transaction),
        )
        .route("/proof/v2/cardano-block", get(proof_v2_cardano_block))
        .with_state(state)
}

/// Route: /proof/v2/cardano-transaction
async fn proof_v2_cardano_transaction(
    state: State<ProofRoutesState>,
) -> Json<CardanoTransactionsProofs> {
    Json(state.transaction_proof.clone())
}

/// Route: /proof/v2/cardano-block
async fn proof_v2_cardano_block(state: State<ProofRoutesState>) -> Json<CardanoTransactionsProofs> {
    Json(state.block_proof.clone())
}
