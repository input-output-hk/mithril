use axum::extract::{Path, State};
use axum::routing::get;
use axum::{Json, Router};

use mithril_common::messages::{
    MithrilStakeDistributionListItemMessage, MithrilStakeDistributionMessage,
};

#[derive(Debug, Clone)]
struct MithrilStakeDistributionRoutesState {
    mithril_stake_distribution_list: Vec<MithrilStakeDistributionListItemMessage>,
    mithril_stake_distribution: MithrilStakeDistributionMessage,
}

pub fn routes(
    mithril_stake_distribution_list: Vec<MithrilStakeDistributionListItemMessage>,
    mithril_stake_distribution: MithrilStakeDistributionMessage,
) -> Router {
    let state = MithrilStakeDistributionRoutesState {
        mithril_stake_distribution_list,
        mithril_stake_distribution,
    };

    Router::new()
        .route(
            "/artifact/mithril-stake-distributions",
            get(mithril_stake_distributions),
        )
        .route(
            "/artifact/mithril-stake-distribution/{hash}",
            get(mithril_stake_distribution_by_hash),
        )
        .with_state(state)
}

/// Route: /artifact/mithril-stake-distributions
async fn mithril_stake_distributions(
    state: State<MithrilStakeDistributionRoutesState>,
) -> Json<Vec<MithrilStakeDistributionListItemMessage>> {
    Json(state.mithril_stake_distribution_list.clone())
}

/// Route: /artifact/mithril-stake-distribution/{hash}
async fn mithril_stake_distribution_by_hash(
    Path(_hash): Path<String>,
    state: State<MithrilStakeDistributionRoutesState>,
) -> Json<MithrilStakeDistributionMessage> {
    Json(state.mithril_stake_distribution.clone())
}
