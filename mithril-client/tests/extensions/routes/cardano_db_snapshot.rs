use std::{
    convert::Infallible,
    path::PathBuf,
    sync::{Arc, RwLock},
};

use warp::{filters::path::FullPath, Filter};

use mithril_client::CardanoDatabaseSnapshot;

use crate::extensions::fake_aggregator::FakeAggregatorCalls;

use super::middleware::with_calls_middleware;

pub fn routes(
    cardano_db_snapshots_returned_value: String,
    cardano_db_snapshot_by_id_returned_value: Arc<RwLock<CardanoDatabaseSnapshot>>,
) -> impl Filter<Extract = (impl warp::Reply,), Error = warp::Rejection> + Clone {
    cardano_db_snapshots(cardano_db_snapshots_returned_value).or(cardano_db_snapshot_by_id(
        cardano_db_snapshot_by_id_returned_value,
    ))
}

/// Route: /artifact/cardano-database
fn cardano_db_snapshots(
    returned_value: String,
) -> impl Filter<Extract = (impl warp::Reply,), Error = warp::Rejection> + Clone {
    warp::path!("artifact" / "cardano-database").map(move || returned_value.clone())
}

/// Route: /artifact/cardano-database/:hash
fn cardano_db_snapshot_by_id(
    returned_value: Arc<RwLock<CardanoDatabaseSnapshot>>,
) -> impl Filter<Extract = (impl warp::Reply,), Error = warp::Rejection> + Clone {
    warp::path!("artifact" / "cardano-database" / String)
        .map(move |_| serde_json::to_string(&returned_value.clone()).unwrap())
}

/// Route: /cardano-database-download
pub fn download_immutables_archive(
    calls: FakeAggregatorCalls,
    immutable_archives_path: PathBuf,
) -> impl Filter<Extract = (impl warp::Reply,), Error = warp::Rejection> + Clone {
    warp::path("cardano-database-download")
        .and(warp::fs::dir(immutable_archives_path))
        .and(warp::path::full().map(move |p| p))
        .and(with_calls_middleware(calls.clone()))
        .and_then(store_call_and_download_return)
}

async fn store_call_and_download_return(
    reply: warp::fs::File,
    full_path: FullPath,
    calls: FakeAggregatorCalls,
) -> Result<impl warp::Reply, Infallible> {
    let mut call_list = calls.lock().await;
    call_list.push(full_path.as_str().to_string());

    let filepath = reply.path().to_path_buf();
    Ok(Box::new(warp::reply::with_header(
        reply,
        "Content-Disposition",
        format!(
            "attachment; filename=\"{}\"",
            filepath.file_name().unwrap().to_str().unwrap()
        ),
    )) as Box<dyn warp::Reply>)
}
