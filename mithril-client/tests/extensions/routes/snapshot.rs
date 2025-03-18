use super::middleware::with_calls_middleware;
use crate::extensions::fake::{FakeAggregator, FakeAggregatorCalls};
use mithril_client::Snapshot;
use std::{
    convert::Infallible,
    path::PathBuf,
    sync::{Arc, RwLock},
};
use warp::{filters::path::FullPath, Filter};

pub fn routes(
    calls: FakeAggregatorCalls,
    snapshots_returned_value: String,
    snapshot_by_id_returned_value: Arc<RwLock<Snapshot>>,
) -> impl Filter<Extract = (impl warp::Reply,), Error = warp::Rejection> + Clone {
    snapshots(calls.clone(), snapshots_returned_value)
        .or(snapshot_by_id(calls.clone(), snapshot_by_id_returned_value))
}

/// Route: /artifact/snapshots
fn snapshots(
    calls: FakeAggregatorCalls,
    returned_value: String,
) -> impl Filter<Extract = (impl warp::Reply,), Error = warp::Rejection> + Clone {
    warp::path!("artifact" / "snapshots")
        .and(warp::path::full().map(move |p| p))
        .and(with_calls_middleware(calls.clone()))
        .and_then(move |fullpath, calls| {
            FakeAggregator::store_call_and_return_value(fullpath, calls, returned_value.clone())
        })
}

/// Route: /artifact/snapshot/:id
fn snapshot_by_id(
    calls: FakeAggregatorCalls,
    returned_value: Arc<RwLock<Snapshot>>,
) -> impl Filter<Extract = (impl warp::Reply,), Error = warp::Rejection> + Clone {
    warp::path!("artifact" / "snapshot" / String)
        .and(warp::path::full().map(move |p| p))
        .and(with_calls_middleware(calls.clone()))
        .and_then(move |_param, fullpath, calls| {
            let data = returned_value.read().unwrap();
            FakeAggregator::store_call_and_return_value(
                fullpath,
                calls,
                serde_json::to_string(&data.clone()).unwrap(),
            )
        })
}

/// Route: /snapshot_download/:filename:
pub fn download(
    calls: FakeAggregatorCalls,
    archive_path: PathBuf,
) -> impl Filter<Extract = (impl warp::Reply,), Error = warp::Rejection> + Clone {
    warp::path("snapshot_download")
        .and(warp::fs::dir(archive_path))
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
