use serde::Serialize;
use warp::http::StatusCode;

use mithril_common::entities::ClientError;

pub fn json<T>(value: &T, status_code: StatusCode) -> Box<dyn warp::Reply>
where
    T: Serialize,
{
    Box::new(warp::reply::with_status(
        warp::reply::json(value),
        status_code,
    ))
}

pub fn empty(status_code: StatusCode) -> Box<dyn warp::Reply> {
    Box::new(warp::reply::with_status(warp::reply::reply(), status_code))
}

pub fn bad_request(label: String, message: String) -> Box<dyn warp::Reply> {
    json(&ClientError::new(label, message), StatusCode::BAD_REQUEST)
}
