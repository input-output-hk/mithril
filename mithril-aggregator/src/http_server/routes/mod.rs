mod aggregator_status;
mod artifact_routes;
mod certificate_routes;
mod epoch_routes;
mod middlewares;
mod proof_routes;
pub(crate) mod reply;
mod root_routes;
pub mod router;
mod signatures_routes;
mod signer_routes;
mod statistics_routes;

/// Match the given result and do an early return with an internal server error (500)
/// if it was an Error. Else return the unwrapped value.
#[macro_export]
macro_rules! unwrap_to_internal_server_error {
    ($code:expr, $logger:expr => $($warn_comment:tt)*) => {
        match $code {
            Ok(res) => res,
            Err(err) => {
                slog::warn!($logger, $($warn_comment)*; "error" => ?err);
                return Ok($crate::http_server::routes::reply::server_error(
                    err,
                ));
            }
        }
    };
}

pub(crate) fn http_server_child_logger(logger: &slog::Logger) -> slog::Logger {
    use mithril_common::logging::LoggerExtensions;
    logger.new_with_name("http_server")
}
