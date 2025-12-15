use axum::extract::{Path, State};
use axum::routing::get;
use axum::{Json, Router};
use mithril_client::{MithrilCertificate, MithrilCertificateListItem};

#[derive(Debug, Clone)]
struct CertificateRoutesState {
    certificate_list: Vec<MithrilCertificateListItem>,
    certificate: MithrilCertificate,
}

pub fn routes(
    certificate_list: Vec<MithrilCertificateListItem>,
    certificate: MithrilCertificate,
) -> Router {
    let state = CertificateRoutesState {
        certificate_list,
        certificate,
    };

    Router::new()
        .route("/certificates", get(certificates))
        .route("/certificate/{certificate_hash}", get(certificate_by_hash))
        .with_state(state)
}

/// Route: /certificates
async fn certificates(
    state: State<CertificateRoutesState>,
) -> Json<Vec<MithrilCertificateListItem>> {
    Json(state.certificate_list.clone())
}

/// Route: /certificate/{certificate_hash}
async fn certificate_by_hash(
    Path(_hash): Path<String>,
    state: State<CertificateRoutesState>,
) -> Json<MithrilCertificate> {
    Json(state.certificate.clone())
}
