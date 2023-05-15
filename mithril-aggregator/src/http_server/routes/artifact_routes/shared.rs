pub mod handlers {
    use crate::database::provider::SignedEntityStorer;
    use crate::http_server::routes::reply;
    use mithril_common::entities::SignedEntityType;
    use mithril_common::messages::MessageAdapter;
    use serde::{Deserialize, Serialize};
    use slog_scope::{debug, warn};
    use std::convert::Infallible;
    use std::sync::Arc;
    use warp::http::StatusCode;

    pub const LIST_MAX_ITEMS: usize = 20;

    /// List Artifacts by signed entity type
    pub async fn artifacts_by_signed_entity_type<
        T: Serialize + for<'a> Deserialize<'a>,
        U: MessageAdapter<Vec<T>, V>,
        V: Serialize + for<'a> Deserialize<'a>,
    >(
        signed_entity_storer: Arc<dyn SignedEntityStorer>,
        signed_entity_type: SignedEntityType,
    ) -> Result<impl warp::Reply, Infallible> {
        debug!("⇄ HTTP SERVER: artifacts");

        match signed_entity_storer
            .get_last_signed_entities_by_type(&signed_entity_type, LIST_MAX_ITEMS)
            .await
        {
            Ok(signed_entities) => {
                let mut artifacts = Vec::new();
                for signed_entity in signed_entities {
                    if let Ok(artifact) = serde_json::from_str::<T>(&signed_entity.artifact) {
                        artifacts.push(artifact)
                    }
                }
                let messages = U::adapt(artifacts);
                Ok(reply::json(&messages, StatusCode::OK))
            }
            Err(err) => {
                warn!("artifacts_by_signed_entity_type"; "signed_entity_type" => ?signed_entity_type, "error" => ?err);
                Ok(reply::internal_server_error(err.to_string()))
            }
        }
    }

    /// Get Artifact by signed entity id
    pub async fn artifact_by_signed_entity_id<
        T: Serialize + for<'a> Deserialize<'a>,
        U: MessageAdapter<T, V>,
        V: Serialize + for<'a> Deserialize<'a>,
    >(
        signed_entity_id: String,
        signed_entity_storer: Arc<dyn SignedEntityStorer>,
    ) -> Result<impl warp::Reply, Infallible> {
        debug!("⇄ HTTP SERVER: artifact/{signed_entity_id}");

        match signed_entity_storer
            .get_signed_entity(signed_entity_id.clone())
            .await
        {
            Ok(signed_entity) => match signed_entity {
                Some(signed_entity) => match serde_json::from_str::<T>(&signed_entity.artifact) {
                    Ok(artifact) => Ok(reply::json(&U::adapt(artifact), StatusCode::OK)),
                    Err(err) => Ok(reply::internal_server_error(err.to_string())),
                },
                None => Ok(reply::empty(StatusCode::NOT_FOUND)),
            },
            Err(err) => {
                warn!("artifact_by_signed_entity_id"; "signed_entity_id" => ?signed_entity_id, "error" => ?err);
                Ok(reply::internal_server_error(err.to_string()))
            }
        }
    }
}

#[cfg(test)]
pub mod tests {
    use mithril_common::entities::SignedEntityType;
    use serde::Serialize;

    use crate::database::provider::SignedEntityRecord;

    pub fn create_signed_entity_records<T: Serialize>(
        signed_entity_type: SignedEntityType,
        records: Vec<T>,
    ) -> Vec<SignedEntityRecord> {
        records
            .into_iter()
            .enumerate()
            .map(|(idx, record)| SignedEntityRecord {
                signed_entity_id: format!("{idx}"),
                signed_entity_type: signed_entity_type.to_owned(),
                certificate_id: format!("certificate-{idx}"),
                artifact: serde_json::to_string(&record).unwrap(),
                created_at: "2023-01-19T13:43:05.618857482Z".to_string(),
            })
            .collect()
    }
}
