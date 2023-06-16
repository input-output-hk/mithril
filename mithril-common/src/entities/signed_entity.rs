use crate::signable_builder::Artifact;
use chrono::{DateTime, Utc};

use super::SignedEntityType;

/// Aggregate for signed entity
#[derive(Debug, Clone)]
pub struct SignedEntity<T>
where
    T: Artifact,
{
    /// Signed entity id.
    pub signed_entity_id: String,

    /// Signed entity type.
    pub signed_entity_type: SignedEntityType,

    /// Certificate id for this signed entity.
    pub certificate_id: String,

    /// Artifact
    pub artifact: T,

    /// Date and time when the signed_entity was created
    pub created_at: DateTime<Utc>,
}
