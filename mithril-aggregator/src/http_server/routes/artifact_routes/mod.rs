pub mod cardano_transaction;
pub mod mithril_stake_distribution;
pub mod snapshot;

#[cfg(test)]
pub mod test_utils {
    use chrono::{DateTime, Utc};
    use mithril_common::entities::{SignedEntity, SignedEntityType};
    use mithril_common::signable_builder::Artifact;

    pub fn create_signed_entities<T>(
        signed_entity_type: SignedEntityType,
        records: Vec<T>,
    ) -> Vec<SignedEntity<T>>
    where
        T: Artifact,
    {
        records
            .into_iter()
            .enumerate()
            .map(|(idx, record)| SignedEntity {
                signed_entity_id: format!("{idx}"),
                signed_entity_type: signed_entity_type.to_owned(),
                certificate_id: format!("certificate-{idx}"),
                artifact: record,
                created_at: DateTime::parse_from_rfc3339("2023-01-19T13:43:05.618857482Z")
                    .unwrap()
                    .with_timezone(&Utc),
            })
            .collect()
    }
}
