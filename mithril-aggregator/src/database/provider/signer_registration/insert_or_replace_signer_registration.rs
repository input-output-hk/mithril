use sqlite::Value;

use mithril_common::StdResult;
use mithril_persistence::sqlite::{
    Provider, SourceAlias, SqLiteEntity, SqliteConnection, WhereCondition,
};

use crate::database::record::SignerRegistrationRecord;

/// Query to insert or replace [SignerRegistrationRecord] in the sqlite database
pub struct InsertOrReplaceSignerRegistrationRecordProvider<'conn> {
    connection: &'conn SqliteConnection,
}

impl<'conn> InsertOrReplaceSignerRegistrationRecordProvider<'conn> {
    /// Create a new instance
    pub fn new(connection: &'conn SqliteConnection) -> Self {
        Self { connection }
    }

    pub fn get_insert_or_replace_condition(
        &self,
        signer_registration_record: SignerRegistrationRecord,
    ) -> WhereCondition {
        WhereCondition::new(
            "(signer_id, epoch_setting_id, verification_key, verification_key_signature, operational_certificate, kes_period, stake, created_at) values (?*, ?*, ?*, ?*, ?*, ?*, ?*, ?*)",
            vec![
                Value::String(signer_registration_record.signer_id),
                Value::Integer(
                    signer_registration_record.epoch_setting_id.try_into().unwrap(),
                ),
                Value::String(signer_registration_record.verification_key),
                signer_registration_record
                    .verification_key_signature
                    .map(Value::String)
                    .unwrap_or(Value::Null),
                signer_registration_record
                    .operational_certificate
                    .map(Value::String)
                    .unwrap_or(Value::Null),
                signer_registration_record
                    .kes_period
                    .map(|k| Value::Integer(i64::from(k)))
                    .unwrap_or(Value::Null),
                signer_registration_record
                    .stake
                    .map(|s| Value::Integer(i64::try_from(s).unwrap()))
                    .unwrap_or(Value::Null),
                Value::String(signer_registration_record.created_at.to_rfc3339()),
            ],
        )
    }

    pub fn persist(
        &self,
        signer_registration_record: SignerRegistrationRecord,
    ) -> StdResult<SignerRegistrationRecord> {
        let filters = self.get_insert_or_replace_condition(signer_registration_record.clone());

        let entity = self.find(filters)?.next().unwrap_or_else(|| {
            panic!(
                "No entity returned by the persister, signer_registration_record = {signer_registration_record:?}"
            )
        });

        Ok(entity)
    }
}

impl<'conn> Provider<'conn> for InsertOrReplaceSignerRegistrationRecordProvider<'conn> {
    type Entity = SignerRegistrationRecord;

    fn get_connection(&'conn self) -> &'conn SqliteConnection {
        self.connection
    }

    fn get_definition(&self, condition: &str) -> String {
        // it is important to alias the fields with the same name as the table
        // since the table cannot be aliased in a RETURNING statement in SQLite.
        let projection = Self::Entity::get_projection().expand(SourceAlias::new(&[(
            "{:signer_registration:}",
            "signer_registration",
        )]));

        format!("insert or replace into signer_registration {condition} returning {projection}")
    }
}

#[cfg(test)]
mod tests {
    use mithril_common::entities::{Epoch, SignerWithStake};
    use mithril_common::test_utils::MithrilFixtureBuilder;

    use crate::database::test_helper::main_db_connection;

    use super::*;

    #[test]
    fn test_update_signer_registration_record() {
        let fixture = MithrilFixtureBuilder::default().with_signers(5).build();
        let signer_with_stakes = fixture.signers_with_stake();
        let signer_with_stakes_copy = signer_with_stakes
            .iter()
            .map(|s| {
                let mut s_new = s.clone();
                s_new.stake += 10;
                s_new
            })
            .collect::<Vec<SignerWithStake>>();

        let connection = main_db_connection().unwrap();
        let provider = InsertOrReplaceSignerRegistrationRecordProvider::new(&connection);

        for signer_with_stake in signer_with_stakes {
            let signer_registration_record =
                SignerRegistrationRecord::from_signer_with_stake(signer_with_stake, Epoch(1));
            let signer_registration_record_saved = provider
                .persist(signer_registration_record.clone())
                .unwrap();
            assert_eq!(signer_registration_record, signer_registration_record_saved);
        }

        for signer_with_stake in signer_with_stakes_copy {
            let signer_registration_record =
                SignerRegistrationRecord::from_signer_with_stake(signer_with_stake, Epoch(1));
            let signer_registration_record_saved = provider
                .persist(signer_registration_record.clone())
                .unwrap();
            assert_eq!(signer_registration_record, signer_registration_record_saved);
        }
    }
}
