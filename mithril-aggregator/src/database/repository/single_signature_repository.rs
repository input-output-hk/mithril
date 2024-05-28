use std::sync::Arc;

use mithril_common::entities::SingleSignatures;
use mithril_common::StdResult;
use mithril_persistence::sqlite::{ConnectionExtensions, SqliteConnection};

use crate::database::query::UpdateSingleSignatureRecordQuery;
use crate::database::record::{OpenMessageRecord, SingleSignatureRecord};

/// Service to deal with single_signature (read & write).
pub struct SingleSignatureRepository {
    connection: Arc<SqliteConnection>,
}

impl SingleSignatureRepository {
    /// Create a new SingleSignatureStoreAdapter service
    pub fn new(connection: Arc<SqliteConnection>) -> Self {
        Self { connection }
    }

    /// Create a new Single Signature in database
    pub async fn create_single_signature(
        &self,
        single_signature: &SingleSignatures,
        open_message: &OpenMessageRecord,
    ) -> StdResult<SingleSignatureRecord> {
        let single_signature = SingleSignatureRecord::try_from_single_signatures(
            single_signature,
            &open_message.open_message_id,
            open_message.epoch.offset_to_signer_retrieval_epoch()?,
        )?;
        let record = self.connection.fetch_first(UpdateSingleSignatureRecordQuery::one(single_signature.clone()))?
            .unwrap_or_else(|| {
                panic!(
                    "No entity returned by the persister, single_signature_record = {single_signature:?}"
                )
            }) ;

        Ok(record)
    }
}
