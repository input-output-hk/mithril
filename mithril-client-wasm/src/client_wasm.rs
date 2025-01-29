use async_trait::async_trait;
use chrono::TimeDelta;
use serde::Serialize;
use std::sync::Arc;
use wasm_bindgen::prelude::*;

use mithril_client::{
    certificate_client::CertificateVerifierCache,
    common::Epoch,
    feedback::{FeedbackReceiver, MithrilEvent},
    CardanoTransactionsProofs, Client, ClientBuilder, ClientOptions, MessageBuilder,
    MithrilCertificate,
};

use crate::certificate_verification_cache::LocalStorageCertificateVerifierCache;
use crate::WasmResult;

#[wasm_bindgen]
struct JSBroadcastChannelFeedbackReceiver {
    channel: String,
}

impl JSBroadcastChannelFeedbackReceiver {
    pub fn new(channel: &str) -> Self {
        Self {
            channel: channel.to_string(),
        }
    }
}

#[cfg_attr(target_family = "wasm", async_trait(?Send))]
#[cfg_attr(not(target_family = "wasm"), async_trait)]
impl FeedbackReceiver for JSBroadcastChannelFeedbackReceiver {
    async fn handle_event(&self, event: MithrilEvent) {
        let event = MithrilEventWasm::from(event);
        let bc = web_sys::BroadcastChannel::new(&self.channel).unwrap();
        let _ = bc.post_message(&serde_wasm_bindgen::to_value(&event).unwrap());
        bc.close();
    }
}

#[derive(Serialize)]
struct MithrilEventWasm {
    #[serde(rename = "type")]
    event_type: String,
    #[serde(rename = "payload")]
    event_data: MithrilEvent,
}

impl From<MithrilEvent> for MithrilEventWasm {
    fn from(event: MithrilEvent) -> Self {
        Self {
            event_type: event.to_string(),
            event_data: event,
        }
    }
}

/// Structure that wraps a [Client] and enables its functions to be used in WASM
#[wasm_bindgen(getter_with_clone)]
pub struct MithrilClient {
    client: Client,
    certificate_verifier_cache: Option<Arc<dyn CertificateVerifierCache>>,
    unstable: bool,
}

#[wasm_bindgen]
impl MithrilClient {
    /// Constructor for wasm client
    #[wasm_bindgen(constructor)]
    pub fn new(
        aggregator_endpoint: &str,
        genesis_verification_key: &str,
        options: JsValue,
    ) -> MithrilClient {
        let feedback_receiver = Arc::new(JSBroadcastChannelFeedbackReceiver::new("mithril-client"));

        let client_options = if options.is_undefined() {
            ClientOptions::default()
        } else {
            serde_wasm_bindgen::from_value(options)
                .map_err(|err| format!("Failed to parse options: {err:?}"))
                .unwrap()
        };

        let certificate_verifier_cache = if client_options.unstable
            && client_options.enable_certificate_chain_verification_cache
        {
            Self::build_certifier_cache(
                aggregator_endpoint,
                TimeDelta::seconds(
                    client_options.certificate_chain_verification_cache_duration_in_seconds as i64,
                ),
            )
        } else {
            None
        };

        let client = ClientBuilder::aggregator(aggregator_endpoint, genesis_verification_key)
            .add_feedback_receiver(feedback_receiver)
            .with_options(client_options.clone())
            .with_certificate_verifier_cache(certificate_verifier_cache.clone())
            .build()
            .map_err(|err| format!("{err:?}"))
            .unwrap();

        MithrilClient {
            client,
            certificate_verifier_cache,
            unstable: client_options.unstable,
        }
    }

    fn build_certifier_cache(
        aggregator_endpoint: &str,
        expiration_delay: TimeDelta,
    ) -> Option<Arc<dyn CertificateVerifierCache>> {
        if web_sys::window().is_none() {
            web_sys::console::warn_1(
                &"Can't enable certificate chain verification cache: window object is not available\
                    (are you running in a browser environment?)"
                    .into(),
            );
            return None;
        }

        web_sys::console::warn_1(
            &"Danger: the certificate chain verification cache is enabled.\n\
            This feature is highly experimental and insecure, and it must not be used in production."
                .into(),
        );

        Some(Arc::new(LocalStorageCertificateVerifierCache::new(
            aggregator_endpoint,
            expiration_delay,
        )))
    }

    /// Call the client to get a snapshot from a digest
    #[wasm_bindgen]
    pub async fn get_snapshot(&self, digest: &str) -> WasmResult {
        let result = self
            .client
            .snapshot()
            .get(digest)
            .await
            .map_err(|err| format!("{err:?}"))?
            .ok_or(JsValue::from_str(&format!(
                "No snapshot found for digest: '{digest}'"
            )))?;

        Ok(serde_wasm_bindgen::to_value(&result)?)
    }

    /// Call the client to get the list of available snapshots
    #[wasm_bindgen]
    pub async fn list_snapshots(&self) -> WasmResult {
        let result = self
            .client
            .snapshot()
            .list()
            .await
            .map_err(|err| format!("{err:?}"))?;

        Ok(serde_wasm_bindgen::to_value(&result)?)
    }

    /// Call the client to get a mithril stake distribution from a hash
    #[wasm_bindgen]
    pub async fn get_mithril_stake_distribution(&self, hash: &str) -> WasmResult {
        let result = self
            .client
            .mithril_stake_distribution()
            .get(hash)
            .await
            .map_err(|err| format!("{err:?}"))?
            .ok_or(JsValue::from_str(&format!(
                "No mithril stake distribution found for hash: '{hash}'"
            )))?;

        Ok(serde_wasm_bindgen::to_value(&result)?)
    }

    /// Call the client for the list of available mithril stake distributions
    #[wasm_bindgen]
    pub async fn list_mithril_stake_distributions(&self) -> WasmResult {
        let result = self
            .client
            .mithril_stake_distribution()
            .list()
            .await
            .map_err(|err| format!("{err:?}"))?;

        Ok(serde_wasm_bindgen::to_value(&result)?)
    }

    /// Call the client to compute a mithril stake distribution message
    #[wasm_bindgen]
    pub async fn compute_mithril_stake_distribution_message(
        &self,
        stake_distribution: JsValue,
        certificate: JsValue,
    ) -> WasmResult {
        let certificate: MithrilCertificate =
            serde_wasm_bindgen::from_value(certificate).map_err(|err| format!("{err:?}"))?;
        let stake_distribution =
            serde_wasm_bindgen::from_value(stake_distribution).map_err(|err| format!("{err:?}"))?;
        let result = MessageBuilder::new()
            .compute_mithril_stake_distribution_message(&certificate, &stake_distribution)
            .map_err(|err| format!("{err:?}"))?;

        Ok(serde_wasm_bindgen::to_value(&result)?)
    }

    /// Call the client to verify a mithril stake distribution message
    #[wasm_bindgen]
    pub async fn verify_message_match_certificate(
        &self,
        message: JsValue,
        certificate: JsValue,
    ) -> WasmResult {
        let certificate: MithrilCertificate =
            serde_wasm_bindgen::from_value(certificate).map_err(|err| format!("{err:?}"))?;
        let message = serde_wasm_bindgen::from_value(message).map_err(|err| format!("{err:?}"))?;

        Ok(JsValue::from(certificate.match_message(&message)))
    }

    /// Call the client to get a mithril certificate from a certificate hash
    #[wasm_bindgen]
    pub async fn get_mithril_certificate(&self, hash: &str) -> WasmResult {
        let result = self
            .client
            .certificate()
            .get(hash)
            .await
            .map_err(|err| format!("{err:?}"))?
            .ok_or(JsValue::from_str(&format!(
                "No certificate found for hash: '{hash}'"
            )))?;

        Ok(serde_wasm_bindgen::to_value(&result)?)
    }

    /// Call the client for the list of available mithril certificates
    #[wasm_bindgen]
    pub async fn list_mithril_certificates(&self) -> WasmResult {
        let result = self
            .client
            .certificate()
            .list()
            .await
            .map_err(|err| format!("{err:?}"))?;

        Ok(serde_wasm_bindgen::to_value(&result)?)
    }

    /// Call the client to verify the certificate chain from a certificate hash
    #[wasm_bindgen]
    pub async fn verify_certificate_chain(&self, hash: &str) -> WasmResult {
        let result = self
            .client
            .certificate()
            .verify_chain(hash)
            .await
            .map_err(|err| format!("{err:?}"))?;

        Ok(serde_wasm_bindgen::to_value(&result)?)
    }

    /// Call the client for the list of available Cardano transactions snapshots
    #[wasm_bindgen]
    pub async fn list_cardano_transactions_snapshots(&self) -> WasmResult {
        let result = self
            .client
            .cardano_transaction()
            .list_snapshots()
            .await
            .map_err(|err| format!("{err:?}"))?;

        Ok(serde_wasm_bindgen::to_value(&result)?)
    }

    /// Call the client to get a Cardano transactions snapshot from a hash
    #[wasm_bindgen]
    pub async fn get_cardano_transactions_snapshot(&self, hash: &str) -> WasmResult {
        let result = self
            .client
            .cardano_transaction()
            .get_snapshot(hash)
            .await
            .map_err(|err| format!("{err:?}"))?
            .ok_or(JsValue::from_str(&format!(
                "No cardano transactions snapshot found for hash: '{hash}'"
            )))?;

        Ok(serde_wasm_bindgen::to_value(&result)?)
    }

    /// Call the client to get a Cardano transactions proofs
    #[wasm_bindgen]
    pub async fn get_cardano_transaction_proofs(
        &self,
        ctx_hashes: Box<[JsValue]>,
    ) -> Result<CardanoTransactionsProofs, JsValue> {
        let hashes = ctx_hashes
            .iter()
            .map(|h| {
                h.as_string().ok_or(JsValue::from_str(&format!(
                    "All transaction hashes must be strings: '{h:?}'"
                )))
            })
            .collect::<Result<Vec<String>, JsValue>>()
            .map_err(|err| format!("{err:?}"))?;

        let result = self
            .client
            .cardano_transaction()
            .get_proofs(&hashes)
            .await
            .map_err(|err| format!("{err:?}"))?;

        Ok(result)
    }

    /// Call the client to verify a cardano transaction proof and compute a message
    #[wasm_bindgen]
    pub async fn verify_cardano_transaction_proof_then_compute_message(
        &self,
        cardano_transaction_proof: &CardanoTransactionsProofs,
        certificate: JsValue,
    ) -> WasmResult {
        let certificate: MithrilCertificate =
            serde_wasm_bindgen::from_value(certificate).map_err(|err| format!("{err:?}"))?;
        let verified_proof = cardano_transaction_proof
            .verify()
            .map_err(|err| format!("{err:?}"))?;
        let result = MessageBuilder::new()
            .compute_cardano_transactions_proofs_message(&certificate, &verified_proof);

        Ok(serde_wasm_bindgen::to_value(&result)?)
    }

    /// Call the client to get a cardano stake distribution from a hash
    #[wasm_bindgen]
    pub async fn get_cardano_stake_distribution(&self, hash: &str) -> WasmResult {
        let result = self
            .client
            .cardano_stake_distribution()
            .get(hash)
            .await
            .map_err(|err| format!("{err:?}"))?
            .ok_or(JsValue::from_str(&format!(
                "No cardano stake distribution found for hash: '{hash}'"
            )))?;

        Ok(serde_wasm_bindgen::to_value(&result)?)
    }

    /// Call the client to get a cardano stake distribution from an epoch
    /// The epoch represents the epoch at the end of which the Cardano stake distribution is computed by the Cardano node
    #[wasm_bindgen]
    pub async fn get_cardano_stake_distribution_by_epoch(&self, epoch: u64) -> WasmResult {
        let result = self
            .client
            .cardano_stake_distribution()
            .get_by_epoch(Epoch(epoch))
            .await
            .map_err(|err| format!("{err:?}"))?
            .ok_or(JsValue::from_str(&format!(
                "No cardano stake distribution found for epoch: '{epoch}'"
            )))?;

        Ok(serde_wasm_bindgen::to_value(&result)?)
    }

    /// Call the client for the list of available cardano stake distributions
    #[wasm_bindgen]
    pub async fn list_cardano_stake_distributions(&self) -> WasmResult {
        let result = self
            .client
            .cardano_stake_distribution()
            .list()
            .await
            .map_err(|err| format!("{err:?}"))?;

        Ok(serde_wasm_bindgen::to_value(&result)?)
    }

    /// Call the client to compute a cardano stake distribution message
    #[wasm_bindgen]
    pub async fn compute_cardano_stake_distribution_message(
        &self,
        certificate: JsValue,
        cardano_stake_distribution: JsValue,
    ) -> WasmResult {
        let certificate =
            serde_wasm_bindgen::from_value(certificate).map_err(|err| format!("{err:?}"))?;
        let cardano_stake_distribution = serde_wasm_bindgen::from_value(cardano_stake_distribution)
            .map_err(|err| format!("{err:?}"))?;
        let result = MessageBuilder::new()
            .compute_cardano_stake_distribution_message(&certificate, &cardano_stake_distribution)
            .map_err(|err| format!("{err:?}"))?;

        Ok(serde_wasm_bindgen::to_value(&result)?)
    }

    /// `unstable` Reset the certificate verifier cache if enabled
    #[wasm_bindgen]
    pub async fn reset_certificate_verifier_cache(&self) -> Result<(), JsValue> {
        self.guard_unstable()?;

        if let Some(cache) = self.certificate_verifier_cache.as_ref() {
            cache.reset().await.map_err(|err| format!("{err:?}"))?;
        }

        Ok(())
    }
}

// Unstable functions are only available when the unstable flag is set
#[wasm_bindgen]
impl MithrilClient {
    fn guard_unstable(&self) -> Result<(), wasm_bindgen::JsValue> {
        if !self.unstable {
            return Err(JsValue::from_str("Unstable functions are not enabled. Set the 'unstable' client option field to 'true' to enable them."));
        }

        Ok(())
    }

    /// Call the client to get a cardano database snapshot from a hash
    ///
    /// Warning: this function is unstable and may be modified in the future
    #[wasm_bindgen]
    pub async fn get_cardano_database_v2(&self, hash: &str) -> WasmResult {
        self.guard_unstable()?;

        let result = self
            .client
            .cardano_database()
            .get(hash)
            .await
            .map_err(|err| format!("{err:?}"))?
            .ok_or(JsValue::from_str(&format!(
                "No cardano database snapshot found for hash: '{hash}'"
            )))?;

        Ok(serde_wasm_bindgen::to_value(&result)?)
    }

    /// Call the client for the list of available cardano database snapshots
    ///
    /// Warning: this function is unstable and may be modified in the future
    #[wasm_bindgen]
    pub async fn list_cardano_database_v2(&self) -> WasmResult {
        self.guard_unstable()?;

        let result = self
            .client
            .cardano_database()
            .list()
            .await
            .map_err(|err| format!("{err:?}"))?;

        Ok(serde_wasm_bindgen::to_value(&result)?)
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;
    use wasm_bindgen_test::*;

    use mithril_client::{
        common::ProtocolMessage, CardanoDatabaseSnapshot, CardanoDatabaseSnapshotListItem,
        CardanoStakeDistribution, CardanoStakeDistributionListItem, CardanoTransactionSnapshot,
        MithrilCertificateListItem, MithrilStakeDistribution, MithrilStakeDistributionListItem,
        Snapshot, SnapshotListItem,
    };

    use crate::test_data;

    use super::*;

    const GENESIS_VERIFICATION_KEY: &str = "5b33322c3235332c3138362c3230312c3137372c31312c3131372c3133352c3138372c3136372c3138312c3138382c32322c35392c3230362c3130352c3233312c3135302c3231352c33302c37382c3231322c37362c31362c3235322c3138302c37322c3133342c3133372c3234372c3136312c36385d";
    const FAKE_AGGREGATOR_IP: &str = "127.0.0.1";
    const FAKE_AGGREGATOR_PORT: &str = "8000";

    fn get_mithril_client(options: ClientOptions) -> MithrilClient {
        let options_js_value = serde_wasm_bindgen::to_value(&options).unwrap();
        MithrilClient::new(
            &format!(
                "http://{}:{}/aggregator",
                FAKE_AGGREGATOR_IP, FAKE_AGGREGATOR_PORT
            ),
            GENESIS_VERIFICATION_KEY,
            options_js_value,
        )
    }

    fn get_mithril_client_stable() -> MithrilClient {
        let options = ClientOptions::new(None).with_unstable_features(false);
        get_mithril_client(options)
    }

    fn get_mithril_client_unstable() -> MithrilClient {
        let options = ClientOptions::new(None).with_unstable_features(true);
        get_mithril_client(options)
    }

    #[cfg(not(feature = "test-node"))]
    wasm_bindgen_test_configure!(run_in_browser);

    #[wasm_bindgen_test]
    fn build_mithril_client_with_custom_http_header() {
        let mut http_headers = HashMap::new();
        http_headers.insert("Authorization".to_string(), "Bearer token".to_string());
        let options = ClientOptions::new(Some(http_headers));
        let options_js_value = serde_wasm_bindgen::to_value(&options).unwrap();

        MithrilClient::new(
            &format!(
                "http://{}:{}/aggregator",
                FAKE_AGGREGATOR_IP, FAKE_AGGREGATOR_PORT
            ),
            GENESIS_VERIFICATION_KEY,
            options_js_value,
        );
    }

    #[wasm_bindgen_test]
    fn build_mithril_client_with_undefined_options() {
        MithrilClient::new(
            &format!(
                "http://{}:{}/aggregator",
                FAKE_AGGREGATOR_IP, FAKE_AGGREGATOR_PORT
            ),
            GENESIS_VERIFICATION_KEY,
            JsValue::undefined(),
        );
    }

    #[wasm_bindgen_test]
    #[should_panic(expected = "Failed to parse options")]
    fn build_mithril_client_with_unparsable_options() {
        let invalid_js_value = JsValue::from_str("invalid");
        MithrilClient::new(
            &format!(
                "http://{}:{}/aggregator",
                FAKE_AGGREGATOR_IP, FAKE_AGGREGATOR_PORT
            ),
            GENESIS_VERIFICATION_KEY,
            invalid_js_value,
        );
    }

    #[wasm_bindgen_test]
    async fn list_snapshots_should_return_value_convertible_in_rust_type() {
        let snapshots_list_js_value = get_mithril_client_stable()
            .list_snapshots()
            .await
            .expect("list_snapshots should not fail");
        let snapshots_list =
            serde_wasm_bindgen::from_value::<Vec<SnapshotListItem>>(snapshots_list_js_value)
                .expect("conversion should not fail");

        assert_eq!(
            snapshots_list.len(),
            // Aggregator return up to 20 items for a list route
            test_data::snapshot_digests().len().min(20)
        );
    }

    #[wasm_bindgen_test]
    async fn get_snapshot_should_return_value_convertible_in_rust_type() {
        let snapshot_js_value = get_mithril_client_stable()
            .get_snapshot(test_data::snapshot_digests()[0])
            .await
            .expect("get_snapshot should not fail");
        let snapshot = serde_wasm_bindgen::from_value::<Snapshot>(snapshot_js_value)
            .expect("conversion should not fail");

        assert_eq!(snapshot.digest, test_data::snapshot_digests()[0]);
    }

    #[wasm_bindgen_test]
    async fn get_snapshot_should_fail_with_unknown_digest() {
        get_mithril_client_stable()
            .get_snapshot("whatever")
            .await
            .expect_err("get_snapshot should fail");
    }

    #[wasm_bindgen_test]
    async fn list_mithril_stake_distributions_should_return_value_convertible_in_rust_type() {
        let msd_list_js_value = get_mithril_client_stable()
            .list_mithril_stake_distributions()
            .await
            .expect("list_mithril_stake_distributions should not fail");
        let msd_list = serde_wasm_bindgen::from_value::<Vec<MithrilStakeDistributionListItem>>(
            msd_list_js_value,
        )
        .expect("conversion should not fail");

        assert_eq!(
            msd_list.len(),
            // Aggregator return up to 20 items for a list route
            test_data::msd_hashes().len().min(20)
        );
    }

    #[wasm_bindgen_test]
    async fn get_mithril_stake_distribution_should_return_value_convertible_in_rust_type() {
        let msd_js_value = get_mithril_client_stable()
            .get_mithril_stake_distribution(test_data::msd_hashes()[0])
            .await
            .expect("get_mithril_stake_distribution should not fail");
        let msd = serde_wasm_bindgen::from_value::<MithrilStakeDistribution>(msd_js_value)
            .expect("conversion should not fail");

        assert_eq!(msd.hash, test_data::msd_hashes()[0]);
    }

    #[wasm_bindgen_test]
    async fn get_mithril_stake_distribution_should_fail_with_unknown_hash() {
        get_mithril_client_stable()
            .get_mithril_stake_distribution("whatever")
            .await
            .expect_err("get_mithril_stake_distribution should fail");
    }

    #[wasm_bindgen_test]
    async fn list_mithril_certificates_should_return_value_convertible_in_rust_type() {
        let certificates_list_js_value = get_mithril_client_stable()
            .list_mithril_certificates()
            .await
            .expect("list_mithril_certificates should not fail");
        let certificates_list = serde_wasm_bindgen::from_value::<Vec<MithrilCertificateListItem>>(
            certificates_list_js_value,
        )
        .expect("conversion should not fail");

        assert_eq!(
            certificates_list.len(),
            // Aggregator return up to 20 items for a list route
            test_data::certificate_hashes().len().min(20)
        );
    }

    #[wasm_bindgen_test]
    async fn get_mithril_certificate_should_return_value_convertible_in_rust_type() {
        let certificate_js_value = get_mithril_client_stable()
            .get_mithril_certificate(test_data::certificate_hashes()[0])
            .await
            .expect("get_mithril_certificate should not fail");
        let certificate =
            serde_wasm_bindgen::from_value::<MithrilCertificate>(certificate_js_value)
                .expect("conversion should not fail");

        assert_eq!(certificate.hash, test_data::certificate_hashes()[0]);
    }

    #[wasm_bindgen_test]
    async fn get_mithril_certificate_should_fail_with_unknown_hash() {
        get_mithril_client_stable()
            .get_mithril_certificate("whatever")
            .await
            .expect_err("get_mithril_certificate should fail");
    }

    #[wasm_bindgen_test]
    async fn compute_mithril_stake_distribution_message_should_return_value_convertible_in_rust_type(
    ) {
        let client = get_mithril_client_stable();
        let msd_js_value = client
            .get_mithril_stake_distribution(test_data::msd_hashes()[0])
            .await
            .unwrap();
        let msd = serde_wasm_bindgen::from_value::<MithrilStakeDistribution>(msd_js_value.clone())
            .unwrap();
        let certificate_js_value = client
            .get_mithril_certificate(&msd.certificate_hash)
            .await
            .unwrap();

        let message_js_value = client
            .compute_mithril_stake_distribution_message(msd_js_value, certificate_js_value)
            .await
            .expect("compute_mithril_stake_distribution_message should not fail");
        serde_wasm_bindgen::from_value::<ProtocolMessage>(message_js_value)
            .expect("conversion should not fail");
    }

    #[wasm_bindgen_test]
    async fn verify_certificate_chain_should_return_value_convertible_in_rust_type() {
        let client = get_mithril_client_stable();
        let msd_js_value = client
            .get_mithril_stake_distribution(test_data::msd_hashes()[0])
            .await
            .unwrap();
        let msd = serde_wasm_bindgen::from_value::<MithrilStakeDistribution>(msd_js_value).unwrap();

        let certificate_js_value = client
            .verify_certificate_chain(&msd.certificate_hash)
            .await
            .expect("verify_certificate_chain should not fail");
        serde_wasm_bindgen::from_value::<MithrilCertificate>(certificate_js_value)
            .expect("conversion should not fail");
    }

    #[wasm_bindgen_test]
    async fn verify_message_match_certificate_should_return_true() {
        let client = get_mithril_client_stable();
        let msd_js_value = client
            .get_mithril_stake_distribution(test_data::msd_hashes()[0])
            .await
            .unwrap();
        let msd = serde_wasm_bindgen::from_value::<MithrilStakeDistribution>(msd_js_value.clone())
            .unwrap();
        let last_certificate_js_value = client
            .verify_certificate_chain(&msd.certificate_hash)
            .await
            .unwrap();
        let message_js_value = client
            .compute_mithril_stake_distribution_message(
                msd_js_value,
                last_certificate_js_value.clone(),
            )
            .await
            .unwrap();

        client
            .verify_message_match_certificate(message_js_value, last_certificate_js_value)
            .await
            .expect("verify_message_match_certificate should not fail");
    }

    #[wasm_bindgen_test]
    async fn list_cardano_transactions_snapshots_should_return_value_convertible_in_rust_type() {
        let cardano_tx_sets_js_value = get_mithril_client_stable()
            .list_cardano_transactions_snapshots()
            .await
            .expect("list_cardano_transactions_snapshots should not fail");
        let cardano_tx_sets = serde_wasm_bindgen::from_value::<Vec<CardanoTransactionSnapshot>>(
            cardano_tx_sets_js_value,
        )
        .expect("conversion should not fail");

        assert_eq!(
            cardano_tx_sets.len(),
            // Aggregator return up to 20 items for a list route
            test_data::ctx_snapshot_hashes().len().min(20)
        );
    }

    #[wasm_bindgen_test]
    async fn get_cardano_transactions_snapshot_should_return_value_convertible_in_rust_type() {
        let cardano_tx_set_js_value = get_mithril_client_stable()
            .get_cardano_transactions_snapshot(test_data::ctx_snapshot_hashes()[0])
            .await
            .expect("get_cardano_transactions_snapshot should not fail");
        let cardano_tx_set =
            serde_wasm_bindgen::from_value::<CardanoTransactionSnapshot>(cardano_tx_set_js_value)
                .expect("conversion should not fail");

        assert_eq!(cardano_tx_set.hash, test_data::ctx_snapshot_hashes()[0]);
    }

    #[wasm_bindgen_test]
    async fn get_cardano_transactions_snapshot_should_fail_with_unknown_digest() {
        get_mithril_client_stable()
            .get_cardano_transactions_snapshot("whatever")
            .await
            .expect_err("get_cardano_transactions_snapshot should fail");
    }

    #[wasm_bindgen_test]
    async fn get_cardano_transaction_proofs_should_return_value_convertible_in_rust_type() {
        let tx_hash = test_data::proof_transaction_hashes()[0];
        let ctx_hashes = Box::new([JsValue::from(tx_hash)]);
        let client = get_mithril_client_stable();

        let tx_proof = client
            .get_cardano_transaction_proofs(ctx_hashes)
            .await
            .expect("get_verified_cardano_transaction_proofs should not fail");
        let certificate = client
            .get_mithril_certificate(&tx_proof.certificate_hash)
            .await
            .unwrap();

        client
            .verify_cardano_transaction_proof_then_compute_message(&tx_proof, certificate)
            .await
            .expect("Compute tx proof message for matching cert failed");
    }

    #[wasm_bindgen_test]
    async fn list_cardano_stake_distributions_should_return_value_convertible_in_rust_type() {
        let csd_list_js_value = get_mithril_client_stable()
            .list_cardano_stake_distributions()
            .await
            .expect("cardano_mithril_stake_distributions should not fail");
        let csd_list = serde_wasm_bindgen::from_value::<Vec<CardanoStakeDistributionListItem>>(
            csd_list_js_value,
        )
        .expect("conversion should not fail");

        assert_eq!(
            csd_list.len(),
            // Aggregator return up to 20 items for a list route
            test_data::csd_hashes().len().min(20)
        );
    }

    #[wasm_bindgen_test]
    async fn get_cardano_stake_distribution_should_return_value_convertible_in_rust_type() {
        let csd_js_value = get_mithril_client_stable()
            .get_cardano_stake_distribution(test_data::csd_hashes()[0])
            .await
            .expect("get_cardano_stake_distribution should not fail");
        let csd = serde_wasm_bindgen::from_value::<CardanoStakeDistribution>(csd_js_value)
            .expect("conversion should not fail");

        assert_eq!(csd.hash, test_data::csd_hashes()[0]);
    }

    #[wasm_bindgen_test]
    async fn get_cardano_stake_distribution_should_fail_with_unknown_hash() {
        get_mithril_client_stable()
            .get_cardano_stake_distribution("whatever")
            .await
            .expect_err("get_cardano_stake_distribution should fail");
    }

    #[wasm_bindgen_test]
    async fn get_cardano_stake_distribution_by_epoch_should_return_value_convertible_in_rust_type()
    {
        let epoch: u64 = test_data::csd_epochs()[0].parse().unwrap();
        let csd_js_value = get_mithril_client_stable()
            .get_cardano_stake_distribution_by_epoch(epoch)
            .await
            .expect("get_cardano_stake_distribution_by_epoch should not fail");

        let csd = serde_wasm_bindgen::from_value::<CardanoStakeDistribution>(csd_js_value)
            .expect("conversion should not fail");

        assert_eq!(csd.epoch, epoch);
    }

    #[wasm_bindgen_test]
    async fn get_cardano_stake_distribution_by_epoch_should_fail_with_unknown_epoch() {
        get_mithril_client_stable()
            .get_cardano_stake_distribution_by_epoch(u64::MAX)
            .await
            .expect_err("get_cardano_stake_distribution_by_epoch should fail");
    }

    #[wasm_bindgen_test]
    async fn compute_cardano_stake_distribution_message_should_return_value_convertible_in_rust_type(
    ) {
        let client = get_mithril_client_stable();
        let csd_js_value = client
            .get_cardano_stake_distribution(test_data::csd_hashes()[0])
            .await
            .unwrap();
        let csd = serde_wasm_bindgen::from_value::<CardanoStakeDistribution>(csd_js_value.clone())
            .unwrap();
        let certificate_js_value = client
            .get_mithril_certificate(&csd.certificate_hash)
            .await
            .unwrap();

        let message_js_value = client
            .compute_cardano_stake_distribution_message(certificate_js_value, csd_js_value)
            .await
            .expect("compute_cardano_stake_distribution_message should not fail");
        serde_wasm_bindgen::from_value::<ProtocolMessage>(message_js_value)
            .expect("conversion should not fail");
    }

    #[wasm_bindgen_test]
    async fn list_cardano_database_v2_should_return_value_convertible_in_rust_type() {
        let cdb_list_js_value = get_mithril_client_unstable()
            .list_cardano_database_v2()
            .await
            .expect("list_cardano_database_v2 should not fail");
        let cdb_list = serde_wasm_bindgen::from_value::<Vec<CardanoDatabaseSnapshotListItem>>(
            cdb_list_js_value,
        )
        .expect("conversion should not fail");

        assert_eq!(
            cdb_list.len(),
            // Aggregator return up to 20 items for a list route
            test_data::cdb_hashes().len().min(20)
        );
    }

    #[wasm_bindgen_test]
    async fn get_cardano_database_v2_should_return_value_convertible_in_rust_type() {
        let cdb_js_value = get_mithril_client_unstable()
            .get_cardano_database_v2(test_data::cdb_hashes()[0])
            .await
            .expect("get_cardano_database_v2 should not fail");
        let csd = serde_wasm_bindgen::from_value::<CardanoDatabaseSnapshot>(cdb_js_value)
            .expect("conversion should not fail");

        assert_eq!(csd.hash, test_data::cdb_hashes()[0]);
    }

    #[wasm_bindgen_test]
    async fn get_cardano_database_v2_should_fail_with_unknown_hash() {
        get_mithril_client_unstable()
            .get_cardano_database_v2("whatever")
            .await
            .expect_err("get_cardano_database_v2 should fail");
    }
}
