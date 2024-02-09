use async_trait::async_trait;
use mithril_client::{
    feedback::{FeedbackReceiver, MithrilEvent},
    Client, ClientBuilder, MessageBuilder, MithrilCertificate,
};
use serde::Serialize;
use std::sync::Arc;
use tokio_util::sync::CancellationToken;
use wasm_bindgen::prelude::*;
use web_sys::AbortSignal;

type WasmResult = Result<JsValue, JsValue>;

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
        let _ = web_sys::BroadcastChannel::new(&self.channel)
            .unwrap()
            .post_message(&serde_wasm_bindgen::to_value(&event).unwrap());
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
#[wasm_bindgen]
pub struct MithrilClient {
    client: Client,
}

#[wasm_bindgen]
impl MithrilClient {
    /// Constructor for wasm client
    #[wasm_bindgen(constructor)]
    pub fn new(aggregator_endpoint: &str, genesis_verification_key: &str) -> MithrilClient {
        let feedback_receiver = Arc::new(JSBroadcastChannelFeedbackReceiver::new("mithril-client"));
        let client = ClientBuilder::aggregator(aggregator_endpoint, genesis_verification_key)
            .add_feedback_receiver(feedback_receiver)
            .build()
            .map_err(|err| format!("{err:?}"))
            .unwrap();
        MithrilClient { client }
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
    ) -> WasmResult {
        let stake_distribution =
            serde_wasm_bindgen::from_value(stake_distribution).map_err(|err| format!("{err:?}"))?;
        let result = MessageBuilder::new()
            .compute_mithril_stake_distribution_message(&stake_distribution)
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
    pub async fn verify_certificate_chain(&self, hash: &str, signal: AbortSignal) -> WasmResult {
        let certificate_client = self.client.certificate().clone();
        let hash = hash.to_string();
        let token = CancellationToken::new();
        let cloned_token = token.clone();

        let join_handle = tokio::task::spawn_local(async move {
            tokio::select! {
                err = cloned_token.cancelled() => {
                    Err(format!("Aborted, error: {err:?}").into())
                }
                res = tokio::spawn(async move { certificate_client.verify_chain(&hash).await }) => {
                    let result = res
                        .map_err(|err| format!("{err:?}"))?
                        .map_err(|err| format!("{err:?}"))?;

                    Ok(serde_wasm_bindgen::to_value(&result)?)
                }
            }
        });

        // Signal may have already been aborted
        if signal.aborted() {
            token.cancel();
        }

        let abort_closuse = Closure::<dyn Fn()>::new(move || token.cancel());
        signal.add_event_listener_with_callback("abort", abort_closuse.as_ref().unchecked_ref())?;

        match join_handle.await {
            Ok(val) => val,
            Err(err) => Err(format!("Aborted, error: {err}").into()),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use mithril_client::{
        common::ProtocolMessage, MithrilCertificateListItem, MithrilStakeDistribution,
        MithrilStakeDistributionListItem, Snapshot, SnapshotListItem,
    };
    use wasm_bindgen_test::*;
    use web_sys::AbortController;

    const GENESIS_VERIFICATION_KEY: &str = "5b33322c3235332c3138362c3230312c3137372c31312c3131372c3133352c3138372c3136372c3138312c3138382c32322c35392c3230362c3130352c3233312c3135302c3231352c33302c37382c3231322c37362c31362c3235322c3138302c37322c3133342c3133372c3234372c3136312c36385d";
    const FAKE_AGGREGATOR_IP: &str = "127.0.0.1";
    const FAKE_AGGREGATOR_PORT: &str = "8000";
    const FAKE_AGGREGATOR_SNAPSHOT_DIGEST: &str =
        "000ee4c84c7b64a62dc30ec78a765a1f3bb81cd9dd4bd1eccf9f2da785e70877";
    const FAKE_AGGREGATOR_MSD_HASH: &str =
        "03ebb00e6626037f2e58eb7cc50d308fd57c253baa1fe2b04eb5945ced16b5bd";
    const FAKE_CERTIFICATE_HASH: &str =
        "05bf6740e781e649dd2fe7e3319818747d8038ca759c67711c90cf24cdade8a9";

    fn get_mithril_client() -> MithrilClient {
        MithrilClient::new(
            &format!(
                "http://{}:{}/aggregator",
                FAKE_AGGREGATOR_IP, FAKE_AGGREGATOR_PORT
            ),
            GENESIS_VERIFICATION_KEY,
        )
    }

    wasm_bindgen_test_configure!(run_in_browser);
    #[wasm_bindgen_test]
    async fn list_snapshots_should_return_value_convertible_in_rust_type() {
        let snapshots_list_js_value = get_mithril_client()
            .list_snapshots()
            .await
            .expect("list_snapshots should not fail");
        let snapshots_list =
            serde_wasm_bindgen::from_value::<Vec<SnapshotListItem>>(snapshots_list_js_value)
                .expect("conversion should not fail");

        assert_eq!(snapshots_list.len(), 3);
    }

    #[wasm_bindgen_test]
    async fn get_snapshot_should_return_value_convertible_in_rust_type() {
        let snapshot_js_value = get_mithril_client()
            .get_snapshot(FAKE_AGGREGATOR_SNAPSHOT_DIGEST)
            .await
            .expect("get_snapshot should not fail");
        let snapshot = serde_wasm_bindgen::from_value::<Snapshot>(snapshot_js_value)
            .expect("conversion should not fail");

        assert_eq!(snapshot.digest, FAKE_AGGREGATOR_SNAPSHOT_DIGEST);
    }

    #[wasm_bindgen_test]
    async fn get_snapshot_should_fail_with_unknown_digest() {
        get_mithril_client()
            .get_snapshot("whatever")
            .await
            .expect_err("get_snapshot should fail");
    }

    #[wasm_bindgen_test]
    async fn list_mithril_stake_distributions_should_return_value_convertible_in_rust_type() {
        let msd_list_js_value = get_mithril_client()
            .list_mithril_stake_distributions()
            .await
            .expect("list_mithril_stake_distributions should not fail");
        let msd_list = serde_wasm_bindgen::from_value::<Vec<MithrilStakeDistributionListItem>>(
            msd_list_js_value,
        )
        .expect("conversion should not fail");

        assert_eq!(msd_list.len(), 3);
    }

    #[wasm_bindgen_test]
    async fn get_mithril_stake_distribution_should_return_value_convertible_in_rust_type() {
        let msd_js_value = get_mithril_client()
            .get_mithril_stake_distribution(FAKE_AGGREGATOR_MSD_HASH)
            .await
            .expect("get_mithril_stake_distribution should not fail");
        let msd = serde_wasm_bindgen::from_value::<MithrilStakeDistribution>(msd_js_value)
            .expect("conversion should not fail");

        assert_eq!(msd.hash, FAKE_AGGREGATOR_MSD_HASH);
    }

    #[wasm_bindgen_test]
    async fn get_mithril_stake_distribution_should_fail_with_unknown_hash() {
        get_mithril_client()
            .get_mithril_stake_distribution("whatever")
            .await
            .expect_err("get_mithril_stake_distribution should fail");
    }

    #[wasm_bindgen_test]
    async fn list_mithril_certificates_should_return_value_convertible_in_rust_type() {
        let certificates_list_js_value = get_mithril_client()
            .list_mithril_certificates()
            .await
            .expect("list_mithril_certificates should not fail");
        let certificates_list = serde_wasm_bindgen::from_value::<Vec<MithrilCertificateListItem>>(
            certificates_list_js_value,
        )
        .expect("conversion should not fail");

        assert_eq!(certificates_list.len(), 7);
    }

    #[wasm_bindgen_test]
    async fn get_mithril_certificate_should_return_value_convertible_in_rust_type() {
        let certificate_js_value = get_mithril_client()
            .get_mithril_certificate(FAKE_CERTIFICATE_HASH)
            .await
            .expect("get_mithril_certificate should not fail");
        let certificate =
            serde_wasm_bindgen::from_value::<MithrilCertificate>(certificate_js_value)
                .expect("conversion should not fail");

        assert_eq!(certificate.hash, FAKE_CERTIFICATE_HASH);
    }

    #[wasm_bindgen_test]
    async fn get_mithril_certificate_should_fail_with_unknown_hash() {
        get_mithril_client()
            .get_mithril_certificate("whatever")
            .await
            .expect_err("get_mithril_certificate should fail");
    }

    #[wasm_bindgen_test]
    async fn compute_mithril_stake_distribution_message_should_return_value_convertible_in_rust_type(
    ) {
        let client = get_mithril_client();
        let msd_js_value = client
            .get_mithril_stake_distribution(FAKE_AGGREGATOR_MSD_HASH)
            .await
            .unwrap();

        let message_js_value = client
            .compute_mithril_stake_distribution_message(msd_js_value)
            .await
            .expect("compute_mithril_stake_distribution_message should not fail");
        serde_wasm_bindgen::from_value::<ProtocolMessage>(message_js_value)
            .expect("conversion should not fail");
    }

    #[wasm_bindgen_test]
    async fn verify_certificate_chain_should_return_value_convertible_in_rust_type() {
        let client = get_mithril_client();
        let msd_js_value = client
            .get_mithril_stake_distribution(FAKE_AGGREGATOR_MSD_HASH)
            .await
            .unwrap();
        let msd = serde_wasm_bindgen::from_value::<MithrilStakeDistribution>(msd_js_value).unwrap();

        let controller = AbortController::new().unwrap();
        let certificate_js_value = client
            .verify_certificate_chain(&msd.certificate_hash, controller.signal())
            .await
            .expect("verify_certificate_chain should not fail");
        serde_wasm_bindgen::from_value::<MithrilCertificate>(certificate_js_value)
            .expect("conversion should not fail");
    }

    #[wasm_bindgen_test]
    async fn verify_message_match_certificate_should_return_true() {
        let client = get_mithril_client();
        let msd_js_value = client
            .get_mithril_stake_distribution(FAKE_AGGREGATOR_MSD_HASH)
            .await
            .unwrap();
        let msd = serde_wasm_bindgen::from_value::<MithrilStakeDistribution>(msd_js_value.clone())
            .unwrap();
        let controller = AbortController::new().unwrap();
        let last_certificate_js_value = client
            .verify_certificate_chain(&msd.certificate_hash, controller.signal())
            .await
            .unwrap();
        let message_js_value = client
            .compute_mithril_stake_distribution_message(msd_js_value)
            .await
            .unwrap();

        client
            .verify_message_match_certificate(message_js_value, last_certificate_js_value)
            .await
            .expect("verify_message_match_certificate should not fail");
    }

    #[wasm_bindgen_test]
    async fn verify_certificate_chain_should_be_aborted() {
        let client = get_mithril_client();
        let msd_js_value = client
            .get_mithril_stake_distribution(FAKE_AGGREGATOR_MSD_HASH)
            .await
            .unwrap();
        let msd = serde_wasm_bindgen::from_value::<MithrilStakeDistribution>(msd_js_value).unwrap();

        let controller = AbortController::new().unwrap();

        tokio::join!(
            async {
                client
                    .verify_certificate_chain(&msd.certificate_hash, controller.signal())
                    .await
                    .expect_err("verify_certificate_chain should have an 'abort' error");
            },
            async {
                controller.abort();
            }
        );
    }
}
