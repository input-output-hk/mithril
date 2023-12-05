#![cfg(target_family = "wasm")]
use super::{Client, ClientBuilder, MessageBuilder};
use mithril_common::messages::CertificateMessage;
use wasm_bindgen::prelude::*;

type WasmResult = Result<JsValue, JsValue>;

#[cfg_attr(target_family = "wasm", wasm_bindgen(js_name = Client))]
struct ClientWasm {
    client: Client,
}

#[wasm_bindgen(js_class = Client)]
impl ClientWasm {
    /// Constructor for wasm client
    #[wasm_bindgen(constructor)]
    pub async fn new(aggregator_endpoint: &str, genesis_verification_key: &str) -> ClientWasm {
        let client = ClientBuilder::aggregator(aggregator_endpoint, genesis_verification_key)
            .build()
            .map_err(|err| format!("{err:?}"))
            .unwrap();
        ClientWasm { client }
    }

    /// Call the client to get a snapshot from a digest
    #[wasm_bindgen(js_name = get_snapshot)]
    pub async fn get_snapshot(&self, digest: &str) -> WasmResult {
        let result = self
            .client
            .snapshot()
            .get(digest)
            .await
            .map_err(|err| format!("{err:?}"))?;

        Ok(serde_wasm_bindgen::to_value(&result)?)
    }

    /// Call the client to get the list of available snapshots
    #[wasm_bindgen(js_name = list_snapshots)]
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
    #[wasm_bindgen(js_name = get_mithril_stake_distribution)]
    pub async fn get_mithril_stake_distribution(&self, hash: &str) -> WasmResult {
        let result = self
            .client
            .mithril_stake_distribution()
            .get(hash)
            .await
            .map_err(|err| format!("{err:?}"))?;

        Ok(serde_wasm_bindgen::to_value(&result)?)
    }

    /// Call the client for the list of available mithril stake distributions
    #[wasm_bindgen(js_name = list_mithril_stake_distributions)]
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
    #[wasm_bindgen(js_name = compute_mithril_stake_distribution_message)]
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

    ///
    #[wasm_bindgen(js_name = verify_message_match_certificate)]
    pub async fn verify_message_match_certificate(
        &self,
        message: JsValue,
        certificate: JsValue,
    ) -> WasmResult {
        let certificate: CertificateMessage =
            serde_wasm_bindgen::from_value(certificate).map_err(|err| format!("{err:?}"))?;
        let message = serde_wasm_bindgen::from_value(message).map_err(|err| format!("{err:?}"))?;

        Ok(JsValue::from(certificate.match_message(&message)))
    }

    /// Call the client to get a mithril certificate from a certificate hash
    #[wasm_bindgen(js_name = get_mithril_certificate)]
    pub async fn get_mithril_certificate(&self, hash: &str) -> WasmResult {
        let result = self
            .client
            .certificate()
            .get(hash)
            .await
            .map_err(|err| format!("{err:?}"))?;

        Ok(serde_wasm_bindgen::to_value(&result)?)
    }

    /// Call the client for the list of available mithril certificates
    #[wasm_bindgen(js_name = list_mithril_certificates)]
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
    #[wasm_bindgen(js_name = verify_certificate_chain)]
    pub async fn verify_certificate_chain(&self, hash: &str) -> WasmResult {
        let result = self
            .client
            .certificate()
            .verify_chain(hash)
            .await
            .map_err(|err| format!("{err:?}"))?;

        Ok(serde_wasm_bindgen::to_value(&result)?)
    }
}
