/* #![cfg(target_family = "wasm")]
use mithril_client::ClientBuilder;
use mithril_client_wasm::MithrilClient;
use wasm_bindgen_test::*;

#[wasm_bindgen_test]
async fn certificate_get_list() {
    let cb = ClientBuilder::new("test");
    let client = MithrilClient::new(
        "https://aggregator.testing-preview.api.mithril.network/aggregator",
        "5b3132372c37332c3132342c3136312c362c3133372c3133312c3231332c3230372c3131372c3139382c38352c3137362c3139392c3136322c3234312c36382c3132332c3131392c3134352c31332c3233322c3234332c34392c3232392c322c3234392c3230352c3230352c33392c3233352c34345d",
    ).build()
    .unwrap();

    // let list = client.certificate().list().await.unwrap();
    // assert!(list.len() > 0);
}
 */
