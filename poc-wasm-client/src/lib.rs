// mod entities;
mod utils;

use std::convert::{TryFrom, TryInto};

use blake2::{digest::typenum::U32, Blake2b};
use hex::FromHex;
use mithril_stm::stm::{StmAggrSig, StmAggrVerificationKey, StmParameters, StmSig};
use serde::de::DeserializeOwned;
use wasm_bindgen::prelude::*;
use wasm_bindgen_futures::JsFuture;
use web_sys::{Request, RequestInit, RequestMode, Response};

pub(crate) type D = Blake2b<U32>;

pub const GENESIS_VERIFICATION_KEY: &str = "5b3132372c37332c3132342c3136312c362c3133372c3133312c3231332c3230372c3131372c3139382c38352c3137362c3139392c3136322c3234312c36382c3132332c3131392c3134352c31332c3233322c3234332c34392c3232392c322c3234392c3230352c3230352c33392c3233352c34345d";
pub const PROTOCOL_PARAMETERS: StmParameters = StmParameters {
    m: 100,
    k: 5,
    phi_f: 0.65,
};
pub const AGGREGATOR_URL: &str =
    "https://aggregator.release-preprod.api.mithril.network/aggregator";

#[wasm_bindgen]
extern "C" {
    fn alert(s: &str);

    #[wasm_bindgen(js_namespace = console)]
    fn log(s: &str);
}

macro_rules! console_log {
    // Note that this is using the `log` function imported above during
    // `bare_bones`
    ($($t:tt)*) => (log(&format_args!($($t)*).to_string()))
}
pub fn key_decode_hex<T>(from: &str) -> Result<T, String>
where
    T: DeserializeOwned,
{
    let from_vec = Vec::from_hex(from).map_err(|e| {
        format!(
            "Key decode hex: can not turn hexadecimal value '{from}' into bytes, ERROR = '{e}'."
        )
    })?;

    serde_json::from_slice(from_vec.as_slice()).map_err(|e| {
        format!(
            "Key decode hex: can not deserialize to type '{}' from binary JSON, ERROR = '{e}'",
            std::any::type_name::<T>()
        )
    })
}

#[derive(Debug)]
pub enum CertificateSignature {
    MultiSignature(StmAggrSig<D>),
    GenesisSignature(ed25519_dalek::Signature),
}

impl CertificateSignature {
    pub fn verify(
        &self,
        message: &[u8],
        avk: &StmAggrVerificationKey<D>,
        protocol_parameters: &StmParameters,
    ) -> Result<(), String> {
        match self {
            Self::GenesisSignature(signature) => {
                let vkey: ed25519_dalek::VerifyingKey = key_decode_hex(GENESIS_VERIFICATION_KEY)?;
                vkey.verify_strict(message, signature)
                    .map_err(|e| format!("ERROR GENESIS={e}"))
            }
            Self::MultiSignature(signature) => signature
                .verify(message, avk, protocol_parameters)
                .map_err(|e| format!("ERROR MULTISIG={e}")),
        }
    }
}

#[derive(Debug)]
pub struct CertificateMessage {
    hash: String,
    previous_hash: String,
    multi_signature: String,
    genesis_signature: String,
    aggregate_verification_key: String,
    signed_message: String,
}

#[derive(Debug)]
struct Certificate {
    hash: String,
    previous_hash: Option<String>,
    signature: CertificateSignature,
    avk: StmAggrVerificationKey<D>,
    message: Vec<u8>,
    protocol_parameters: StmParameters,
}

impl TryFrom<CertificateMessage> for Certificate {
    type Error = String;

    fn try_from(value: CertificateMessage) -> Result<Certificate, Self::Error> {
        let signature = if value.previous_hash.is_empty() {
            &value.genesis_signature
        } else {
            &value.multi_signature
        };

        Certificate::new(
            &value.hash,
            &value.previous_hash,
            signature,
            &value.aggregate_verification_key,
            &value.signed_message,
        )
    }
}

impl Certificate {
    pub fn new(
        hash: &str,
        previous_hash: &str,
        signature: &str,
        avk: &str,
        message: &str,
    ) -> Result<Self, String> {
        let previous_hash = if previous_hash.is_empty() {
            None
        } else {
            Some(previous_hash.to_owned())
        };
        let signature = if previous_hash.is_some() {
            CertificateSignature::MultiSignature(key_decode_hex(signature)?)
        } else {
            CertificateSignature::GenesisSignature(ed25519_dalek::Signature::from_bytes(
                Vec::from_hex(signature)
                    .map_err(|e| format!("HEX parsing error: {e}"))?
                    .as_slice()
                    .try_into()
                    .map_err(|e| {
                        format!("The given signature is not 64 bytes long. ERROR = '{e}'.")
                    })?,
            ))
        };

        let myself = Self {
            hash: hash.to_owned(),
            previous_hash,
            signature,
            avk: key_decode_hex(avk)?,
            message: message.as_bytes().to_owned(),
            protocol_parameters: PROTOCOL_PARAMETERS,
        };

        Ok(myself)
    }

    pub fn verify(&self) -> Result<(), String> {
        self.signature
            .verify(&self.message, &self.avk, &self.protocol_parameters)
            .map_err(|e| format!("ERROR: {e:?}"))
    }

    pub fn dummy_genesis() -> Result<Self, String> {
        let hash = "18b2a801d1311fa3d9ede8c418e2cbaebdcf640f143466b2acf517a8d111ea32";
        let previous_hash = "";
        let signature = "bdd1cd24cec3b31ff141effa9d0c7d88c109d4a8b98876f3b40f6183c6703ff3cb1b2fdf447b1ff5c108125d39816c4f7fdf4e0f92302542aef5f851622c3407";
        let avk = "7b226d745f636f6d6d69746d656e74223a7b22726f6f74223a5b3233312c3234392c36342c35372c33392c37322c3231312c35302c38312c312c31372c33312c3133302c3135362c39362c3230372c3234342c37392c3137322c3130352c38332c37302c39382c3135332c3235302c3133382c3231322c38342c3130302c3233312c3136332c3138355d2c226e725f6c6561766573223a31322c22686173686572223a6e756c6c7d2c22746f74616c5f7374616b65223a32313238363535393737393431377d";
        let message = "03143aac8a613af4434ab0fc5cb05839de569597695042b7465e43258f225c55";

        Self::new(hash, previous_hash, signature, avk, message)
    }

    pub fn dummy_multisig() -> Result<Self, String> {
        let hash = "61b241a842ae986e54df26a4…fba5122cb20ca77df74131f";
        let previous_hash = "6cb7696b7faceb7da5e42bb9…1e45df83221f4a51c25bf7f";
        let signature = "7b227369676e617475726573223a5b5b7b227369676d61223a5b3135332c3132372c3230352c3230312c3134392c32362c3233372c3132322c33372c3139362c36382c3233332c3232382c3131382c37382c37322c32342c3130392c32382c37392c36372c32302c3233312c3133322c3136382c3133352c37302c3133312c3234342c372c3235312c3130302c3231372c3231362c3133302c38322c37382c3130352c3130392c3234382c3137312c3134372c33302c3130352c3231352c3137322c3137352c3130305d2c22696e6465786573223a5b312c36315d2c227369676e65725f696e646578223a397d2c5b5b3133322c3131342c37332c31372c32372c39382c3234312c3233312c3131382c302c3131362c3139372c36362c3131312c36332c3131392c3137362c34312c32332c3131332c3235322c37332c3133392c39332c3134392c3132352c33312c3134322c33312c3135332c312c34332c35392c3135372c322c3233362c3138342c3231392c3233312c36302c3233312c3233322c3132342c3233332c3139322c3135322c3137342c302c362c3139372c35382c3139382c3233322c3134352c35302c3234312c3231322c3133362c3235342c3137302c3138342c3135302c3233342c3135392c37332c3138312c3137302c3137372c3231392c32372c3230332c3234342c3235352c3134322c3139382c33312c3135372c3234362c3135342c38322c3138372c3135302c3231352c35332c312c3132332c35332c37382c392c3132322c3139322c39332c3139322c37372c3130342c3133305d2c313039333739323135383038305d5d2c5b7b227369676d61223a5b3137352c3231322c3130382c39372c3132362c3136312c3232342c3233322c34302c3138372c3134332c3233322c3230332c31352c3231302c3234362c3230302c32362c3133372c3137302c39332c3130322c3233362c3138352c36332c372c39362c3135312c35382c3232362c3135302c3138312c3232392c38332c36372c37362c3136392c3135372c3131392c32332c3230362c3231312c3130392c33392c312c3133332c38382c39385d2c22696e6465786573223a5b322c31342c31382c32302c34392c35352c39375d2c227369676e65725f696e646578223a32357d2c5b5b3134322c3136302c3134382c3131322c3139312c35372c36332c3134342c372c3234312c3132372c3133392c3137352c312c3231372c3230322c3135312c3232322c38352c3233312c32322c35382c39302c3232312c3135372c39362c3130342c33382c32332c3131362c342c3131372c34352c3232332c32352c3137302c35312c3139332c3134322c3133372c33362c35312c3233352c3230312c3233382c31362c3135392c37332c31332c38302c37392c36312c3131322c3234392c32372c392c3232362c35362c35312c3134342c36332c37322c3232342c372c332c36372c3138322c3133302c36352c3132372c3136332c32332c3138302c3233372c35322c37302c312c3138372c3136382c3130342c35392c3230332c3235322c38322c3232392c3136372c3232332c3231332c3231302c3135382c3130332c3230362c3132352c35392c3139372c33375d2c343130313336323531373930355d5d5d2c2262617463685f70726f6f66223a7b2276616c756573223a5b5b3135392c3130302c3139352c37312c31352c3234312c3138352c3233352c31352c3135342c3233372c38362c31312c35392c3139342c3135352c3131342c36322c3138302c3130342c392c3139352c33352c342c3136312c39392c37332c3131322c38362c39352c39392c33345d2c5b33362c3230372c3137302c3130342c3133302c3134372c36362c3130302c3132312c34372c322c32312c3131382c34312c33302c3138312c3137332c34372c3131392c3135302c35332c36302c33372c3232332c34352c3133312c3231382c35302c31352c3131372c37302c34305d2c5b32332c37332c32352c33382c33332c3230362c35352c3232302c37392c3139392c3136332c3231362c3133312c3137332c3230392c3130312c35382c3234352c37312c3137372c38372c3234362c3139302c3134392c38332c3139322c3231362c3132352c3131382c3233392c3132342c38345d2c5b38322c3232342c31382c3130322c33332c39302c32392c3233392c3135322c37342c3232362c33312c3132342c37352c3232362c39302c352c3135392c3139342c39332c3233342c35362c3135372c3230372c3230392c3234372c32302c38382c39312c3138392c3133392c36305d2c5b31312c3231372c37322c3230322c38302c3132312c3235312c33332c32392c35392c3233352c3138322c3135372c3132362c39392c3137382c3132372c3130322c3232342c35332c3235352c31322c3134362c33372c3232392c3230342c35312c3230302c3138342c3131372c3130312c3139305d2c5b3132362c3234342c3138392c3131342c3139362c31322c3134382c37362c37372c3232382c3131322c3232352c3132312c3137352c33382c3230332c39312c35352c3131392c362c3232372c35342c3235322c33342c3131362c34312c35372c3134312c3235302c3230332c3135302c3138345d2c5b372c36352c3231342c37392c3136372c35312c342c3136322c382c35312c3230322c3131302c3131352c3133302c3134362c3139362c3137372c3231352c3134372c3139352c3130312c3135342c3230332c3139302c3235352c3234392c39392c3131322c38352c33382c3233332c3131335d2c5b37322c3132312c3131332c3234362c32362c3133342c32342c3234302c3234302c31302c3133342c3139382c3137342c3231362c3234372c3131322c36362c3234342c3233352c3133312c31392c3137332c39322c3139332c38312c3130352c3232392c39312c36372c37372c3137302c3137305d5d2c22696e6469636573223a5b392c32355d2c22686173686572223a6e756c6c7d7d";
        let avk = "7b226d745f636f6d6d69746d656e74223a7b22726f6f74223a5b3130332c3137352c3138312c3233382c32302c3133332c3131302c3232302c36332c3233352c3139392c33342c39352c36322c3133302c3131372c3234362c3135332c3130302c3135392c31352c3234372c33362c3132302c31362c3230372c36332c3137352c3235322c3130352c3133382c375d2c226e725f6c6561766573223a32382c22686173686572223a6e756c6c7d2c22746f74616c5f7374616b65223a35343533383535303239363136317d";
        let message = "765e790269f21055e42d019386f9398784238a8e9b7aa5b3ad4d984f75ab756b";

        Self::new(hash, previous_hash, signature, avk, message)
    }
}

async fn fetch_certificate(hash: &str) -> Result<Certificate, String> {
    todo!();
    /*     let mut opts = RequestInit::new();
    opts.method("GET");
    opts.mode(RequestMode::Cors);
    let url = format!("{AGGREGATOR_URL}/certificate/{hash}");
    let request = Request::new_with_str_and_init(&url, &opts)
        .map_err(|e| format!("WEB-SYS: request error: {e:?}"))?;
    request
        .headers()
        .set("Accept", "application/vnd.github.v3+json")
        .map_err(|e| format!("WEB-SYS: headers error: {e:?}"))?;
    let window = web_sys::window().ok_or_else(|| "WEB-SYS: no Window created!".to_string())?;
    let resp_value = JsFuture::from(window.fetch_with_request(&request))
        .await
        .map_err(|e| format!("WEB-SYS: fetch error: {e:?}"))?;
    let response: Response = resp_value
        .dyn_into()
        .map_err(|e| format!("WEB-SYS: response error: {e:?}"))?;
    let json = JsFuture::from(
        response
            .json()
            .map_err(|e| format!("WEB-SYS: Cannot read JSON response from body: {e:?}"))?,
    )
    .await
    .map_err(|e| format!("WEB-SYS: Cannot read JS memory: {e:?}"))?;

    let certificate_message: CertificateMessage = serde_json::from_str(
        &json
            .as_string()
            .ok_or_else(|| format!("WEB-SYS: given JSON is not a String"))?,
    )
    .map_err(|e| format!("SERDE-JSON: Could not deserialize CertificateMessge from given JSON "))?;

    certificate_message.try_into() */
}

#[wasm_bindgen(start)]
pub fn main() -> Result<(), JsValue> {
    /*
    let mut certificate = fetch_certificate(&Certificate::dummy_multisig()?.hash).await?;

    loop {
        certificate.verify().map_err(|e| {
            format!(
                "Verification failed for certificate hash='{}', ERROR = '{e}",
                certificate.hash
            )
        })?;
        console_log!("OK certificate hash='{}'.", certificate.hash);

        certificate = match &certificate.previous_hash {
            None => break,
            Some(hash) => fetch_certificate(hash).await?,
        };
    } */
    alert("❎ certificate chain verified!");

    Ok(())
}
