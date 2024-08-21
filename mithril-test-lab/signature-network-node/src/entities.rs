use mithril_common::messages::RegisterSignatureMessage;
use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum Message {
    MithrilRegisterSignature(RegisterSignatureMessage),
}
