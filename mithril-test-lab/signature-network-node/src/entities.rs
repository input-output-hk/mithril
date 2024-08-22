use std::sync::Arc;

use serde::{Deserialize, Serialize};
use tokio::sync::Mutex;

use mithril_common::messages::RegisterSignatureMessage;

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum Message {
    MithrilRegisterSignature(RegisterSignatureMessage),
}

#[derive(Clone)]
pub struct RouterDependencies {
    pub available_signatures_registrations: Arc<Mutex<Vec<RegisterSignatureMessage>>>,
}
