use std::sync::Arc;

use serde::{Deserialize, Serialize};
use tokio::sync::{mpsc, Mutex};

use mithril_common::messages::RegisterSignatureMessage;

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum Message {
    MithrilRegisterSignature(RegisterSignatureMessage),
}

#[derive(Clone)]
pub struct RouterDependencies {
    pub available_signatures_registrations: Arc<Mutex<Vec<RegisterSignatureMessage>>>,
    pub incoming_messages_sender: mpsc::Sender<Message>,
}
