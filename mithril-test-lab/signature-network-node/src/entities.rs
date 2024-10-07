use std::sync::Arc;

use serde::{Deserialize, Serialize};
use tokio::sync::{mpsc, Mutex};

use mithril_common::entities::SignedEntityType;
use mithril_common::messages::RegisterSignatureMessage;

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum Message {
    MithrilRegisterSignature(RegisterSignatureMessage),
}

impl Message {
    pub fn file_identifier(&self) -> String {
        match self {
            Message::MithrilRegisterSignature(msg) => format!(
                "register-signature-{}-{}",
                get_signed_entity_type_identifier(&msg.signed_entity_type),
                msg.party_id
            ),
        }
    }
}

fn get_signed_entity_type_identifier(signed_entity_type: &SignedEntityType) -> String {
    match signed_entity_type {
        SignedEntityType::MithrilStakeDistribution(epoch) => {
            format!("msd-e{}", epoch.0)
        }
        SignedEntityType::CardanoStakeDistribution(epoch) => {
            format!("csd-e{}", epoch.0)
        }
        SignedEntityType::CardanoImmutableFilesFull(beacon) => {
            format!(
                "cif-{}-e{}-i{}",
                beacon.network, beacon.epoch, beacon.immutable_file_number
            )
        }
        SignedEntityType::CardanoTransactions(epoch, block_number) => {
            format!("ctx-e{}-b{}", epoch, block_number)
        }
    }
}

#[derive(Clone)]
pub struct RouterDependencies {
    pub available_signatures_registrations: Arc<Mutex<Vec<RegisterSignatureMessage>>>,
    pub incoming_messages_sender: mpsc::Sender<Message>,
}

#[cfg(test)]
mod tests {
    use mithril_common::entities::{BlockNumber, CardanoDbBeacon, Epoch, SignedEntityType};

    use super::*;

    #[test]
    fn get_message_file_identifier() {
        let inner_message = RegisterSignatureMessage {
            signed_entity_type: SignedEntityType::MithrilStakeDistribution(Epoch(3)),
            party_id: "party-24".to_string(),
            ..RegisterSignatureMessage::dummy()
        };

        assert_eq!(
            Message::MithrilRegisterSignature(inner_message.clone()).file_identifier(),
            "register-signature-msd-e3-party-24".to_string()
        );

        assert_eq!(
            Message::MithrilRegisterSignature(RegisterSignatureMessage {
                party_id: "43209FA789FD9".to_string(),
                ..inner_message.clone()
            })
            .file_identifier(),
            "register-signature-msd-e3-43209FA789FD9".to_string()
        );

        assert_eq!(
            Message::MithrilRegisterSignature(RegisterSignatureMessage {
                signed_entity_type: SignedEntityType::CardanoTransactions(
                    Epoch(24),
                    BlockNumber(32)
                ),
                ..inner_message.clone()
            })
            .file_identifier(),
            "register-signature-ctx-e24-b32-party-24".to_string()
        );

        assert_eq!(
            Message::MithrilRegisterSignature(RegisterSignatureMessage {
                signed_entity_type: SignedEntityType::CardanoImmutableFilesFull(
                    CardanoDbBeacon::new("net", 72, 367)
                ),
                ..inner_message.clone()
            })
            .file_identifier(),
            "register-signature-cif-net-e72-i367-party-24".to_string()
        );

        assert_eq!(
            Message::MithrilRegisterSignature(RegisterSignatureMessage {
                signed_entity_type: SignedEntityType::CardanoStakeDistribution(Epoch(687)),
                ..inner_message.clone()
            })
            .file_identifier(),
            "register-signature-csd-e687-party-24".to_string()
        );
    }
}
