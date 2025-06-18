use anyhow::Context;
use mithril_common::{
    entities::{SingleSignature, SingleSignatureAuthenticationStatus},
    messages::{RegisterSignatureMessage, TryFromMessageAdapter},
    StdResult,
};

pub struct FromRegisterSingleSignatureAdapter;

impl TryFromMessageAdapter<RegisterSignatureMessage, SingleSignature>
    for FromRegisterSingleSignatureAdapter
{
    fn try_adapt(
        register_single_signature_message: RegisterSignatureMessage,
    ) -> StdResult<SingleSignature> {
        let signature = SingleSignature {
            party_id: register_single_signature_message.party_id,
            signature: register_single_signature_message
                .signature
                .try_into()
                .with_context(|| {
                    "'FromRegisterSingleSignatureAdapter' can not convert the single signature"
                })?,
            won_indexes: register_single_signature_message.won_indexes,
            authentication_status: SingleSignatureAuthenticationStatus::Unauthenticated,
        };

        Ok(signature)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn try_adapt_single_signature_message_to_entity() {
        let message = RegisterSignatureMessage::dummy();
        let signature = FromRegisterSingleSignatureAdapter::try_adapt(message.clone()).unwrap();

        assert_eq!(message.party_id, signature.party_id);
        assert_eq!(
            SingleSignatureAuthenticationStatus::Unauthenticated,
            signature.authentication_status
        );
    }

    mod golden_protocol_key_encodings {
        use mithril_common::entities::{CardanoDbBeacon, Epoch, SignedEntityType};

        use super::*;

        fn golden_message_with_json_hex_encoding() -> RegisterSignatureMessage {
            RegisterSignatureMessage {
                signed_entity_type: SignedEntityType::CardanoImmutableFilesFull(
                    CardanoDbBeacon::new(*Epoch(10), 1728),
                ),
                party_id: "party_id".to_string(),
                signature: "7b227369676d61223a5b3133302c3137372c31352c3232392c32342c3235312c3234372c3137312c3139362c3231302c3134332c3131332c38362c3138392c39322c35362c3131322c33332c3139332c3231322c35342c3231342c32382c3231362c3232372c3137332c3130302c3132372c3137382c34302c39382c38372c32392c3138312c3235352c3131312c3135372c3232342c3233352c34362c3130302c3136392c3233322c3138392c3235322c38322c3133392c33365d2c22696e6465786573223a5b302c312c332c342c362c382c392c31302c31312c31322c31342c31382c32312c32322c32332c32352c32362c32372c33302c33332c33342c33382c34312c34332c35302c35382c35392c36302c36312c36322c36372c36392c37312c37332c37352c37362c37372c38312c38322c38332c38342c39302c39312c39322c39332c39372c39385d2c227369676e65725f696e646578223a327d".to_string(),
                won_indexes: vec![1, 3],
                signed_message: "6a7e737c312972d2346b65ac3075696e04286d046dddaf8004121e3d5e27cc0d".to_string(),
            }
        }

        fn golden_message_with_bytes_hex_encoding() -> RegisterSignatureMessage {
            RegisterSignatureMessage {
                signed_entity_type: SignedEntityType::CardanoImmutableFilesFull(
                    CardanoDbBeacon::new(*Epoch(10), 1728),
                ),
                party_id: "party_id".to_string(),
                signature: "000000000000002f0000000000000000000000000000000100000000000000030000000000000004000000000000000600000000000000080000000000000009000000000000000a000000000000000b000000000000000c000000000000000e00000000000000120000000000000015000000000000001600000000000000170000000000000019000000000000001a000000000000001b000000000000001e0000000000000021000000000000002200000000000000260000000000000029000000000000002b0000000000000032000000000000003a000000000000003b000000000000003c000000000000003d000000000000003e0000000000000043000000000000004500000000000000470000000000000049000000000000004b000000000000004c000000000000004d0000000000000051000000000000005200000000000000530000000000000054000000000000005a000000000000005b000000000000005c000000000000005d0000000000000061000000000000006282b10fe518fbf7abc4d28f7156bd5c387021c1d436d61cd8e3ad647fb22862571db5ff6f9de0eb2e64a9e8bdfc528b240000000000000002".to_string(),
                won_indexes: vec![1, 3],
                signed_message: "6a7e737c312972d2346b65ac3075696e04286d046dddaf8004121e3d5e27cc0d"
                    .to_string(),
            }
        }

        #[test]
        fn restorations_from_json_hex_and_bytes_hex_give_same_signature() {
            let signature_from_json_hex = FromRegisterSingleSignatureAdapter::try_adapt(
                golden_message_with_json_hex_encoding(),
            )
            .unwrap();
            let signature_from_bytes_hex = FromRegisterSingleSignatureAdapter::try_adapt(
                golden_message_with_bytes_hex_encoding(),
            )
            .unwrap();

            assert_eq!(signature_from_json_hex, signature_from_bytes_hex);
        }
    }
}
