use mithril_stm::stm::StmSig;
use serde::{Deserialize, Serialize};
use std::fmt::{Debug, Formatter};

use crate::{
    crypto_helper::ProtocolSingleSignature,
    entities::{LotteryIndex, PartyId},
};

/// SingleSignatures represent single signatures originating from a participant in the network
/// for a digest at won lottery indexes
#[derive(Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct SingleSignatures {
    /// The unique identifier of the signer
    pub party_id: PartyId,

    /// The single signature of the digest
    pub signature: ProtocolSingleSignature,

    /// The indexes of the won lotteries that lead to the single signatures
    #[serde(rename = "indexes")]
    pub won_indexes: Vec<LotteryIndex>,

    /// Message that is signed by the signer
    ///
    /// Used to buffer the signature for later if the aggregator has yet to create an open message
    /// for the signed entity type.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub signed_message: Option<String>,

    /// Status of the authentication of the signer that emitted the signature
    pub authentication_status: SingleSignatureAuthenticationStatus,
}

/// Status of the authentication of the signer that emitted the signature
#[derive(Copy, Clone, PartialEq, Eq, Default, Serialize, Deserialize)]
pub enum SingleSignatureAuthenticationStatus {
    /// We could authenticate the signer that emitted the signature
    Authenticated,
    /// We could not authenticate the signer that emitted the signature
    #[default]
    Unauthenticated,
}

impl SingleSignatures {
    /// `SingleSignatures` factory
    pub fn new<T: Into<PartyId>>(
        party_id: T,
        signature: ProtocolSingleSignature,
        won_indexes: Vec<LotteryIndex>,
    ) -> SingleSignatures {
        SingleSignatures {
            party_id: party_id.into(),
            signature,
            won_indexes,
            signed_message: None,
            authentication_status: SingleSignatureAuthenticationStatus::Unauthenticated,
        }
    }

    /// `SingleSignatures` factory including the signed message
    pub fn new_with_signed_message<T: Into<PartyId>>(
        party_id: T,
        signature: ProtocolSingleSignature,
        won_indexes: Vec<LotteryIndex>,
        signed_message: String,
    ) -> SingleSignatures {
        SingleSignatures {
            party_id: party_id.into(),
            signature,
            won_indexes,
            signed_message: Some(signed_message),
            authentication_status: SingleSignatureAuthenticationStatus::Unauthenticated,
        }
    }

    /// Convert this [SingleSignatures] to its corresponding [MithrilStm Signature][StmSig].
    pub fn to_protocol_signature(&self) -> StmSig {
        self.signature.clone().into()
    }

    /// Check that the signer that emitted the signature is authenticated
    pub fn is_authenticated(&self) -> bool {
        self.authentication_status == SingleSignatureAuthenticationStatus::Authenticated
    }
}

cfg_test_tools! {
impl SingleSignatures {
    /// Create a fake [SingleSignatures] with valid cryptographic data for testing purposes.
    pub fn fake<T1: Into<String>, T2: Into<String>>(party_id: T1, message: T2) -> Self {
        Self {
            signed_message: None,
            ..Self::fake_with_signed_message(party_id, message)
        }
    }

    /// Create a fake [SingleSignatures] with valid cryptographic data for testing purposes.
    // TODO: this method is slow due to the fixture creation, we should either make
    // the fixture faster or find a faster alternative.
    pub fn fake_with_signed_message<T1: Into<String>, T2: Into<String>>(party_id: T1, message: T2) -> Self {
        use crate::entities::{ProtocolParameters};
        use crate::test_utils::{MithrilFixtureBuilder, StakeDistributionGenerationMethod};

        let party_id = party_id.into();
        let message = message.into();

        let fixture = MithrilFixtureBuilder::default()
            .with_stake_distribution(StakeDistributionGenerationMethod::Custom(
                std::collections::BTreeMap::from([(party_id.to_string(), 100)]),
            ))
            .with_protocol_parameters(ProtocolParameters::new(1, 1, 1.0))
            .build();
        let signature = fixture.signers_fixture()[0].sign(&message).unwrap();

        Self {
            party_id,
            signature: signature.signature,
            won_indexes: vec![10, 15],
            signed_message: Some(message),
            authentication_status: SingleSignatureAuthenticationStatus::Unauthenticated,
        }
    }
}
}

impl Debug for SingleSignatures {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let is_pretty_printing = f.alternate();
        let mut debug = f.debug_struct("SingleSignatures");
        debug
            .field("party_id", &self.party_id)
            .field("won_indexes", &format_args!("{:?}", self.won_indexes))
            .field("signed_message", &self.signed_message);

        match is_pretty_printing {
            true => debug
                .field("signature", &format_args!("{:?}", self.signature))
                .finish(),
            false => debug.finish_non_exhaustive(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{crypto_helper::tests_setup::setup_message, test_utils::MithrilFixtureBuilder};

    #[test]
    fn single_signatures_should_convert_to_protocol_signatures() {
        let message = setup_message();
        let fixture = MithrilFixtureBuilder::default().with_signers(1).build();
        let signer = &fixture.signers_fixture()[0];
        let protocol_sigs = signer
            .protocol_signer
            .sign(message.compute_hash().as_bytes())
            .unwrap();

        let signature = SingleSignatures::new(
            signer.signer_with_stake.party_id.to_owned(),
            protocol_sigs.clone().into(),
            protocol_sigs.indexes.clone(),
        );

        assert_eq!(protocol_sigs, signature.to_protocol_signature());
    }
}
