use sha2::{Digest, Sha256};
use std::ops::Deref;

use crate::{BaseFieldElement, StmResult, circuits::halo2_ivc::types::MessageHash};

#[derive(Clone, Debug)]
pub struct GenesisMessagePreimage(pub Vec<u8>);

impl TryInto<MessageHash> for &GenesisMessagePreimage {
    type Error = anyhow::Error;
    fn try_into(self) -> StmResult<MessageHash> {
        let genesis_preimage_hash: [u8; 32] = Sha256::digest(self.0.clone()).into();
        let genesis_message_field_elem = BaseFieldElement::from_raw(&genesis_preimage_hash)?.0;
        Ok(MessageHash::from_field(genesis_message_field_elem))
    }
}

impl Deref for GenesisMessagePreimage {
    type Target = [u8];

    fn deref(&self) -> &[u8] {
        &self.0
    }
}
