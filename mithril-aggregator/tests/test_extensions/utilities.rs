use mithril_common::crypto_helper::{
    ProtocolInitializer, ProtocolPartyId, ProtocolSigner, ProtocolSignerVerificationKey,
    ProtocolStake,
};

pub type TestSigner = (
    ProtocolPartyId,
    ProtocolStake,
    ProtocolSignerVerificationKey,
    ProtocolSigner,
    ProtocolInitializer,
);

/// Simple struct to give a more helpful error message when ticking the state machine
pub struct TickCounter {
    tick_no: u64,
}

impl TickCounter {
    pub fn new() -> Self {
        Self { tick_no: 0 }
    }

    pub fn increase(&mut self) {
        self.tick_no += 1;
    }

    pub fn get_message(&self) -> String {
        format!(
            "Ticking the state machine should not fail (tick n° {})",
            self.tick_no
        )
    }
}
