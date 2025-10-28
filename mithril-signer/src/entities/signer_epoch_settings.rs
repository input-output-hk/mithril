use mithril_common::entities::{Epoch, Signer};

/// SignerEpochSettings represents the settings of an epoch
#[derive(Clone, Debug, PartialEq)]
pub struct SignerEpochSettings {
    /// Current Epoch
    pub epoch: Epoch,

    /// Current Signers
    pub current_signers: Vec<Signer>,

    /// Signers that will be able to sign on the next epoch
    pub next_signers: Vec<Signer>,
}

#[cfg(test)]
impl mithril_common::test::double::Dummy for SignerEpochSettings {
    /// Create a dummy `SignerEpochSettings`
    fn dummy() -> SignerEpochSettings {
        use mithril_common::test::double::fake_data;

        // Beacon
        let beacon = fake_data::beacon();

        // Signers
        let signers = fake_data::signers(5);
        let current_signers = signers[1..3].to_vec();
        let next_signers = signers[2..5].to_vec();

        // Signer Epoch settings
        SignerEpochSettings {
            epoch: beacon.epoch,
            current_signers,
            next_signers,
        }
    }
}
