use mithril_common::entities::{Epoch, Signer};

/// `RegisteredSigners` represents the registered signers of an epoch
#[derive(Clone, Debug, PartialEq)]
pub struct RegisteredSigners {
    /// Epoch for which those registrations are active.
    pub epoch: Epoch,

    /// Current Signers
    pub current_signers: Vec<Signer>,

    /// Signers that will be able to sign on the next epoch
    pub next_signers: Vec<Signer>,
}

#[cfg(test)]
impl mithril_common::test::double::Dummy for RegisteredSigners {
    /// Create a dummy `RegisteredSigners`
    fn dummy() -> RegisteredSigners {
        use mithril_common::test::double::fake_data;

        // Beacon
        let beacon = fake_data::beacon();

        // Signers
        let signers = fake_data::signers(5);
        let current_signers = signers[1..3].to_vec();
        let next_signers = signers[2..5].to_vec();

        // Signer Epoch settings
        RegisteredSigners {
            epoch: beacon.epoch,
            current_signers,
            next_signers,
        }
    }
}
