use async_trait::async_trait;
use mithril_common::StdResult;
use std::collections::HashMap;

use mithril_common::entities::{Epoch, PartyId, Signer, SignerWithStake};

/// Store and get signers verification keys for given epoch.
///
/// Important note: This store works on the **recording** epoch, the epoch at which the signers
/// are signed into a certificate so they can sign single signatures at the next epoch.
#[cfg_attr(test, mockall::automock)]
#[async_trait]
pub trait VerificationKeyStorer: Sync + Send {
    /// Save the verification key, for the given [Signer] for the given [Epoch], returns the
    /// previous values if one already existed.
    async fn save_verification_key(
        &self,
        epoch: Epoch,
        signer: SignerWithStake,
    ) -> StdResult<Option<SignerWithStake>>;

    /// Returns a HashMap of [Signer] indexed by [PartyId] for the given `epoch`.
    async fn get_verification_keys(
        &self,
        epoch: Epoch,
    ) -> StdResult<Option<HashMap<PartyId, Signer>>>;

    /// Returns the list of signers for the given `epoch`.
    async fn get_signers(&self, epoch: Epoch) -> StdResult<Option<Vec<SignerWithStake>>>;

    /// Prune all verification keys that are at or below the given epoch.
    async fn prune_verification_keys(&self, max_epoch_to_prune: Epoch) -> StdResult<()>;
}
