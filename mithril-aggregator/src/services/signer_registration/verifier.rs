use std::sync::Arc;

use anyhow::{anyhow, Context};
use async_trait::async_trait;

use mithril_common::{
    chain_observer::ChainObserver,
    crypto_helper::{KESPeriod, ProtocolKeyRegistration},
    entities::{Signer, SignerWithStake, StakeDistribution},
    StdResult,
};

use super::SignerRegistrationVerifier;

/// Implementation of a [SignerRegistrationVerifier]
pub struct MithrilSignerRegistrationVerifier {
    /// Chain observer service.
    chain_observer: Arc<dyn ChainObserver>,
}

impl MithrilSignerRegistrationVerifier {
    /// Creates a new [MithrilSignerRegistrationVerifier].
    pub fn new(chain_observer: Arc<dyn ChainObserver>) -> Self {
        Self { chain_observer }
    }
}

#[async_trait]
impl SignerRegistrationVerifier for MithrilSignerRegistrationVerifier {
    async fn verify(
        &self,
        signer: &Signer,
        stake_distribution: &StakeDistribution,
    ) -> StdResult<SignerWithStake> {
        let mut key_registration = ProtocolKeyRegistration::init(
            &stake_distribution
                .iter()
                .map(|(k, v)| (k.to_owned(), *v))
                .collect::<Vec<_>>(),
        );
        let party_id_register = match signer.party_id.as_str() {
            "" => None,
            party_id => Some(party_id.to_string()),
        };
        let kes_period = match &signer.operational_certificate {
            Some(operational_certificate) => Some(
                self.chain_observer
                    .get_current_kes_period(operational_certificate)
                    .await?
                    .unwrap_or_default()
                    - operational_certificate.start_kes_period as KESPeriod,
            ),
            None => None,
        };
        let party_id_registered = key_registration
            .register(
                party_id_register.clone(),
                signer.operational_certificate.clone(),
                signer.verification_key_signature,
                kes_period,
                signer.verification_key,
            )
            .with_context(|| {
                format!(
                    "KeyRegwrapper can not register signer with party_id: '{:?}'",
                    party_id_register
                )
            })
            .map_err(|e| anyhow!(e))?;
        let party_id_registered_stake = *stake_distribution
            .get(&party_id_registered)
            .ok_or(anyhow!("Stake not found"))?;

        Ok(SignerWithStake {
            party_id: party_id_registered,
            ..SignerWithStake::from_signer(signer.to_owned(), party_id_registered_stake)
        })
    }
}

#[cfg(test)]
mod tests {
    use mithril_common::{
        chain_observer::FakeObserver, entities::TimePoint, test_utils::MithrilFixtureBuilder,
    };

    use super::*;

    #[tokio::test]
    async fn verify_succeeds_with_valid_signer_registration() {
        let fixture = MithrilFixtureBuilder::default().with_signers(1).build();
        let signer_to_register: Signer = fixture.signers()[0].to_owned();
        let signer_registration_verifier = MithrilSignerRegistrationVerifier::new(Arc::new(
            FakeObserver::new(Some(TimePoint::dummy())),
        ));

        signer_registration_verifier
            .verify(&signer_to_register, &fixture.stake_distribution())
            .await
            .unwrap();
    }

    #[tokio::test]
    async fn verify_fails_with_invalid_signer_registration() {
        let fixture = MithrilFixtureBuilder::default().with_signers(2).build();
        let signer_to_register: Signer = Signer {
            verification_key_signature: fixture.signers()[1].verification_key_signature,
            ..fixture.signers()[0].to_owned()
        };
        let signer_registration_verifier = MithrilSignerRegistrationVerifier::new(Arc::new(
            FakeObserver::new(Some(TimePoint::dummy())),
        ));

        signer_registration_verifier
            .verify(&signer_to_register, &fixture.stake_distribution())
            .await
            .expect_err("Verification should fail");
    }
}
