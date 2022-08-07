use slog_scope::{debug, error, info};
use std::{error::Error, thread::sleep, time::Duration};

use mithril_common::entities::{Beacon, CertificatePending, Signer};

use super::Runner;

#[derive(Debug, PartialEq)]
pub struct RegisteredState {
    beacon: Beacon,
}

#[derive(Debug, PartialEq)]
pub struct SignedState {
    beacon: Beacon,
}

#[derive(Debug, PartialEq)]
pub enum SignerState {
    Unregistered,
    Registered(RegisteredState),
    Signed(SignedState),
}

pub struct StateMachine {
    state: SignerState,
    runner: Box<dyn Runner>,
    state_sleep: Duration,
}

impl StateMachine {
    pub fn new(
        starting_state: SignerState,
        runner: Box<dyn Runner>,
        state_sleep: Duration,
    ) -> Self {
        Self {
            state: starting_state,
            runner,
            state_sleep,
        }
    }

    pub fn get_state(&self) -> &SignerState {
        &self.state
    }

    /// perform a cycle of the state machine
    pub async fn cycle(&mut self) -> Result<(), Box<dyn Error + Sync + Send>> {
        info!("================================================================================");
        info!("New state machine cycle.");

        match &self.state {
            SignerState::Unregistered => {
                if let Some(pending_certificate) = self.runner.get_pending_certificate().await? {
                    debug!("Pending certificate found, launching registration.");
                    let state = self
                        .from_unregistered_to_registered(&pending_certificate)
                        .await?;
                    self.state = SignerState::Registered(state);
                }
            }
            SignerState::Registered(state) => {
                if let Some(_new_beacon) = self.has_beacon_changed(&state.beacon).await? {
                    self.state = SignerState::Unregistered;
                } else if let Some(pending_certificate) =
                    self.runner.get_pending_certificate().await?
                {
                    if let Some(_signer) = self.runner.can_i_sign(&pending_certificate) {
                        let state = self.from_registered_to_signed(&pending_certificate).await?;
                        self.state = SignerState::Signed(state)
                    }
                }
            }
            SignerState::Signed(state) => {
                if let Some(_new_beacon) = self.has_beacon_changed(&state.beacon).await? {
                    self.state = SignerState::Unregistered;
                }
            }
        };

        Ok(())
    }

    /// launch the state machine until an error occures or it is interrupted
    pub async fn run(&mut self) -> Box<dyn Error> {
        info!("state machine: launching");

        loop {
            if let Err(e) = self.cycle().await {
                error!("state machine: an error occured");

                return e;
            }
            info!("Sleeping for {} ms", self.state_sleep.as_millis());
            sleep(self.state_sleep);
        }
    }

    async fn has_beacon_changed(
        &self,
        beacon: &Beacon,
    ) -> Result<Option<Beacon>, Box<dyn Error + Sync + Send>> {
        let current_beacon = self.runner.get_current_beacon().await?;

        if current_beacon.epoch > beacon.epoch {
            Ok(Some(current_beacon))
        } else {
            Ok(None)
        }
    }

    async fn from_unregistered_to_registered(
        &self,
        pending_certificate: &CertificatePending,
    ) -> Result<RegisteredState, Box<dyn Error + Sync + Send>> {
        let beacon = pending_certificate.beacon.clone();
        self.runner.update_stake_distribution(beacon.epoch).await?;
        self.runner
            .register_signer_to_aggregator(pending_certificate)
            .await?;

        Ok(RegisteredState { beacon })
    }

    async fn from_registered_to_signed(
        &self,
        pending_certificate: &CertificatePending,
    ) -> Result<SignedState, Box<dyn Error + Sync + Send>> {
        let (message, signers) = self.runner.compute_message(pending_certificate).await?;
        let single_signatures = self
            .runner
            .compute_single_signature(&message, &signers, pending_certificate.beacon.epoch)
            .await?;
        self.runner.send_single_signature(single_signatures).await?;

        Ok(SignedState {
            beacon: pending_certificate.beacon.clone(),
        })
    }
}

#[cfg(test)]
mod tests {
    use mithril_common::{entities::ProtocolMessage, fake_data};

    use super::*;
    use crate::state_machine::runner::MockSignerRunner;

    fn init_state_machine(init_state: SignerState, runner: MockSignerRunner) -> StateMachine {
        StateMachine {
            state: init_state,
            runner: Box::new(runner),
            state_sleep: Duration::from_millis(100),
        }
    }

    #[tokio::test]
    async fn unregistered_no_transition() {
        let mut runner = MockSignerRunner::new();
        runner
            .expect_get_pending_certificate()
            .times(1)
            .returning(|| Ok(None));
        let mut state_machine = init_state_machine(SignerState::Unregistered, runner);
        state_machine
            .cycle()
            .await
            .expect("Cycling the state machine should not fail");

        assert_eq!(&SignerState::Unregistered, state_machine.get_state());
    }

    #[tokio::test]
    async fn unregistered_to_registered() {
        let mut runner = MockSignerRunner::new();
        runner
            .expect_get_pending_certificate()
            .times(1)
            .returning(|| Ok(Some(fake_data::certificate_pending())));
        runner
            .expect_update_stake_distribution()
            .times(1)
            .returning(|_| Ok(()));
        runner
            .expect_register_signer_to_aggregator()
            .times(1)
            .returning(|_| Ok(()));

        let mut state_machine = init_state_machine(SignerState::Unregistered, runner);

        state_machine
            .cycle()
            .await
            .expect("Cycling the state machine should not fail");

        if let SignerState::Registered(_state) = state_machine.get_state() {
        } else {
            panic!(
                "state machine did not return a RegisteredState but {:?}",
                state_machine.get_state()
            );
        }
    }

    #[tokio::test]
    async fn registered_to_unregistered() {
        let mut runner = MockSignerRunner::new();
        runner
            .expect_get_current_beacon()
            .times(1)
            .returning(|| Ok(fake_data::beacon()));
        let mut state_machine = init_state_machine(
            SignerState::Registered(RegisteredState {
                beacon: Beacon {
                    epoch: 0,
                    immutable_file_number: 0,
                    ..Default::default()
                },
            }),
            runner,
        );
        state_machine
            .cycle()
            .await
            .expect("Cycling the state machine should not fail");
        assert_eq!(&SignerState::Unregistered, state_machine.get_state());
    }

    #[tokio::test]
    async fn registered_to_registered() {
        let beacon = Beacon {
            immutable_file_number: 99,
            epoch: 9,
            ..Default::default()
        };
        let state = RegisteredState {
            beacon: beacon.clone(),
        };

        let mut certificate_pending = fake_data::certificate_pending();
        certificate_pending.beacon = beacon.clone();
        let mut runner = MockSignerRunner::new();
        runner
            .expect_get_current_beacon()
            .times(1)
            .returning(move || Ok(beacon.to_owned()));
        runner
            .expect_get_pending_certificate()
            .times(1)
            .returning(move || Ok(Some(certificate_pending.to_owned())));
        runner.expect_can_i_sign().times(1).returning(|_| None);

        let mut state_machine = init_state_machine(SignerState::Registered(state), runner);
        state_machine
            .cycle()
            .await
            .expect("Cycling the state machine should not fail");
        if let SignerState::Registered(state) = state_machine.get_state() {
            assert_eq!(99, state.beacon.immutable_file_number);
            assert_eq!(9, state.beacon.epoch);
        } else {
            panic!(
                "state machine did not return a RegisteredState but {:?}",
                state_machine.get_state()
            );
        }
    }
}
