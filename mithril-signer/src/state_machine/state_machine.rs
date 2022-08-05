use slog_scope::{debug, error, info};
use std::{error::Error, thread::sleep, time::Duration};

use mithril_common::entities::Beacon;

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
                    self.runner.update_stake_distribution().await?;
                    self.runner
                        .register_signer_to_aggregator(&pending_certificate)
                        .await?;
                    self.state = SignerState::Registered(RegisteredState {
                        beacon: pending_certificate.beacon,
                    });
                }

                Ok(())
            }
            SignerState::Registered(state) => Ok(()),
            SignerState::Signed(state) => Ok(()),
        }
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
}

#[cfg(test)]
mod tests {
    use mithril_common::{
        entities::{Epoch, ImmutableFileNumber},
        fake_data,
    };

    use super::*;
    use crate::state_machine::runner::MockSignerRunner;

    fn init_state_machine(init_state: SignerState, runner: MockSignerRunner) -> StateMachine {
        StateMachine {
            state: init_state,
            runner: Box::new(runner),
            state_sleep: Duration::from_millis(100),
        }
    }

    fn create_beacon(epoch: Epoch, immutable_file_number: ImmutableFileNumber) -> Beacon {
        Beacon {
            network: "testnet".to_string(),
            epoch,
            immutable_file_number,
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
            .expect_get_current_beacon()
            .times(1)
            .returning(move || Ok(create_beacon(10, 999)));
        runner
            .expect_update_stake_distribution()
            .times(1)
            .returning(|| Ok(()));
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
}
