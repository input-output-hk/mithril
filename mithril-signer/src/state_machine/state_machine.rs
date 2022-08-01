use slog_scope::{debug, error, info, trace};
use std::{error::Error, thread::sleep, time::Duration};

use mithril_common::entities::Beacon;

use crate::certificate_handler;

use super::Runner;

pub struct RegisteredState {
    beacon: Beacon,
}

pub struct SignedState {
    beacon: Beacon,
}

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

    /// perform a cycle of the state machine
    pub async fn cycle(&mut self) -> Result<(), Box<dyn Error + Sync + Send>> {
        match &self.state {
            SignerState::Unregistered => {
                if self.runner.get_pending_certificate().await?.is_some() {
                    let beacon = self.runner.get_current_beacon().await?;
                    self.runner.update_stake_distribution().await?;
                    self.state = SignerState::Registered(RegisteredState { beacon });
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
