use slog_scope::{debug, error, info};
use std::{error::Error, fmt::Display, thread::sleep, time::Duration};

use mithril_common::entities::{Beacon, CertificatePending, Epoch, EpochSettings, SignerWithStake};

use super::Runner;

/// Structure to hold `Registered` state information.
#[derive(Debug, PartialEq, Eq)]
pub struct RegisteredState {
    beacon: Beacon,
}

/// Structure to hold `Signed` state information.
#[derive(Debug, PartialEq, Eq)]
pub struct SignedState {
    beacon: Beacon,
}

/// Different possible states of the state machine.
#[derive(Debug, PartialEq, Eq)]
pub enum SignerState {
    /// starting state, may hold the latest known epoch in order to help synchronisation
    /// with the aggregator
    Unregistered(Option<Epoch>),

    /// `Registered` state
    Registered(RegisteredState),

    /// `Signed` state
    Signed(SignedState),
}

impl SignerState {
    /// Returns `true` if the state in `Unregistered`
    pub fn is_unregistered(&self) -> bool {
        matches!(*self, SignerState::Unregistered(_))
    }

    /// Returns `true` if the state in `Registered`
    pub fn is_registered(&self) -> bool {
        matches!(*self, SignerState::Registered(_))
    }

    /// Returns `true` if the state in `Signed`
    pub fn is_signed(&self) -> bool {
        matches!(*self, SignerState::Signed(_))
    }
}

impl Display for SignerState {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Unregistered(state) => write!(
                f,
                "Unregistered - {}",
                match state {
                    None => "No Epoch".to_string(),
                    Some(e) => format!("Epoch({})", e),
                }
            ),
            Self::Registered(state) => write!(f, "Registered - {}", state.beacon),
            Self::Signed(state) => write!(f, "Signed - {}", state.beacon),
        }
    }
}

/// The state machine is responsible of the execution of the signer automate.
pub struct StateMachine {
    state: SignerState,
    runner: Box<dyn Runner>,
    state_sleep: Duration,
}

impl StateMachine {
    /// Create a new StateMachine instance.
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

    /// Return the current state of the state machine.
    pub fn get_state(&self) -> &SignerState {
        &self.state
    }

    /// Launch the state machine until an error occurs or it is interrupted.
    pub async fn run(&mut self) -> Result<(), Box<dyn Error>> {
        info!("STATE MACHINE: launching");

        loop {
            if let Err(e) = self.cycle().await {
                error!("STATE MACHINE: an error occured: "; "error" => ?e);
            }

            info!(
                "… Cycle finished, Sleeping for {} ms",
                self.state_sleep.as_millis()
            );
            sleep(self.state_sleep);
        }
    }

    /// Perform a cycle of the state machine.
    pub async fn cycle(&mut self) -> Result<(), Box<dyn Error + Sync + Send>> {
        info!("================================================================================");
        info!("STATE MACHINE: new cycle: {}", self.state);

        match &self.state {
            SignerState::Unregistered(maybe_epoch) => {
                if let Some(epoch_settings) = self.runner.get_epoch_settings().await? {
                    if maybe_epoch.map_or_else(|| true, |e| epoch_settings.epoch >= e) {
                        info!("→ Epoch settings found, transiting to REGISTERED");
                        let state = self
                            .transition_from_unregistered_to_registered(&epoch_settings)
                            .await?;
                        self.state = SignerState::Registered(state);
                    } else {
                        info!(
                            " ⋅ Epoch settings found, but it's epoch is behind the known epoch, waiting…";
                            "epoch_settings" => ?epoch_settings,
                            "known_epoch" => ?maybe_epoch,
                        );
                    }
                } else {
                    info!(" ⋅ No epoch settings found yet, waiting…");
                }
            }
            SignerState::Registered(state) => {
                if let Some(new_beacon) = self.has_epoch_changed(&state.beacon).await? {
                    info!("→ Epoch has changed, transiting to UNREGISTERED");
                    self.state = SignerState::Unregistered(Some(new_beacon.epoch));
                } else if let Some(pending_certificate) =
                    self.runner.get_pending_certificate().await?
                {
                    info!(
                        " ⋅ Epoch has NOT changed but there is a pending certificate";
                        "pending_certificate" => ?pending_certificate
                    );

                    if self.runner.can_i_sign(&pending_certificate).await? {
                        info!(" → we can sign this certificate, transiting to SIGNED");
                        let state = self
                            .transition_from_registered_to_signed(&pending_certificate)
                            .await?;
                        self.state = SignerState::Signed(state)
                    } else {
                        info!(" ⋅ cannot sign this pending certificate, waiting…");
                    }
                } else {
                    info!(" ⋅ no pending certificate, waiting…");
                }
            }
            SignerState::Signed(state) => {
                if let Some(new_beacon) = self.has_beacon_changed(&state.beacon).await? {
                    info!("  New beacon detected: {:?}", new_beacon);

                    if new_beacon.epoch > state.beacon.epoch {
                        info!(" → new Epoch detected, transiting to UNREGISTERED");
                        self.state = SignerState::Unregistered(Some(new_beacon.epoch));
                    } else {
                        info!(" → new immutable file detected, transiting to REGISTERED");
                        self.state =
                            SignerState::Registered(RegisteredState { beacon: new_beacon });
                    }
                } else {
                    info!(" ⋅ NO new beacon detected, waiting");
                }
            }
        };

        Ok(())
    }

    /// Return true if the epoch is different than the one in the given beacon.
    async fn has_epoch_changed(
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

    /// Return true if the current beacon is different than the given beacon.
    async fn has_beacon_changed(
        &self,
        beacon: &Beacon,
    ) -> Result<Option<Beacon>, Box<dyn Error + Sync + Send>> {
        let current_beacon = self.runner.get_current_beacon().await?;

        if &current_beacon != beacon {
            Ok(Some(current_beacon))
        } else {
            Ok(None)
        }
    }

    /// Launch the transition process from the `Unregistered` to the `Registered` state.
    async fn transition_from_unregistered_to_registered(
        &self,
        epoch_settings: &EpochSettings,
    ) -> Result<RegisteredState, Box<dyn Error + Sync + Send>> {
        let beacon = self.runner.get_current_beacon().await?;
        self.runner.update_stake_distribution(beacon.epoch).await?;
        self.runner
            .register_signer_to_aggregator(beacon.epoch, &epoch_settings.next_protocol_parameters)
            .await?;

        Ok(RegisteredState { beacon })
    }

    /// Launch the transition process from the `Registered` to the `Signed` state.
    async fn transition_from_registered_to_signed(
        &self,
        pending_certificate: &CertificatePending,
    ) -> Result<SignedState, Box<dyn Error + Sync + Send>> {
        let current_beacon = &pending_certificate.beacon;
        let (retrieval_epoch, next_retrieval_epoch) = (
            current_beacon.epoch.offset_to_signer_retrieval_epoch()?,
            current_beacon.epoch.offset_to_next_signer_retrieval_epoch(),
        );

        debug!(
            " > transition_from_registered_to_signed";
            "current_epoch" => ?current_beacon.epoch,
            "retrieval_epoch" => ?retrieval_epoch,
            "next_retrieval_epoch" => ?next_retrieval_epoch,
        );

        let signers: Vec<SignerWithStake> = self
            .runner
            .associate_signers_with_stake(retrieval_epoch, &pending_certificate.signers)
            .await?;
        let next_signers: Vec<SignerWithStake> = self
            .runner
            .associate_signers_with_stake(next_retrieval_epoch, &pending_certificate.next_signers)
            .await?;

        let message = self
            .runner
            .compute_message(current_beacon, &next_signers)
            .await?;
        let single_signatures = self
            .runner
            .compute_single_signature(current_beacon.epoch, &message, &signers)
            .await?;
        self.runner.send_single_signature(single_signatures).await?;

        Ok(SignedState {
            beacon: current_beacon.clone(),
        })
    }
}

#[cfg(test)]
mod tests {
    use mithril_common::entities::Epoch;
    use mithril_common::{entities::ProtocolMessage, fake_data};

    use super::*;
    use crate::runtime::runner::MockSignerRunner;

    fn init_state_machine(init_state: SignerState, runner: MockSignerRunner) -> StateMachine {
        StateMachine {
            state: init_state,
            runner: Box::new(runner),
            state_sleep: Duration::from_millis(100),
        }
    }

    #[tokio::test]
    async fn unregistered_epoch_settings_not_found() {
        let mut runner = MockSignerRunner::new();
        runner
            .expect_get_epoch_settings()
            .once()
            .returning(|| Ok(None));
        let mut state_machine = init_state_machine(SignerState::Unregistered(None), runner);
        state_machine
            .cycle()
            .await
            .expect("Cycling the state machine should not fail");

        assert_eq!(&SignerState::Unregistered(None), state_machine.get_state());
    }

    #[tokio::test]
    async fn unregistered_epoch_settings_behind_known_epoch() {
        let mut runner = MockSignerRunner::new();
        let epoch_settings = EpochSettings {
            epoch: Epoch(3),
            protocol_parameters: fake_data::protocol_parameters(),
            next_protocol_parameters: fake_data::protocol_parameters(),
        };
        let known_epoch = Epoch(4);
        runner
            .expect_get_epoch_settings()
            .once()
            .returning(move || Ok(Some(epoch_settings.to_owned())));
        let mut state_machine =
            init_state_machine(SignerState::Unregistered(Some(known_epoch)), runner);
        state_machine
            .cycle()
            .await
            .expect("Cycling the state machine should not fail");

        assert_eq!(
            &SignerState::Unregistered(Some(known_epoch)),
            state_machine.get_state()
        );
    }

    #[tokio::test]
    async fn unregistered_to_registered() {
        let mut runner = MockSignerRunner::new();
        runner
            .expect_get_epoch_settings()
            .once()
            .returning(|| Ok(Some(fake_data::epoch_settings())));
        runner
            .expect_get_current_beacon()
            .once()
            .returning(|| Ok(fake_data::beacon()));
        runner
            .expect_update_stake_distribution()
            .once()
            .returning(|_| Ok(()));
        runner
            .expect_register_signer_to_aggregator()
            .once()
            .returning(|_, _| Ok(()));

        let mut state_machine = init_state_machine(SignerState::Unregistered(None), runner);

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
            .once()
            .returning(|| Ok(fake_data::beacon()));
        let mut state_machine = init_state_machine(
            SignerState::Registered(RegisteredState {
                beacon: Beacon {
                    epoch: Epoch(0),
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
        assert_eq!(
            &SignerState::Unregistered(Some(fake_data::beacon().epoch)),
            state_machine.get_state()
        );
    }

    #[tokio::test]
    async fn registered_to_registered() {
        let beacon = Beacon {
            immutable_file_number: 99,
            epoch: Epoch(9),
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
            .once()
            .returning(move || Ok(beacon.to_owned()));
        runner
            .expect_get_pending_certificate()
            .once()
            .returning(move || Ok(Some(certificate_pending.to_owned())));
        runner.expect_can_i_sign().once().returning(|_| Ok(false));

        let mut state_machine = init_state_machine(SignerState::Registered(state), runner);
        state_machine
            .cycle()
            .await
            .expect("Cycling the state machine should not fail");

        assert_eq!(
            SignerState::Registered(RegisteredState {
                beacon: Beacon {
                    epoch: Epoch(9),
                    immutable_file_number: 99,
                    ..Default::default()
                }
            }),
            *state_machine.get_state(),
            "state machine did not return a RegisteredState but {:?}",
            state_machine.get_state()
        );
    }

    #[tokio::test]
    async fn registered_to_signed() {
        let beacon = Beacon {
            immutable_file_number: 99,
            epoch: Epoch(9),
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
            .once()
            .returning(move || Ok(beacon.clone()));
        runner
            .expect_get_pending_certificate()
            .once()
            .returning(move || Ok(Some(certificate_pending.clone())));
        runner.expect_can_i_sign().once().returning(|_| Ok(true));
        runner
            .expect_associate_signers_with_stake()
            .times(2)
            .returning(|_, _| Ok(fake_data::signers_with_stakes(4)));
        runner
            .expect_compute_single_signature()
            .once()
            .returning(|_, _, _| Ok(Some(fake_data::single_signatures(vec![1, 5, 23]))));
        runner
            .expect_compute_message()
            .once()
            .returning(|_, _| Ok(ProtocolMessage::new()));
        runner
            .expect_send_single_signature()
            .once()
            .returning(|_| Ok(()));

        let mut state_machine = init_state_machine(SignerState::Registered(state), runner);
        state_machine
            .cycle()
            .await
            .expect("Cycling the state machine should not fail");

        assert_eq!(
            SignerState::Signed(SignedState {
                beacon: Beacon {
                    epoch: Epoch(9),
                    immutable_file_number: 99,
                    ..Default::default()
                }
            }),
            *state_machine.get_state(),
            "state machine did not return a RegisteredState but {:?}",
            state_machine.get_state()
        );
    }

    #[tokio::test]
    async fn signed_to_registered() {
        let beacon = Beacon {
            immutable_file_number: 99,
            epoch: Epoch(9),
            ..Default::default()
        };
        let new_beacon = Beacon {
            immutable_file_number: 100,
            ..beacon.clone()
        };
        let state = SignedState {
            beacon: beacon.clone(),
        };

        let mut runner = MockSignerRunner::new();
        runner
            .expect_get_current_beacon()
            .once()
            .returning(move || Ok(new_beacon.to_owned()));

        let mut state_machine = init_state_machine(SignerState::Signed(state), runner);
        state_machine
            .cycle()
            .await
            .expect("Cycling the state machine should not fail");

        assert_eq!(
            SignerState::Registered(RegisteredState {
                beacon: Beacon {
                    immutable_file_number: 100,
                    ..beacon.clone()
                }
            }),
            *state_machine.get_state()
        );
    }

    #[tokio::test]
    async fn signed_to_unregistered() {
        let beacon = Beacon {
            immutable_file_number: 99,
            epoch: Epoch(9),
            ..Default::default()
        };
        let new_beacon = Beacon {
            epoch: Epoch(10),
            ..beacon.clone()
        };
        let state = SignedState {
            beacon: beacon.clone(),
        };

        let mut runner = MockSignerRunner::new();
        runner
            .expect_get_current_beacon()
            .once()
            .returning(move || Ok(new_beacon.to_owned()));

        let mut state_machine = init_state_machine(SignerState::Signed(state), runner);
        state_machine
            .cycle()
            .await
            .expect("Cycling the state machine should not fail");

        assert_eq!(
            SignerState::Unregistered(Some(Epoch(10))),
            *state_machine.get_state()
        );
    }
}
