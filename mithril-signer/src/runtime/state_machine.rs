use slog::{crit, debug, error, info, Logger};
use std::{fmt::Display, ops::Deref, sync::Arc, time::Duration};
use tokio::{sync::Mutex, time::sleep};

use mithril_common::{
    crypto_helper::ProtocolInitializerError,
    entities::{Epoch, TimePoint},
    logging::LoggerExtensions,
};

use crate::entities::{BeaconToSign, SignerEpochSettings};
use crate::MetricsService;

use super::{Runner, RuntimeError};

/// Different possible states of the state machine.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SignerState {
    /// Starting state
    Init,
    /// Hold the latest known epoch in order to help synchronisation
    /// with the aggregator
    Unregistered {
        /// Current Epoch
        epoch: Epoch,
    },

    /// `ReadyToSign` state. The signer is registered and ready to sign new messages.
    ReadyToSign {
        /// Epoch when signer transitioned to the state.
        epoch: Epoch,
    },

    /// `RegisteredNotAbleToSign` state. The signer is registered but not able to sign for the duration of the epoch.
    RegisteredNotAbleToSign {
        /// Epoch when signer transitioned to the state.
        epoch: Epoch,
    },
}

impl SignerState {
    /// Returns `true` if the state in `Init`
    pub fn is_init(&self) -> bool {
        matches!(*self, SignerState::Init)
    }

    /// Returns `true` if the state in `Unregistered`
    pub fn is_unregistered(&self) -> bool {
        matches!(*self, SignerState::Unregistered { .. })
    }

    /// Returns `true` if the state in `ReadyToSign`
    pub fn is_ready_to_sign(&self) -> bool {
        matches!(*self, SignerState::ReadyToSign { .. })
    }

    /// Returns `true` if the state in `RegisteredNotAbleToSign`
    pub fn is_registered_not_able_to_sign(&self) -> bool {
        matches!(*self, SignerState::RegisteredNotAbleToSign { .. })
    }
}

impl Display for SignerState {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Init => write!(f, "Init"),
            Self::Unregistered { epoch } => write!(f, "Unregistered - {epoch:?}"),
            Self::RegisteredNotAbleToSign { epoch } => {
                write!(f, "RegisteredNotAbleToSign - {epoch}")
            }
            Self::ReadyToSign { epoch } => {
                write!(f, "ReadyToSign - {epoch}")
            }
        }
    }
}

/// The state machine is responsible of the execution of the signer automate.
pub struct StateMachine {
    state: Mutex<SignerState>,
    runner: Box<dyn Runner>,
    state_sleep: Duration,
    metrics_service: Arc<MetricsService>,
    logger: Logger,
}

impl StateMachine {
    /// Create a new StateMachine instance.
    pub fn new(
        starting_state: SignerState,
        runner: Box<dyn Runner>,
        state_sleep: Duration,
        metrics_service: Arc<MetricsService>,
        logger: Logger,
    ) -> Self {
        Self {
            state: Mutex::new(starting_state),
            runner,
            state_sleep,
            metrics_service,
            logger: logger.new_with_component_name::<Self>(),
        }
    }

    /// Return the current state of the state machine.
    pub async fn get_state(&self) -> SignerState {
        self.state.lock().await.to_owned()
    }

    /// Launch the state machine until an error occurs or it is interrupted.
    pub async fn run(&self) -> Result<(), RuntimeError> {
        info!(self.logger, "launching");

        loop {
            if let Err(e) = self.cycle().await {
                if e.is_critical() {
                    crit!(self.logger, "{e}");

                    return Err(e);
                } else {
                    error!(self.logger, "{e}");
                }
            }

            info!(
                self.logger,
                "… Cycle finished, Sleeping for {} ms",
                self.state_sleep.as_millis()
            );
            sleep(self.state_sleep).await;
        }
    }

    /// Perform a cycle of the state machine.
    pub async fn cycle(&self) -> Result<(), RuntimeError> {
        let mut state = self.state.lock().await;
        info!(
            self.logger,
            "================================================================================"
        );
        info!(self.logger, "new cycle: {}", *state);

        self.metrics_service
            .runtime_cycle_total_since_startup_counter_increment();

        match state.deref() {
            SignerState::Init => {
                *state = self.transition_from_init_to_unregistered().await?;
            }
            SignerState::Unregistered { epoch } => {
                if let Some(new_epoch) = self.has_epoch_changed(*epoch).await? {
                    info!(
                        self.logger,
                        "→ Epoch has changed, transiting to Unregistered"
                    );
                    *state = self
                        .transition_from_unregistered_to_unregistered(new_epoch)
                        .await?;
                } else if let Some(epoch_settings) = self
                    .runner
                    .get_epoch_settings()
                    .await
                    .map_err(|e| RuntimeError::KeepState {
                        message: format!("could not retrieve epoch settings at epoch {epoch:?}"),
                        nested_error: Some(e),
                    })?
                {
                    info!(self.logger, "→ Epoch settings found");
                    if epoch_settings.epoch >= *epoch {
                        info!(self.logger, "new Epoch found");
                        info!(self.logger, " ⋅ transiting to Registered");
                        *state = self
                            .transition_from_unregistered_to_one_of_registered_states(
                                epoch_settings,
                            )
                            .await?;
                    } else {
                        info!(
                            self.logger,
                            " ⋅ Epoch settings found, but its epoch is behind the known epoch, waiting…";
                            "epoch_settings" => ?epoch_settings,
                            "known_epoch" => ?epoch,
                        );
                    }
                } else {
                    info!(self.logger, "→ No epoch settings found yet, waiting…");
                }
            }
            SignerState::RegisteredNotAbleToSign { epoch } => {
                if let Some(new_epoch) = self.has_epoch_changed(*epoch).await? {
                    info!(
                        self.logger,
                        " → new Epoch detected, transiting to Unregistered"
                    );
                    *state = self
                        .transition_from_registered_not_able_to_sign_to_unregistered(new_epoch)
                        .await?;
                } else {
                    info!(self.logger, " ⋅ Epoch has NOT changed, waiting…");
                }
            }

            SignerState::ReadyToSign { epoch } => match self.has_epoch_changed(*epoch).await? {
                Some(new_epoch) => {
                    info!(
                        self.logger,
                        "→ Epoch has changed, transiting to Unregistered"
                    );
                    *state = self
                        .transition_from_ready_to_sign_to_unregistered(new_epoch)
                        .await?;
                }
                None => {
                    let beacon_to_sign = self.runner.get_beacon_to_sign().await.map_err(|e| {
                        RuntimeError::KeepState {
                            message: "could not fetch the beacon to sign".to_string(),
                            nested_error: Some(e),
                        }
                    })?;

                    match beacon_to_sign {
                        Some(beacon) => {
                            info!(
                                self.logger,
                                "→ Epoch has NOT changed we can sign this beacon, transiting to ReadyToSign";
                                "beacon_to_sign" => ?beacon,
                            );
                            *state = self
                                .transition_from_ready_to_sign_to_ready_to_sign(*epoch, beacon)
                                .await?;
                        }
                        None => {
                            info!(self.logger, " ⋅ no beacon to sign, waiting…");
                        }
                    }
                }
            },
        };

        self.metrics_service
            .runtime_cycle_success_since_startup_counter_increment();

        Ok(())
    }

    /// Return the new epoch if the epoch is different than the given one.
    async fn has_epoch_changed(&self, epoch: Epoch) -> Result<Option<Epoch>, RuntimeError> {
        let current_time_point = self
            .get_current_time_point("checking if epoch has changed")
            .await?;

        if current_time_point.epoch > epoch {
            Ok(Some(current_time_point.epoch))
        } else {
            Ok(None)
        }
    }

    async fn transition_from_unregistered_to_unregistered(
        &self,
        new_epoch: Epoch,
    ) -> Result<SignerState, RuntimeError> {
        self.update_era_checker(new_epoch, "unregistered → unregistered")
            .await?;

        Ok(SignerState::Unregistered { epoch: new_epoch })
    }

    async fn transition_from_init_to_unregistered(&self) -> Result<SignerState, RuntimeError> {
        let current_epoch = self
            .get_current_time_point("init → unregistered")
            .await?
            .epoch;
        self.update_era_checker(current_epoch, "init → unregistered")
            .await?;

        Ok(SignerState::Unregistered {
            epoch: current_epoch,
        })
    }

    /// Launch the transition process from the `Unregistered` to `ReadyToSign` or `RegisteredNotAbleToSign` state.
    async fn transition_from_unregistered_to_one_of_registered_states(
        &self,
        epoch_settings: SignerEpochSettings,
    ) -> Result<SignerState, RuntimeError> {
        self.metrics_service
            .signer_registration_total_since_startup_counter_increment();

        let time_point = self
            .get_current_time_point("unregistered → registered")
            .await?;
        let epoch = time_point.epoch;
        self.runner.update_stake_distribution(epoch)
            .await
            .map_err(|e| RuntimeError::KeepState {
                message: format!("Could not update stake distribution in 'unregistered → registered' phase for epoch {:?}.", epoch),
                nested_error: Some(e),
            })?;

        self.runner
            .inform_epoch_settings(epoch_settings)
            .await
            .map_err(|e| RuntimeError::KeepState {
                message: format!(
                    "Could not register epoch information in 'unregistered → registered' phase for epoch {:?}.",
                    epoch
                ),
                nested_error: Some(e),
            })?;

        self.runner.register_signer_to_aggregator()
            .await.map_err(|e| {
            if e.downcast_ref::<ProtocolInitializerError>().is_some() {
                RuntimeError::Critical { message: format!("Could not register to aggregator in 'unregistered → registered' phase for epoch {:?}.", epoch), nested_error: Some(e) }
            } else {
                RuntimeError::KeepState { message: format!("Could not register to aggregator in 'unregistered → registered' phase for epoch {:?}.", epoch), nested_error: Some(e) }
            }
        })?;

        self.metrics_service
            .signer_registration_success_since_startup_counter_increment();
        self.metrics_service
            .signer_registration_success_last_epoch_gauge_set(epoch);

        self.runner
            .upkeep(epoch)
            .await
            .map_err(|e| RuntimeError::KeepState {
                message: "Failed to upkeep signer in 'unregistered → registered' phase".to_string(),
                nested_error: Some(e),
            })?;

        match self
            .runner
            .can_sign_current_epoch()
            .await
            .map_err(|e| RuntimeError::KeepState {
                message: "Failed to check if signer can sign in the current epoch in 'unregistered → ?' phase".to_string(),
                nested_error: Some(e),
            })? {
            true => Ok(SignerState::ReadyToSign { epoch }),
            false => Ok(SignerState::RegisteredNotAbleToSign { epoch }),
        }
    }

    async fn transition_from_registered_not_able_to_sign_to_unregistered(
        &self,
        epoch: Epoch,
    ) -> Result<SignerState, RuntimeError> {
        self.update_era_checker(epoch, "registered not able to sign → unregistered")
            .await?;

        Ok(SignerState::Unregistered { epoch })
    }

    async fn transition_from_ready_to_sign_to_unregistered(
        &self,
        epoch: Epoch,
    ) -> Result<SignerState, RuntimeError> {
        self.update_era_checker(epoch, "ready to sign → unregistered")
            .await?;

        Ok(SignerState::Unregistered { epoch })
    }

    /// Launch the transition process from the `ReadyToSign` to the `ReadyToSign` state.
    async fn transition_from_ready_to_sign_to_ready_to_sign(
        &self,
        current_epoch: Epoch,
        beacon_to_sign: BeaconToSign,
    ) -> Result<SignerState, RuntimeError> {
        let (retrieval_epoch, next_retrieval_epoch) = (
            current_epoch.offset_to_signer_retrieval_epoch()?,
            current_epoch.offset_to_next_signer_retrieval_epoch(),
        );

        debug!(
            self.logger,
            " > transition_from_ready_to_sign_to_ready_to_sign";
            "current_epoch" => ?current_epoch,
            "retrieval_epoch" => ?retrieval_epoch,
            "next_retrieval_epoch" => ?next_retrieval_epoch,
        );

        self.metrics_service
            .signature_registration_total_since_startup_counter_increment();

        let message = self
            .runner
            .compute_message(&beacon_to_sign.signed_entity_type)
            .await
            .map_err(|e| RuntimeError::KeepState {
                message: format!("Could not compute message during 'ready to sign → ready to sign' phase (current epoch {current_epoch:?})"),
                nested_error: Some(e)
            })?;

        self.runner.compute_publish_single_signature(&beacon_to_sign, &message)
            .await
            .map_err(|e| RuntimeError::KeepState {
                message: format!("Could not compute and publish single signature during 'ready to sign → ready to sign' phase (current epoch {current_epoch:?})"),
                nested_error: Some(e)
            })?;

        self.metrics_service
            .signature_registration_success_since_startup_counter_increment();
        self.metrics_service
            .signature_registration_success_last_epoch_gauge_set(current_epoch);

        Ok(SignerState::ReadyToSign {
            epoch: current_epoch,
        })
    }

    async fn get_current_time_point(&self, context: &str) -> Result<TimePoint, RuntimeError> {
        let current_time_point =
            self.runner
                .get_current_time_point()
                .await
                .map_err(|e| RuntimeError::KeepState {
                    message: format!(
                        "Could not retrieve current time point in context '{context}'."
                    ),
                    nested_error: Some(e),
                })?;

        Ok(current_time_point)
    }

    async fn update_era_checker(&self, epoch: Epoch, context: &str) -> Result<(), RuntimeError> {
        self.runner
            .update_era_checker(epoch)
            .await
            .map_err(|e| RuntimeError::Critical {
                message: format!(
                    "Could not update Era checker with context '{context}' for epoch {epoch:?}"
                ),
                nested_error: Some(e),
            })
    }
}

#[cfg(test)]
mod tests {
    use chrono::DateTime;
    use mockall::predicate;

    use mithril_common::entities::{ChainPoint, Epoch, ProtocolMessage, SignedEntityType};
    use mithril_common::test_utils::fake_data;

    use crate::runtime::runner::MockSignerRunner;
    use crate::test_tools::TestLogger;

    use super::*;

    fn init_state_machine(init_state: SignerState, runner: MockSignerRunner) -> StateMachine {
        let logger = TestLogger::stdout();
        let metrics_service = Arc::new(MetricsService::new(logger.clone()).unwrap());
        StateMachine {
            state: init_state.into(),
            runner: Box::new(runner),
            state_sleep: Duration::from_millis(100),
            metrics_service,
            logger,
        }
    }

    #[tokio::test]
    async fn unregistered_epoch_settings_not_found() {
        let mut runner = MockSignerRunner::new();
        runner
            .expect_get_epoch_settings()
            .once()
            .returning(|| Ok(None));
        runner
            .expect_get_current_time_point()
            .once()
            .returning(|| Ok(TimePoint::dummy()));
        let state_machine = init_state_machine(
            SignerState::Unregistered {
                epoch: TimePoint::dummy().epoch,
            },
            runner,
        );
        state_machine
            .cycle()
            .await
            .expect("Cycling the state machine should not fail");

        assert_eq!(
            SignerState::Unregistered {
                epoch: TimePoint::dummy().epoch
            },
            state_machine.get_state().await
        );
    }

    #[tokio::test]
    async fn unregistered_epoch_settings_behind_known_epoch() {
        let mut runner = MockSignerRunner::new();
        let epoch_settings = SignerEpochSettings {
            epoch: Epoch(3),
            protocol_parameters: fake_data::protocol_parameters(),
            next_protocol_parameters: fake_data::protocol_parameters(),
            current_signers: vec![],
            next_signers: vec![],
            cardano_transactions_signing_config: None,
            next_cardano_transactions_signing_config: None,
        };
        let known_epoch = Epoch(4);
        runner
            .expect_get_epoch_settings()
            .once()
            .returning(move || Ok(Some(epoch_settings.to_owned())));
        runner.expect_get_current_time_point().once().returning(|| {
            Ok(TimePoint {
                epoch: Epoch(4),
                ..TimePoint::dummy()
            })
        });
        let state_machine =
            init_state_machine(SignerState::Unregistered { epoch: known_epoch }, runner);
        state_machine
            .cycle()
            .await
            .expect("Cycling the state machine should not fail");

        assert_eq!(
            SignerState::Unregistered { epoch: known_epoch },
            state_machine.get_state().await
        );
    }

    #[tokio::test]
    async fn unregistered_to_registered_not_able_to_sign() {
        let mut runner = MockSignerRunner::new();
        runner.expect_upkeep().returning(|_| Ok(())).once();
        runner
            .expect_get_epoch_settings()
            .once()
            .returning(|| Ok(Some(SignerEpochSettings::dummy())));

        runner
            .expect_inform_epoch_settings()
            .with(predicate::eq(SignerEpochSettings::dummy()))
            .once()
            .returning(|_| Ok(()));

        runner
            .expect_get_current_time_point()
            .times(2)
            .returning(|| Ok(TimePoint::dummy()));
        runner
            .expect_update_stake_distribution()
            .once()
            .returning(|_| Ok(()));
        runner
            .expect_register_signer_to_aggregator()
            .once()
            .returning(|| Ok(()));

        runner
            .expect_can_sign_current_epoch()
            .once()
            .returning(|| Ok(false));

        let state_machine = init_state_machine(
            SignerState::Unregistered {
                epoch: TimePoint::dummy().epoch,
            },
            runner,
        );

        state_machine
            .cycle()
            .await
            .expect("Cycling the state machine should not fail");

        if let SignerState::RegisteredNotAbleToSign { epoch: _ } = state_machine.get_state().await {
        } else {
            panic!(
                "state machine did not return a RegisteredNotAbleToSign state but {:?}",
                state_machine.get_state().await
            );
        }
    }

    #[tokio::test]
    async fn unregistered_to_ready_to_sign() {
        let mut runner = MockSignerRunner::new();
        runner.expect_upkeep().returning(|_| Ok(())).once();
        runner
            .expect_get_epoch_settings()
            .once()
            .returning(|| Ok(Some(SignerEpochSettings::dummy())));

        runner
            .expect_inform_epoch_settings()
            .with(predicate::eq(SignerEpochSettings::dummy()))
            .once()
            .returning(|_| Ok(()));

        runner
            .expect_get_current_time_point()
            .times(2)
            .returning(|| Ok(TimePoint::dummy()));
        runner
            .expect_update_stake_distribution()
            .once()
            .returning(|_| Ok(()));
        runner
            .expect_register_signer_to_aggregator()
            .once()
            .returning(|| Ok(()));

        runner
            .expect_can_sign_current_epoch()
            .once()
            .returning(|| Ok(true));

        let state_machine = init_state_machine(
            SignerState::Unregistered {
                epoch: TimePoint::dummy().epoch,
            },
            runner,
        );

        state_machine
            .cycle()
            .await
            .expect("Cycling the state machine should not fail");

        assert_eq!(
            SignerState::ReadyToSign {
                epoch: TimePoint::dummy().epoch,
            },
            state_machine.get_state().await
        );
    }

    #[tokio::test]
    async fn registered_not_able_to_sign_to_unregistered() {
        let mut runner = MockSignerRunner::new();
        runner
            .expect_get_current_time_point()
            .once()
            .returning(|| Ok(TimePoint::new(10, 100, ChainPoint::dummy())));
        runner
            .expect_update_era_checker()
            .once()
            .returning(|_e: Epoch| Ok(()));

        let state_machine = init_state_machine(
            SignerState::RegisteredNotAbleToSign { epoch: Epoch(0) },
            runner,
        );

        state_machine
            .cycle()
            .await
            .expect("Cycling the state machine should not fail");
        assert_eq!(
            SignerState::Unregistered { epoch: Epoch(10) },
            state_machine.get_state().await
        );
    }

    #[tokio::test]
    async fn registered_not_able_to_sign_to_registered_not_able_to_sign() {
        let mut runner = MockSignerRunner::new();
        runner
            .expect_get_current_time_point()
            .once()
            .returning(|| Ok(TimePoint::new(10, 100, ChainPoint::dummy())));

        let state_machine = init_state_machine(
            SignerState::RegisteredNotAbleToSign { epoch: Epoch(10) },
            runner,
        );

        state_machine
            .cycle()
            .await
            .expect("Cycling the state machine should not fail");
        assert_eq!(
            SignerState::RegisteredNotAbleToSign { epoch: Epoch(10) },
            state_machine.get_state().await
        );
    }

    #[tokio::test]
    async fn ready_to_sign_to_unregistered() {
        let mut runner = MockSignerRunner::new();
        runner
            .expect_get_current_time_point()
            .once()
            .returning(|| Ok(TimePoint::new(10, 100, ChainPoint::dummy())));
        runner
            .expect_update_era_checker()
            .once()
            .returning(|_e: Epoch| Ok(()));

        let state_machine =
            init_state_machine(SignerState::ReadyToSign { epoch: Epoch(0) }, runner);

        state_machine
            .cycle()
            .await
            .expect("Cycling the state machine should not fail");
        assert_eq!(
            SignerState::Unregistered { epoch: Epoch(10) },
            state_machine.get_state().await
        );
    }

    #[tokio::test]
    async fn ready_to_sign_to_ready_to_sign_when_there_is_a_beacon_to_sign() {
        let time_point = TimePoint::dummy();
        let beacon_to_sign = BeaconToSign {
            epoch: time_point.epoch,
            signed_entity_type: SignedEntityType::MithrilStakeDistribution(time_point.epoch),
            initiated_at: DateTime::default(),
        };
        let beacon_to_sign_clone = beacon_to_sign.clone();
        let current_epoch = time_point.epoch;

        let mut runner = MockSignerRunner::new();
        runner
            .expect_get_current_time_point()
            .once()
            .returning(move || Ok(time_point.to_owned()));
        runner
            .expect_get_beacon_to_sign()
            .once()
            .returning(move || Ok(Some(beacon_to_sign_clone.clone())));
        runner
            .expect_compute_message()
            .once()
            .returning(|_| Ok(ProtocolMessage::new()));
        runner
            .expect_compute_publish_single_signature()
            .once()
            .returning(|_, _| Ok(()));

        let state_machine = init_state_machine(
            SignerState::ReadyToSign {
                epoch: current_epoch,
            },
            runner,
        );
        state_machine
            .cycle()
            .await
            .expect("Cycling the state machine should not fail");

        assert_eq!(
            SignerState::ReadyToSign {
                epoch: current_epoch
            },
            state_machine.get_state().await,
            "state machine did not return a ReadyToSign state but {:?}",
            state_machine.get_state().await
        );
    }

    #[tokio::test]
    async fn ready_to_sign_to_ready_to_sign_when_there_no_beacon_to_sign() {
        let time_point = TimePoint::dummy();
        let current_epoch = time_point.epoch;

        let mut runner = MockSignerRunner::new();
        runner
            .expect_get_current_time_point()
            .once()
            .returning(move || Ok(time_point.to_owned()));
        runner
            .expect_get_beacon_to_sign()
            .once()
            .returning(move || Ok(None));

        let state_machine = init_state_machine(
            SignerState::ReadyToSign {
                epoch: current_epoch,
            },
            runner,
        );
        state_machine
            .cycle()
            .await
            .expect("Cycling the state machine should not fail");

        assert_eq!(
            SignerState::ReadyToSign {
                epoch: current_epoch
            },
            state_machine.get_state().await,
            "state machine did not return a ReadyToSign state but {:?}",
            state_machine.get_state().await
        );
    }
}
