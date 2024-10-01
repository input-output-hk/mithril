use slog_scope::{crit, debug, error, info};
use std::{fmt::Display, ops::Deref, sync::Arc, time::Duration};
use tokio::{sync::Mutex, time::sleep};

use mithril_common::{
    crypto_helper::ProtocolInitializerError,
    entities::{Epoch, SignedEntityType, TimePoint},
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
        /// Last signed entity type that the signer signed on this epoch.
        last_signed_entity_type: Option<SignedEntityType>,
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
            Self::ReadyToSign {
                epoch,
                last_signed_entity_type,
            } => {
                write!(f, "ReadyToSign - {epoch} - {last_signed_entity_type:?}")
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
}

enum ReadyToSignTransition {
    ToReadyToSign(BeaconToSign),
    NoTransitionAlreadySigned,
    NoTransitionCannotSignBeacon,
    NoTransitionNoBeaconToSign,
}

impl StateMachine {
    /// Create a new StateMachine instance.
    pub fn new(
        starting_state: SignerState,
        runner: Box<dyn Runner>,
        state_sleep: Duration,
        metrics_service: Arc<MetricsService>,
    ) -> Self {
        Self {
            state: Mutex::new(starting_state),
            runner,
            state_sleep,
            metrics_service,
        }
    }

    /// Return the current state of the state machine.
    pub async fn get_state(&self) -> SignerState {
        self.state.lock().await.to_owned()
    }

    /// Launch the state machine until an error occurs or it is interrupted.
    pub async fn run(&self) -> Result<(), RuntimeError> {
        info!("STATE MACHINE: launching");

        loop {
            if let Err(e) = self.cycle().await {
                if e.is_critical() {
                    crit!("{e}");

                    return Err(e);
                } else {
                    error!("{e}");
                }
            }

            info!(
                "… Cycle finished, Sleeping for {} ms",
                self.state_sleep.as_millis()
            );
            sleep(self.state_sleep).await;
        }
    }

    /// Perform a cycle of the state machine.
    pub async fn cycle(&self) -> Result<(), RuntimeError> {
        let mut state = self.state.lock().await;
        info!("================================================================================");
        info!("STATE MACHINE: new cycle: {}", *state);

        self.metrics_service
            .runtime_cycle_total_since_startup_counter_increment();

        match state.deref() {
            SignerState::Init => {
                *state = self.transition_from_init_to_unregistered().await?;
            }
            SignerState::Unregistered { epoch } => {
                if let Some(new_epoch) = self.has_epoch_changed(*epoch).await? {
                    info!("→ Epoch has changed, transiting to Unregistered");
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
                    info!("→ Epoch settings found");
                    if epoch_settings.epoch >= *epoch {
                        info!("new Epoch found");
                        info!(" ⋅ transiting to Registered");
                        *state = self
                            .transition_from_unregistered_to_one_of_registered_states(
                                epoch_settings,
                            )
                            .await?;
                    } else {
                        info!(
                            " ⋅ Epoch settings found, but its epoch is behind the known epoch, waiting…";
                            "epoch_settings" => ?epoch_settings,
                            "known_epoch" => ?epoch,
                        );
                    }
                } else {
                    info!("→ No epoch settings found yet, waiting…");
                }
            }
            SignerState::RegisteredNotAbleToSign { epoch } => {
                if let Some(new_epoch) = self.has_epoch_changed(*epoch).await? {
                    info!(" → new Epoch detected, transiting to Unregistered");
                    *state = self
                        .transition_from_registered_not_able_to_sign_to_unregistered(new_epoch)
                        .await?;
                } else {
                    info!(" ⋅ Epoch has NOT changed, waiting…");
                }
            }

            SignerState::ReadyToSign {
                epoch,
                last_signed_entity_type,
            } => match self.has_epoch_changed(*epoch).await? {
                Some(new_epoch) => {
                    info!("→ Epoch has changed, transiting to Unregistered");
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
                    if let ReadyToSignTransition::ToReadyToSign(beacon) = self
                        .ready_to_sign_certificate_next_transition(
                            beacon_to_sign,
                            last_signed_entity_type,
                        )
                        .await
                    {
                        *state = self
                            .transition_from_ready_to_sign_to_ready_to_sign(*epoch, beacon)
                            .await?;
                    }
                }
            },
        };

        self.metrics_service
            .runtime_cycle_success_since_startup_counter_increment();

        Ok(())
    }

    async fn ready_to_sign_certificate_next_transition(
        &self,
        beacon_to_sign: Option<BeaconToSign>,
        last_signed_entity: &Option<SignedEntityType>,
    ) -> ReadyToSignTransition {
        // Todo: remove those two checks are they are already done by the certifier
        fn is_same_signed_entity_type(
            signed_entity_type: &Option<SignedEntityType>,
            beacon_to_sign: &BeaconToSign,
        ) -> bool {
            Some(&beacon_to_sign.signed_entity_type) == signed_entity_type.as_ref()
        }

        async fn can_sign_signed_entity_type(s: &StateMachine, beacon: &BeaconToSign) -> bool {
            s.runner
                .can_sign_signed_entity_type(&beacon.signed_entity_type)
                .await
        }

        match beacon_to_sign {
            Some(beacon) if is_same_signed_entity_type(last_signed_entity, &beacon) => {
                info!(" ⋅ same entity type, already signed, waiting…");
                ReadyToSignTransition::NoTransitionAlreadySigned
            }
            Some(beacon) if can_sign_signed_entity_type(self, &beacon).await => {
                info!(
                    " ⋅ Epoch has NOT changed we can sign this beacon, transiting to ReadyToSign";
                    "beacon_to_sign" => ?beacon,
                );
                ReadyToSignTransition::ToReadyToSign(beacon)
            }
            Some(beacon) => {
                info!(
                    " ⋅ Epoch has NOT changed but cannot sign this beacon, waiting…";
                    "beacon_to_sign" => ?beacon
                );
                ReadyToSignTransition::NoTransitionCannotSignBeacon
            }
            None => {
                info!(" ⋅ no beacon to sign, waiting…");
                ReadyToSignTransition::NoTransitionNoBeaconToSign
            }
        }
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
            true => Ok(SignerState::ReadyToSign { epoch, last_signed_entity_type: None }),
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

        let single_signatures = self
            .runner
            .compute_single_signature(current_epoch, &message)
            .await
            .map_err(|e| RuntimeError::KeepState {
                message: format!("Could not compute single signature during 'ready to sign → ready to sign' phase (current epoch {current_epoch:?})"),
                nested_error: Some(e)
            })?;
        self.runner.send_single_signature(&beacon_to_sign.signed_entity_type, single_signatures, &message).await
            .map_err(|e| RuntimeError::KeepState {
                message: format!("Could not send single signature during 'ready to sign → ready to sign' phase (current epoch {current_epoch:?})"),
                nested_error: Some(e)
            })?;
        self.runner.mark_beacon_as_signed(&beacon_to_sign).await
            .map_err(|e| RuntimeError::KeepState {
                message: format!("Could not mark beacon as signed during 'ready to sign → ready to sign' phase (current epoch {current_epoch:?})"),
                nested_error: Some(e)
            })?;

        self.metrics_service
            .signature_registration_success_since_startup_counter_increment();
        self.metrics_service
            .signature_registration_success_last_epoch_gauge_set(current_epoch);

        Ok(SignerState::ReadyToSign {
            epoch: current_epoch,
            last_signed_entity_type: Some(beacon_to_sign.signed_entity_type),
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

    use mithril_common::entities::{ChainPoint, Epoch, ProtocolMessage};
    use mithril_common::test_utils::fake_data;

    use crate::runtime::runner::MockSignerRunner;

    use super::*;

    fn init_state_machine(init_state: SignerState, runner: MockSignerRunner) -> StateMachine {
        let metrics_service = Arc::new(MetricsService::new().unwrap());
        StateMachine {
            state: init_state.into(),
            runner: Box::new(runner),
            state_sleep: Duration::from_millis(100),
            metrics_service,
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
                last_signed_entity_type: None
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

        let state_machine = init_state_machine(
            SignerState::ReadyToSign {
                epoch: Epoch(0),
                last_signed_entity_type: None,
            },
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
    async fn ready_to_sign_to_ready_to_sign_when_signed_entity_type_is_locked() {
        let time_point = TimePoint {
            immutable_file_number: 99,
            epoch: Epoch(9),
            chain_point: ChainPoint::dummy(),
        };
        let state = SignerState::ReadyToSign {
            epoch: time_point.epoch,
            last_signed_entity_type: None,
        };

        let beacon_to_sign = BeaconToSign {
            epoch: time_point.epoch,
            signed_entity_type: SignedEntityType::MithrilStakeDistribution(time_point.epoch),
            initiated_at: DateTime::default(),
        };
        let time_point_clone = time_point.clone();
        let mut runner = MockSignerRunner::new();
        runner
            .expect_get_current_time_point()
            .once()
            .returning(move || Ok(time_point_clone.to_owned()));
        runner
            .expect_get_beacon_to_sign()
            .once()
            .returning(move || Ok(Some(beacon_to_sign.to_owned())));
        runner
            .expect_can_sign_signed_entity_type()
            .once()
            .returning(move |_| false);

        let state_machine = init_state_machine(state, runner);
        state_machine
            .cycle()
            .await
            .expect("Cycling the state machine should not fail");

        assert_eq!(
            SignerState::ReadyToSign {
                epoch: time_point.epoch,
                last_signed_entity_type: None
            },
            state_machine.get_state().await,
            "state machine did not return a ReadyToSign but {:?}",
            state_machine.get_state().await
        );
    }

    #[tokio::test]
    async fn ready_to_sign_to_ready_to_sign_when_same_signed_entity_type() {
        let time_point = TimePoint::dummy();
        let beacon_to_sign = BeaconToSign {
            epoch: time_point.epoch,
            signed_entity_type: SignedEntityType::CardanoStakeDistribution(time_point.epoch),
            initiated_at: DateTime::default(),
        };
        let beacon_to_sign_clone = beacon_to_sign.clone();
        let state = SignerState::ReadyToSign {
            epoch: time_point.epoch,
            last_signed_entity_type: Some(beacon_to_sign.signed_entity_type.clone()),
        };
        let time_point_clone = time_point.clone();
        let mut runner = MockSignerRunner::new();
        runner
            .expect_get_current_time_point()
            .returning(move || Ok(time_point_clone.to_owned()));
        runner
            .expect_get_beacon_to_sign()
            .times(1)
            .returning(move || Ok(Some(beacon_to_sign.clone())));
        runner
            .expect_can_sign_signed_entity_type()
            .returning(move |_| true);

        let state_machine = init_state_machine(state, runner);
        state_machine
            .cycle()
            .await
            .expect("Cycling the state machine should not fail");

        assert_eq!(
            SignerState::ReadyToSign {
                epoch: time_point.epoch,
                last_signed_entity_type: Some(beacon_to_sign_clone.signed_entity_type)
            },
            state_machine.get_state().await,
            "state machine did not return a ReadyToSign state but {:?}",
            state_machine.get_state().await
        );
    }

    #[tokio::test]
    async fn ready_to_sign_to_ready_to_sign_when_can_sign_the_first_signed_entity_type() {
        let time_point = TimePoint::dummy();
        let state = SignerState::ReadyToSign {
            epoch: time_point.epoch,
            last_signed_entity_type: None,
        };
        let beacon_to_sign = BeaconToSign {
            epoch: time_point.epoch,
            signed_entity_type: SignedEntityType::MithrilStakeDistribution(time_point.epoch),
            initiated_at: Default::default(),
        };
        assert_ready_to_sign_to_ready_to_sign_when_different_state_than_previous_one(
            state,
            beacon_to_sign,
        )
        .await
    }

    #[tokio::test]
    async fn ready_to_sign_to_ready_to_sign_when_different_state_than_previous_one() {
        let time_point = TimePoint::dummy();
        let state = SignerState::ReadyToSign {
            epoch: time_point.epoch,
            last_signed_entity_type: Some(SignedEntityType::CardanoStakeDistribution(
                time_point.epoch,
            )),
        };
        let beacon_to_sign = BeaconToSign {
            epoch: time_point.epoch,
            signed_entity_type: SignedEntityType::MithrilStakeDistribution(time_point.epoch),
            initiated_at: Default::default(),
        };

        assert_ready_to_sign_to_ready_to_sign_when_different_state_than_previous_one(
            state,
            beacon_to_sign,
        )
        .await
    }

    #[tokio::test]
    async fn ready_to_sign_to_ready_to_sign_when_different_state_than_previous_return_state_with_same_epoch(
    ) {
        let time_point = TimePoint::dummy();
        let state = SignerState::ReadyToSign {
            epoch: time_point.epoch,
            last_signed_entity_type: Some(SignedEntityType::CardanoStakeDistribution(
                time_point.epoch,
            )),
        };
        let beacon_to_sign = BeaconToSign {
            epoch: time_point.epoch + 10, // Check that the epoch
            signed_entity_type: SignedEntityType::MithrilStakeDistribution(time_point.epoch),
            initiated_at: Default::default(),
        };

        assert_ready_to_sign_to_ready_to_sign_when_different_state_than_previous_one(
            state,
            beacon_to_sign,
        )
        .await
    }

    async fn assert_ready_to_sign_to_ready_to_sign_when_different_state_than_previous_one(
        initial_state: SignerState,
        beacon_to_sign: BeaconToSign,
    ) {
        let initial_state_epoch = match initial_state.clone() {
            SignerState::ReadyToSign {
                epoch,
                last_signed_entity_type: _,
            } => epoch,
            _ => panic!("Invalid state, use only ReadyToSign"),
        };
        let time_point = TimePoint {
            epoch: initial_state_epoch,
            ..TimePoint::dummy()
        };

        let beacon_to_sign_clone = beacon_to_sign.clone();
        let mut runner = MockSignerRunner::new();
        runner
            .expect_get_current_time_point()
            .once()
            .returning(move || Ok(time_point.to_owned()));
        runner
            .expect_get_beacon_to_sign()
            .once()
            .returning(move || Ok(Some(beacon_to_sign.clone())));
        runner
            .expect_compute_single_signature()
            .once()
            .returning(|_, _| Ok(Some(fake_data::single_signatures(vec![1, 5, 23]))));
        runner
            .expect_compute_message()
            .once()
            .returning(|_| Ok(ProtocolMessage::new()));
        runner
            .expect_send_single_signature()
            .once()
            .returning(|_, _, _| Ok(()));
        runner
            .expect_mark_beacon_as_signed()
            .once()
            .returning(|_| Ok(()));
        runner
            .expect_can_sign_signed_entity_type()
            .once()
            .returning(move |_| true);

        let state_machine = init_state_machine(initial_state.clone(), runner);
        state_machine
            .cycle()
            .await
            .expect("Cycling the state machine should not fail");

        assert_eq!(
            SignerState::ReadyToSign {
                epoch: initial_state_epoch,
                last_signed_entity_type: Some(beacon_to_sign_clone.signed_entity_type)
            },
            state_machine.get_state().await,
            "state machine did not return a ReadyToSign state but {:?}",
            state_machine.get_state().await
        );
    }
}
