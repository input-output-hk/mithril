use anyhow::Context;
use chrono::Local;
use slog::{info, trace, Logger};
use std::fmt::Display;
use std::sync::Arc;

use mithril_common::entities::TimePoint;
use mithril_common::logging::LoggerExtensions;

use crate::entities::OpenMessage;
use crate::runtime::{AggregatorRunnerTrait, RuntimeError};
use crate::AggregatorConfig;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct IdleState {
    current_time_point: Option<TimePoint>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ReadyState {
    current_time_point: TimePoint,
}

#[derive(Clone, Debug, PartialEq)]
pub struct SigningState {
    current_time_point: TimePoint,
    open_message: OpenMessage,
}

#[derive(Clone, Debug, PartialEq)]
pub enum AggregatorState {
    Idle(IdleState),
    Ready(ReadyState),
    Signing(SigningState),
}

impl Display for AggregatorState {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            AggregatorState::Idle(state) => write!(
                f,
                "Idle - {}",
                match &state.current_time_point {
                    None => "No TimePoint".to_string(),
                    Some(time_point) => time_point.to_string(),
                }
            ),
            AggregatorState::Ready(state) => write!(f, "Ready - {}", state.current_time_point),
            AggregatorState::Signing(state) => write!(f, "Signing - {}", state.current_time_point),
        }
    }
}

/// The AggregatorRuntime responsibility is to create a state machine to handle
/// all actions required by the process of getting multi-signatures.
/// See the
/// [documentation](https://mithril.network/doc/mithril/mithril-network/aggregator#under-the-hood)
/// for more explanations about the Aggregator state machine.
pub struct AggregatorRuntime {
    config: AggregatorConfig,
    state: AggregatorState,
    runner: Arc<dyn AggregatorRunnerTrait>,
    logger: Logger,
}

impl AggregatorRuntime {
    /// Create a new instance of the state machine.
    pub async fn new(
        aggregator_config: AggregatorConfig,
        init_state: Option<AggregatorState>,
        runner: Arc<dyn AggregatorRunnerTrait>,
        logger: Logger,
    ) -> Result<Self, RuntimeError> {
        let logger = logger.new_with_component_name::<Self>();
        info!(logger, "Initializing runtime");

        let state = if let Some(init_state) = init_state {
            trace!(logger, "Got initial state from caller");
            init_state
        } else {
            trace!(logger, "Idle state, no current time point");
            AggregatorState::Idle(IdleState {
                current_time_point: None,
            })
        };

        Ok(Self {
            config: aggregator_config,
            state,
            runner,
            logger,
        })
    }

    /// Return the actual state of the state machine.
    pub fn get_state(&self) -> String {
        match self.state {
            AggregatorState::Idle(_) => "idle".to_string(),
            AggregatorState::Ready(_) => "ready".to_string(),
            AggregatorState::Signing(_) => "signing".to_string(),
        }
    }

    /// Launches an infinite loop ticking the state machine.
    pub async fn run(&mut self) -> Result<(), RuntimeError> {
        info!(self.logger, "Launching State Machine");
        let mut interval = tokio::time::interval(self.config.interval);

        loop {
            interval.tick().await;
            // Note: the "time" property in logs produced by our formatter (slog_bunyan) uses local
            // time, so we must use it as well to avoid confusion.
            let approximate_next_cycle_time = Local::now() + self.config.interval;

            if let Err(e) = self.cycle().await {
                e.write_to_log(&self.logger);
                if e.is_critical() {
                    return Err(e);
                }
            }

            info!(
                self.logger, "… Cycle finished";
                "approximate_next_cycle_time" => %approximate_next_cycle_time.time().format("%H:%M:%S%.3f"),
                "run_interval_in_ms" => self.config.interval.as_millis(),
            );
        }
    }

    /// Perform one tick of the state machine.
    pub async fn cycle(&mut self) -> Result<(), RuntimeError> {
        info!(
            self.logger,
            "================================================================================"
        );
        info!(self.logger, "New cycle: {}", self.state);

        self.runner.increment_runtime_cycle_total_since_startup_counter();

        match self.state.clone() {
            AggregatorState::Idle(state) => {
                let last_time_point = self.runner.get_time_point_from_chain().await.with_context(
                    || "AggregatorRuntime in the state IDLE can not get current time point from chain",
                )?;

                info!(self.logger, "→ Trying to transition to READY"; "last_time_point" => ?last_time_point);

                let can_try_transition_from_idle_to_ready = if self.config.is_follower {
                    self.runner
                        .is_follower_aggregator_at_same_epoch_as_leader(&last_time_point)
                        .await?
                } else {
                    true
                };
                if can_try_transition_from_idle_to_ready {
                    self.try_transition_from_idle_to_ready(
                        state.current_time_point,
                        last_time_point.clone(),
                    )
                    .await?;
                    self.state = AggregatorState::Ready(ReadyState {
                        current_time_point: last_time_point,
                    });
                }
            }
            AggregatorState::Ready(state) => {
                let last_time_point: TimePoint = self
                    .runner
                    .get_time_point_from_chain()
                    .await
                    .with_context(|| {
                        "AggregatorRuntime in the state READY can not get current time point from chain"
                    })?;

                if state.current_time_point.epoch < last_time_point.epoch {
                    // transition READY > IDLE
                    info!(self.logger, "→ Epoch has changed, transitioning to IDLE"; "last_time_point" => ?last_time_point);
                    self.state = AggregatorState::Idle(IdleState {
                        current_time_point: Some(state.current_time_point),
                    });
                } else if let Some(open_message) = self
                    .runner
                    .get_current_non_certified_open_message(&last_time_point)
                    .await
                    .with_context(|| "AggregatorRuntime can not get the current open message")?
                {
                    // transition READY > SIGNING
                    info!(self.logger, "→ Transitioning to SIGNING");
                    let new_state = self
                        .transition_from_ready_to_signing(last_time_point.clone(), open_message.clone())
                        .await.with_context(|| format!("AggregatorRuntime can not perform a transition from READY state to SIGNING with entity_type: '{:?}'", open_message.signed_entity_type))?;
                    self.state = AggregatorState::Signing(new_state);
                } else {
                    // READY > READY
                    info!(
                        self.logger, " ⋅ No open message to certify, waiting…";
                        "time_point" => ?state.current_time_point
                    );
                    self.state = AggregatorState::Ready(ReadyState {
                        current_time_point: last_time_point,
                    });
                }
            }
            AggregatorState::Signing(state) => {
                let last_time_point: TimePoint =
                    self.runner.get_time_point_from_chain().await.with_context(|| {
                        "AggregatorRuntime in the state SIGNING can not get current time point from chain"
                    })?;

                let is_outdated = self
                    .runner
                    .is_open_message_outdated(
                        state.open_message.signed_entity_type.clone(),
                        &last_time_point,
                    )
                    .await?;

                if state.current_time_point.epoch < last_time_point.epoch {
                    // SIGNING > IDLE
                    info!(self.logger, "→ Epoch changed, transitioning to IDLE");
                    let new_state = self.transition_from_signing_to_idle(state).await?;
                    self.state = AggregatorState::Idle(new_state);
                } else if is_outdated {
                    // SIGNING > READY
                    info!(
                        self.logger,
                        "→ Open message changed, transitioning to READY"
                    );
                    let new_state =
                        self.transition_from_signing_to_ready_new_open_message(state).await?;
                    self.state = AggregatorState::Ready(new_state);
                } else {
                    // SIGNING > READY
                    let new_state =
                        self.transition_from_signing_to_ready_multisignature(state).await?;
                    info!(self.logger, "→ A multi-signature has been created, build an artifact & a certificate and transitioning back to READY");
                    self.state = AggregatorState::Ready(new_state);
                }
            }
        }

        self.runner.increment_runtime_cycle_success_since_startup_counter();

        Ok(())
    }

    /// Perform a transition from `IDLE` state to `READY` state when
    /// the certificate chain is valid.
    async fn try_transition_from_idle_to_ready(
        &mut self,
        maybe_current_time_point: Option<TimePoint>,
        new_time_point: TimePoint,
    ) -> Result<(), RuntimeError> {
        trace!(self.logger, "Trying transition from IDLE to READY state");

        if maybe_current_time_point.is_none()
            || maybe_current_time_point.as_ref().unwrap().epoch < new_time_point.epoch
        {
            self.runner.close_signer_registration_round().await?;
            self.runner
                .update_era_checker(new_time_point.epoch)
                .await
                .map_err(|e| RuntimeError::critical("transiting IDLE → READY", Some(e)))?;
            self.runner.update_stake_distribution(&new_time_point).await?;
            self.runner.inform_new_epoch(new_time_point.epoch).await?;
            self.runner.upkeep(new_time_point.epoch).await?;
            self.runner.open_signer_registration_round(&new_time_point).await?;
            self.runner.update_epoch_settings().await?;
            if self.config.is_follower {
                self.runner
                    .synchronize_follower_aggregator_signer_registration()
                    .await?;
            }
            self.runner.precompute_epoch_data().await?;
        }

        self.runner
            .is_certificate_chain_valid(&new_time_point)
            .await
            .map_err(|e| RuntimeError::KeepState {
                message: "certificate chain is invalid".to_string(),
                nested_error: e.into(),
            })?;

        Ok(())
    }

    /// Perform a transition from `SIGNING` state to `READY` state when a new
    /// multi-signature is issued.
    async fn transition_from_signing_to_ready_multisignature(
        &self,
        state: SigningState,
    ) -> Result<ReadyState, RuntimeError> {
        trace!(
            self.logger,
            "Launching transition from SIGNING to READY state"
        );
        let certificate = self
            .runner
            .create_certificate(&state.open_message.signed_entity_type)
            .await?
            .ok_or_else(|| RuntimeError::KeepState {
                message: "not enough signature yet to create a certificate, waiting…".to_string(),
                nested_error: None,
            })?;
        self.runner
            .create_artifact(&state.open_message.signed_entity_type, &certificate)
            .await
            .map_err(|e| RuntimeError::ReInit {
                message: "transiting SIGNING → READY: failed to create artifact. Retrying…"
                    .to_string(),
                nested_error: Some(e),
            })?;

        Ok(ReadyState {
            current_time_point: state.current_time_point,
        })
    }

    /// Perform a transition from `SIGNING` state to `IDLE` state when a new
    /// epoch is detected.
    async fn transition_from_signing_to_idle(
        &self,
        state: SigningState,
    ) -> Result<IdleState, RuntimeError> {
        trace!(
            self.logger,
            "Launching transition from SIGNING to IDLE state"
        );

        Ok(IdleState {
            current_time_point: Some(state.current_time_point),
        })
    }

    /// Perform a transition from `SIGNING` state to `READY` state when a new
    /// open message is detected.
    async fn transition_from_signing_to_ready_new_open_message(
        &self,
        state: SigningState,
    ) -> Result<ReadyState, RuntimeError> {
        trace!(
            self.logger,
            "Launching transition from SIGNING to READY state"
        );

        Ok(ReadyState {
            current_time_point: state.current_time_point,
        })
    }

    /// Perform a transition from `READY` state to `SIGNING` state when a new
    /// open message is opened.
    async fn transition_from_ready_to_signing(
        &mut self,
        new_time_point: TimePoint,
        open_message: OpenMessage,
    ) -> Result<SigningState, RuntimeError> {
        trace!(
            self.logger,
            "Launching transition from READY to SIGNING state"
        );

        let state = SigningState {
            current_time_point: new_time_point,
            open_message,
        };

        Ok(state)
    }
}

#[cfg(test)]
mod tests {
    use crate::entities::OpenMessage;
    use anyhow::anyhow;
    use mockall::predicate;
    use std::time::Duration;

    use mithril_common::test_utils::fake_data;

    use crate::test_tools::TestLogger;

    use super::super::runner::MockAggregatorRunner;
    use super::*;

    async fn init_runtime(
        init_state: Option<AggregatorState>,
        runner: MockAggregatorRunner,
        is_follower: bool,
    ) -> AggregatorRuntime {
        AggregatorRuntime::new(
            AggregatorConfig::new(Duration::from_millis(20), is_follower),
            init_state,
            Arc::new(runner),
            TestLogger::stdout(),
        )
        .await
        .unwrap()
    }

    mod leader {
        use super::*;

        #[tokio::test]
        pub async fn idle_check_certificate_chain_is_not_valid() {
            let mut runner = MockAggregatorRunner::new();
            runner
                .expect_get_time_point_from_chain()
                .once()
                .returning(|| Ok(TimePoint::dummy()));
            runner
                .expect_update_stake_distribution()
                .with(predicate::eq(TimePoint::dummy()))
                .once()
                .returning(|_| Ok(()));
            runner
                .expect_close_signer_registration_round()
                .once()
                .returning(|| Ok(()));
            runner
                .expect_open_signer_registration_round()
                .once()
                .returning(|_| Ok(()));
            runner
                .expect_is_certificate_chain_valid()
                .once()
                .returning(|_| Err(anyhow!("error")));
            runner
                .expect_update_era_checker()
                .with(predicate::eq(TimePoint::dummy().epoch))
                .once()
                .returning(|_| Ok(()));
            runner
                .expect_inform_new_epoch()
                .with(predicate::eq(TimePoint::dummy().epoch))
                .once()
                .returning(|_| Ok(()));
            runner.expect_update_epoch_settings().once().returning(|| Ok(()));
            runner.expect_precompute_epoch_data().once().returning(|| Ok(()));
            runner
                .expect_upkeep()
                .with(predicate::eq(TimePoint::dummy().epoch))
                .once()
                .returning(|_| Ok(()));
            runner
                .expect_increment_runtime_cycle_total_since_startup_counter()
                .once()
                .returning(|| ());
            runner
                .expect_increment_runtime_cycle_success_since_startup_counter()
                .never();

            let mut runtime = init_runtime(
                Some(AggregatorState::Idle(IdleState {
                    current_time_point: None,
                })),
                runner,
                false,
            )
            .await;
            let err = runtime.cycle().await.unwrap_err();
            assert!(matches!(err, RuntimeError::KeepState { .. }));

            assert_eq!("idle".to_string(), runtime.get_state());
        }

        #[tokio::test]
        pub async fn idle_check_certificate_chain_is_valid() {
            let mut runner = MockAggregatorRunner::new();
            runner
                .expect_get_time_point_from_chain()
                .once()
                .returning(|| Ok(TimePoint::dummy()));
            runner
                .expect_update_stake_distribution()
                .with(predicate::eq(TimePoint::dummy()))
                .once()
                .returning(|_| Ok(()));
            runner
                .expect_close_signer_registration_round()
                .once()
                .returning(|| Ok(()));
            runner
                .expect_open_signer_registration_round()
                .once()
                .returning(|_| Ok(()));
            runner
                .expect_is_certificate_chain_valid()
                .once()
                .returning(|_| Ok(()));
            runner
                .expect_update_era_checker()
                .with(predicate::eq(TimePoint::dummy().epoch))
                .once()
                .returning(|_| Ok(()));
            runner
                .expect_inform_new_epoch()
                .with(predicate::eq(TimePoint::dummy().epoch))
                .once()
                .returning(|_| Ok(()));
            runner.expect_update_epoch_settings().once().returning(|| Ok(()));
            runner.expect_precompute_epoch_data().once().returning(|| Ok(()));
            runner
                .expect_upkeep()
                .with(predicate::eq(TimePoint::dummy().epoch))
                .once()
                .returning(|_| Ok(()));
            runner
                .expect_increment_runtime_cycle_total_since_startup_counter()
                .once()
                .returning(|| ());
            runner
                .expect_increment_runtime_cycle_success_since_startup_counter()
                .once()
                .returning(|| ());

            let mut runtime = init_runtime(
                Some(AggregatorState::Idle(IdleState {
                    current_time_point: None,
                })),
                runner,
                false,
            )
            .await;
            runtime.cycle().await.unwrap();

            assert_eq!("ready".to_string(), runtime.get_state());
        }

        #[tokio::test]
        pub async fn ready_new_epoch_detected() {
            let mut runner = MockAggregatorRunner::new();
            let time_point = TimePoint::dummy();
            let new_time_point = TimePoint {
                epoch: time_point.epoch + 1,
                ..time_point.clone()
            };
            runner
                .expect_get_time_point_from_chain()
                .once()
                .returning(move || Ok(new_time_point.clone()));
            runner
                .expect_increment_runtime_cycle_total_since_startup_counter()
                .once()
                .returning(|| ());
            runner
                .expect_increment_runtime_cycle_success_since_startup_counter()
                .once()
                .returning(|| ());
            let mut runtime = init_runtime(
                Some(AggregatorState::Ready(ReadyState {
                    current_time_point: time_point,
                })),
                runner,
                false,
            )
            .await;
            runtime.cycle().await.unwrap();

            assert_eq!("idle".to_string(), runtime.get_state());
        }

        #[tokio::test]
        pub async fn ready_open_message_not_exist() {
            let mut runner = MockAggregatorRunner::new();
            let time_point = TimePoint::dummy();
            let next_time_point = TimePoint {
                immutable_file_number: time_point.immutable_file_number + 1,
                ..time_point.clone()
            };
            let expected_time_point = next_time_point.clone();
            runner
                .expect_get_time_point_from_chain()
                .once()
                .returning(move || Ok(next_time_point.clone()));
            runner
                .expect_get_current_non_certified_open_message()
                .once()
                .returning(|_| Ok(None));
            runner
                .expect_increment_runtime_cycle_total_since_startup_counter()
                .once()
                .returning(|| ());
            runner
                .expect_increment_runtime_cycle_success_since_startup_counter()
                .once()
                .returning(|| ());
            let mut runtime = init_runtime(
                Some(AggregatorState::Ready(ReadyState {
                    current_time_point: time_point.clone(),
                })),
                runner,
                false,
            )
            .await;
            runtime.cycle().await.unwrap();

            assert_eq!("ready".to_string(), runtime.get_state());
            assert_eq!(
                AggregatorState::Ready(ReadyState {
                    current_time_point: expected_time_point,
                }),
                runtime.state
            );
        }

        #[tokio::test]
        pub async fn ready_certificate_does_not_exist_for_time_point() {
            let mut runner = MockAggregatorRunner::new();
            runner
                .expect_get_time_point_from_chain()
                .once()
                .returning(|| Ok(TimePoint::dummy()));
            runner
                .expect_get_current_non_certified_open_message()
                .once()
                .returning(|_| {
                    let open_message = OpenMessage {
                        is_certified: false,
                        ..OpenMessage::dummy()
                    };
                    Ok(Some(open_message))
                });
            runner
                .expect_increment_runtime_cycle_total_since_startup_counter()
                .once()
                .returning(|| ());
            runner
                .expect_increment_runtime_cycle_success_since_startup_counter()
                .once()
                .returning(|| ());

            let mut runtime = init_runtime(
                Some(AggregatorState::Ready(ReadyState {
                    current_time_point: TimePoint::dummy(),
                })),
                runner,
                false,
            )
            .await;
            runtime.cycle().await.unwrap();

            assert_eq!("signing".to_string(), runtime.get_state());
        }

        #[tokio::test]
        async fn signing_changing_open_message_to_ready() {
            let mut runner = MockAggregatorRunner::new();
            runner
                .expect_get_time_point_from_chain()
                .once()
                .returning(|| Ok(TimePoint::dummy()));
            runner
                .expect_is_open_message_outdated()
                .once()
                .returning(|_, _| Ok(true));
            runner
                .expect_increment_runtime_cycle_total_since_startup_counter()
                .once()
                .returning(|| ());
            runner
                .expect_increment_runtime_cycle_success_since_startup_counter()
                .once()
                .returning(|| ());

            let initial_state = AggregatorState::Signing(SigningState {
                current_time_point: TimePoint::dummy(),
                open_message: OpenMessage::dummy(),
            });

            let mut runtime = init_runtime(Some(initial_state), runner, false).await;
            runtime.cycle().await.unwrap();

            assert_eq!("ready".to_string(), runtime.get_state());
        }

        #[tokio::test]
        async fn signing_certificate_is_not_created() {
            let mut runner = MockAggregatorRunner::new();
            runner
                .expect_get_time_point_from_chain()
                .once()
                .returning(|| Ok(TimePoint::dummy()));
            runner
                .expect_is_open_message_outdated()
                .once()
                .returning(|_, _| Ok(false));
            runner.expect_create_certificate().once().returning(|_| Ok(None));
            runner
                .expect_increment_runtime_cycle_total_since_startup_counter()
                .once()
                .returning(|| ());
            runner
                .expect_increment_runtime_cycle_success_since_startup_counter()
                .never();
            let state = SigningState {
                current_time_point: TimePoint::dummy(),
                open_message: OpenMessage::dummy(),
            };
            let mut runtime =
                init_runtime(Some(AggregatorState::Signing(state)), runner, false).await;
            let err = runtime
                .cycle()
                .await
                .expect_err("cycle should have returned an error");

            match err {
                RuntimeError::KeepState { .. } => (),
                _ => panic!("KeepState error expected, got {err:?}."),
            };

            assert_eq!("signing".to_string(), runtime.get_state());
        }

        #[tokio::test]
        async fn signing_artifact_not_created() {
            let mut runner = MockAggregatorRunner::new();
            runner
                .expect_get_time_point_from_chain()
                .once()
                .returning(|| Ok(TimePoint::dummy()));
            runner
                .expect_is_open_message_outdated()
                .once()
                .returning(|_, _| Ok(false));
            runner
                .expect_create_certificate()
                .return_once(move |_| Ok(Some(fake_data::certificate("whatever".to_string()))));
            runner
                .expect_create_artifact()
                .once()
                .returning(|_, _| Err(anyhow!("whatever")));
            runner
                .expect_increment_runtime_cycle_total_since_startup_counter()
                .once()
                .returning(|| ());
            runner
                .expect_increment_runtime_cycle_success_since_startup_counter()
                .never();
            let state = SigningState {
                current_time_point: TimePoint::dummy(),
                open_message: OpenMessage::dummy(),
            };
            let mut runtime =
                init_runtime(Some(AggregatorState::Signing(state)), runner, false).await;
            let err = runtime
                .cycle()
                .await
                .expect_err("cycle should have returned an error");

            match err {
                RuntimeError::ReInit { .. } => (),
                _ => panic!("ReInit error expected, got {err:?}."),
            };

            assert_eq!("signing".to_string(), runtime.get_state());
        }

        #[tokio::test]
        async fn signing_certificate_is_created() {
            let mut runner = MockAggregatorRunner::new();
            runner
                .expect_get_time_point_from_chain()
                .once()
                .returning(|| Ok(TimePoint::dummy()));
            runner
                .expect_is_open_message_outdated()
                .once()
                .returning(|_, _| Ok(false));
            runner
                .expect_create_certificate()
                .return_once(move |_| Ok(Some(fake_data::certificate("whatever".to_string()))));
            runner.expect_create_artifact().once().returning(|_, _| Ok(()));
            runner
                .expect_increment_runtime_cycle_total_since_startup_counter()
                .once()
                .returning(|| ());
            runner
                .expect_increment_runtime_cycle_success_since_startup_counter()
                .once()
                .returning(|| ());

            let state = SigningState {
                current_time_point: TimePoint::dummy(),
                open_message: OpenMessage::dummy(),
            };
            let mut runtime =
                init_runtime(Some(AggregatorState::Signing(state)), runner, false).await;
            runtime.cycle().await.unwrap();

            assert_eq!("ready".to_string(), runtime.get_state());
        }

        #[tokio::test]
        pub async fn critical_error() {
            let mut runner = MockAggregatorRunner::new();
            runner
                .expect_get_time_point_from_chain()
                .once()
                .returning(|| Ok(TimePoint::dummy()));
            runner
                .expect_update_era_checker()
                .with(predicate::eq(TimePoint::dummy().epoch))
                .once()
                .returning(|_| Err(anyhow!("ERROR")));
            runner
                .expect_close_signer_registration_round()
                .once()
                .returning(|| Ok(()));
            runner
                .expect_increment_runtime_cycle_total_since_startup_counter()
                .once()
                .returning(|| ());
            runner
                .expect_increment_runtime_cycle_success_since_startup_counter()
                .never();

            let mut runtime = init_runtime(
                Some(AggregatorState::Idle(IdleState {
                    current_time_point: None,
                })),
                runner,
                false,
            )
            .await;
            runtime.cycle().await.unwrap_err();

            assert_eq!("idle".to_string(), runtime.get_state());
        }
    }

    mod follower {
        use super::*;

        #[tokio::test]
        pub async fn idle_new_epoch_detected_and_leader_not_transitioned_to_epoch() {
            let mut runner = MockAggregatorRunner::new();
            let time_point = TimePoint::dummy();
            let new_time_point = TimePoint {
                epoch: time_point.epoch + 1,
                ..time_point.clone()
            };
            runner
                .expect_get_time_point_from_chain()
                .once()
                .returning(move || Ok(new_time_point.clone()));
            runner
                .expect_is_follower_aggregator_at_same_epoch_as_leader()
                .once()
                .returning(|_| Ok(false));
            runner
                .expect_increment_runtime_cycle_total_since_startup_counter()
                .once()
                .returning(|| ());
            runner
                .expect_increment_runtime_cycle_success_since_startup_counter()
                .once()
                .returning(|| ());
            let mut runtime = init_runtime(
                Some(AggregatorState::Idle(IdleState {
                    current_time_point: Some(time_point),
                })),
                runner,
                true,
            )
            .await;
            runtime.cycle().await.unwrap();

            assert_eq!("idle".to_string(), runtime.get_state());
        }

        #[tokio::test]
        pub async fn idle_new_epoch_detected_and_leader_has_transitioned_to_epoch() {
            let mut runner = MockAggregatorRunner::new();
            let time_point = TimePoint::dummy();
            let new_time_point = TimePoint {
                epoch: time_point.epoch + 1,
                ..time_point.clone()
            };
            let new_time_point_clone = new_time_point.clone();
            runner
                .expect_get_time_point_from_chain()
                .once()
                .returning(move || Ok(new_time_point.clone()));
            runner
                .expect_is_follower_aggregator_at_same_epoch_as_leader()
                .once()
                .returning(|_| Ok(true));
            runner
                .expect_update_stake_distribution()
                .with(predicate::eq(new_time_point_clone.clone()))
                .once()
                .returning(|_| Ok(()));
            runner
                .expect_close_signer_registration_round()
                .once()
                .returning(|| Ok(()));
            runner
                .expect_synchronize_follower_aggregator_signer_registration()
                .once()
                .returning(|| Ok(()));
            runner
                .expect_open_signer_registration_round()
                .once()
                .returning(|_| Ok(()));
            runner
                .expect_is_certificate_chain_valid()
                .once()
                .returning(|_| Ok(()));
            runner
                .expect_update_era_checker()
                .with(predicate::eq(new_time_point_clone.clone().epoch))
                .once()
                .returning(|_| Ok(()));
            runner
                .expect_inform_new_epoch()
                .with(predicate::eq(new_time_point_clone.clone().epoch))
                .once()
                .returning(|_| Ok(()));
            runner.expect_update_epoch_settings().once().returning(|| Ok(()));
            runner.expect_precompute_epoch_data().once().returning(|| Ok(()));
            runner
                .expect_upkeep()
                .with(predicate::eq(new_time_point_clone.clone().epoch))
                .once()
                .returning(|_| Ok(()));
            runner
                .expect_increment_runtime_cycle_total_since_startup_counter()
                .once()
                .returning(|| ());
            runner
                .expect_increment_runtime_cycle_success_since_startup_counter()
                .once()
                .returning(|| ());
            let mut runtime = init_runtime(
                Some(AggregatorState::Idle(IdleState {
                    current_time_point: Some(time_point),
                })),
                runner,
                true,
            )
            .await;
            runtime.cycle().await.unwrap();

            assert_eq!("ready".to_string(), runtime.get_state());
        }
    }
}
