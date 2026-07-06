use anyhow::Context;
use chrono::Local;
use slog::{Logger, info, trace};
use std::fmt::Display;
use std::sync::Arc;

use mithril_common::StdResult;
use mithril_common::entities::{Epoch, TimePoint};
use mithril_common::logging::LoggerExtensions;

use crate::AggregatorConfig;
use crate::entities::OpenMessage;
use crate::runtime::{AggregatorRunnerTrait, RuntimeError};
use crate::services::CertifierServiceError;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct IdleState {
    current_time_point: Option<TimePoint>,
}

impl IdleState {
    pub fn should_run_epoch_initialization_tasks(&self, new_time_point: &TimePoint) -> bool {
        self.current_time_point
            .as_ref()
            .is_none_or(|time_point| time_point.epoch < new_time_point.epoch)
    }
}

/// State when the state machine can't proceed to the next state before the next epoch.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct BlockedState {
    blocked_since_time_point: TimePoint,
    reason: BlockedReason,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum BlockedReason {
    /// No genesis certificate has been issued yet.
    NoGenesis,
    /// The genesis certificate has been issued in the current epoch.
    GenesisEpoch,
    /// There's an epoch gap between the current epoch and the one of the last certificate.
    EpochGap { epoch_of_last_certificate: Epoch },
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
    Blocked(BlockedState),
    Ready(ReadyState),
    Signing(SigningState),
}

impl Display for AggregatorState {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            AggregatorState::Idle(state) => match &state.current_time_point {
                None => write!(f, "Idle - No TimePoint"),
                Some(time_point) => write!(f, "Idle - {time_point}"),
            },
            AggregatorState::Blocked(state) => match state.reason {
                BlockedReason::NoGenesis => {
                    write!(
                        f,
                        "Blocked - No Genesis - {}",
                        state.blocked_since_time_point
                    )
                }
                BlockedReason::GenesisEpoch => {
                    write!(
                        f,
                        "Blocked - Genesis Epoch - {}",
                        state.blocked_since_time_point
                    )
                }
                BlockedReason::EpochGap {
                    epoch_of_last_certificate,
                } => write!(
                    f,
                    "Blocked - Epoch Gap [{epoch_of_last_certificate}] - {}",
                    state.blocked_since_time_point
                ),
            },
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

    /// Return the label of the actual state of the state machine.
    pub fn state_label(&self) -> &'static str {
        match &self.state {
            AggregatorState::Idle(_) => "idle",
            AggregatorState::Blocked(state) => match state.reason {
                BlockedReason::NoGenesis => "blocked-no-genesis",
                BlockedReason::GenesisEpoch => "blocked-genesis-epoch",
                BlockedReason::EpochGap { .. } => "blocked-epoch-gap",
            },
            AggregatorState::Ready(_) => "ready",
            AggregatorState::Signing(_) => "signing",
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

        let next_state = match &self.state {
            AggregatorState::Idle(state) => self.cycle_idle(state).await?,
            AggregatorState::Blocked(state) => self.cycle_blocked(state).await?,
            AggregatorState::Ready(state) => self.cycle_ready(state).await?,
            AggregatorState::Signing(state) => self.cycle_signing(state).await?,
        };

        self.state = next_state;
        self.runner.increment_runtime_cycle_success_since_startup_counter();

        Ok(())
    }

    async fn cycle_idle(&self, state: &IdleState) -> Result<AggregatorState, RuntimeError> {
        let last_time_point = self.runner.get_time_point_from_chain().await.with_context(
            || "AggregatorRuntime in the state IDLE can not get current time point from chain",
        )?;
        let last_genesis_certificate_epoch = self.runner.last_genesis_certificate_epoch().await?;

        info!(
            self.logger,
            "→ Trying to transition to READY";
            "last_time_point" => ?last_time_point, "last_genesis_certificate_epoch" => ?last_genesis_certificate_epoch
        );

        if self.config.is_follower {
            self.cycle_idle_follower(state, last_time_point, last_genesis_certificate_epoch)
                .await
        } else {
            self.cycle_idle_leader(state, last_time_point, last_genesis_certificate_epoch)
                .await
        }
    }

    async fn cycle_idle_leader(
        &self,
        state: &IdleState,
        last_time_point: TimePoint,
        last_genesis_certificate_epoch: Option<Epoch>,
    ) -> Result<AggregatorState, RuntimeError> {
        let chain_validity_result = self
            .run_common_idle_transition_tasks(
                state,
                &last_time_point,
                last_genesis_certificate_epoch,
            )
            .await?;

        self.transition_from_idle(
            last_time_point,
            last_genesis_certificate_epoch,
            chain_validity_result,
        )
    }

    async fn cycle_idle_follower(
        &self,
        state: &IdleState,
        last_time_point: TimePoint,
        last_genesis_certificate_epoch: Option<Epoch>,
    ) -> Result<AggregatorState, RuntimeError> {
        let can_transition_from_idle_to_ready = self
            .runner
            .is_follower_aggregator_at_same_epoch_as_leader(&last_time_point)
            .await?;

        if !can_transition_from_idle_to_ready {
            return Ok(AggregatorState::Idle(state.clone()));
        }

        let chain_validity_result = self
            .run_common_idle_transition_tasks(
                state,
                &last_time_point,
                last_genesis_certificate_epoch,
            )
            .await?;

        let force_sync = chain_validity_result.is_err();
        self.runner
            .synchronize_follower_aggregator_certificate_chain(force_sync)
            .await?;
        // Refetch last genesis certificate epoch after synchronization
        let last_genesis_certificate_epoch = self.runner.last_genesis_certificate_epoch().await?;

        self.transition_from_idle(
            last_time_point,
            last_genesis_certificate_epoch,
            chain_validity_result,
        )
    }

    async fn cycle_blocked(&self, state: &BlockedState) -> Result<AggregatorState, RuntimeError> {
        let last_time_point: TimePoint =
            self.runner.get_time_point_from_chain().await.with_context(
                || "AggregatorRuntime in the state BLOCKED can not get current time point from chain",
            )?;

        if state.blocked_since_time_point.epoch < last_time_point.epoch {
            // transition Blocked > IDLE
            info!(self.logger, "→ Epoch has changed, transitioning to IDLE"; "last_time_point" => ?last_time_point);

            Ok(AggregatorState::Idle(IdleState {
                current_time_point: Some(state.blocked_since_time_point.clone()),
            }))
        } else {
            Ok(AggregatorState::Blocked(state.clone()))
        }
    }

    async fn cycle_ready(&self, state: &ReadyState) -> Result<AggregatorState, RuntimeError> {
        let last_time_point: TimePoint =
            self.runner.get_time_point_from_chain().await.with_context(
                || "AggregatorRuntime in the state READY can not get current time point from chain",
            )?;

        if state.current_time_point.epoch < last_time_point.epoch {
            // transition READY > IDLE
            info!(self.logger, "→ Epoch has changed, transitioning to IDLE"; "last_time_point" => ?last_time_point);

            // Keep the time point from the previous epoch so the next IDLE cycle detects
            // the epoch change and runs epoch initialization tasks.
            Ok(AggregatorState::Idle(IdleState {
                current_time_point: Some(state.current_time_point.clone()),
            }))
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

            Ok(AggregatorState::Signing(new_state))
        } else {
            // READY > READY
            info!(
                self.logger, " ⋅ No open message to certify, waiting…";
                "time_point" => ?state.current_time_point
            );

            Ok(AggregatorState::Ready(ReadyState {
                current_time_point: last_time_point,
            }))
        }
    }

    async fn cycle_signing(&self, state: &SigningState) -> Result<AggregatorState, RuntimeError> {
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
            Ok(AggregatorState::Idle(new_state))
        } else if is_outdated {
            // SIGNING > READY
            info!(
                self.logger,
                "→ Open message changed, transitioning to READY"
            );
            let new_state = self.transition_from_signing_to_ready_new_open_message(state).await?;
            Ok(AggregatorState::Ready(new_state))
        } else {
            // SIGNING > READY
            let new_state = self.transition_from_signing_to_ready_multisignature(state).await?;
            info!(
                self.logger,
                "→ A multi-signature has been created, build an artifact & a certificate and transitioning back to READY"
            );
            Ok(AggregatorState::Ready(new_state))
        }
    }

    /// Execute tasks that must be run once in an epoch.
    async fn execute_epoch_initialization_tasks(
        &self,
        new_time_point: &TimePoint,
    ) -> Result<(), RuntimeError> {
        trace!(self.logger, "Trying transition from IDLE to READY state");

        self.runner.close_signer_registration_round().await?;
        self.runner
            .update_era_checker(new_time_point.epoch)
            .await
            .map_err(|e| RuntimeError::critical("transiting IDLE → READY", Some(e)))?;
        self.runner.update_stake_distribution(new_time_point).await?;
        self.runner.inform_new_epoch(new_time_point.epoch).await?;
        self.runner.upkeep(new_time_point.epoch).await?;
        self.runner.open_signer_registration_round(new_time_point).await?;
        if self.config.is_follower {
            self.runner
                .synchronize_follower_aggregator_signer_registration()
                .await?;
        }

        Ok(())
    }

    /// Run tasks for the idle transition shared by the leader and the follower
    ///
    /// Returns the result of the chain validity check.
    async fn run_common_idle_transition_tasks(
        &self,
        state: &IdleState,
        last_time_point: &TimePoint,
        last_genesis_certificate_epoch: Option<Epoch>,
    ) -> Result<StdResult<()>, RuntimeError> {
        if state.should_run_epoch_initialization_tasks(last_time_point) {
            self.execute_epoch_initialization_tasks(last_time_point).await?;

            // We can't precompute epoch data if there's no genesis yet or the genesis was issued in
            // the actual epoch
            if last_genesis_certificate_epoch
                .is_some_and(|genesis_epoch| genesis_epoch < last_time_point.epoch)
            {
                self.runner.precompute_epoch_data().await?;
            }
        }

        if last_genesis_certificate_epoch.is_none() {
            // No genesis means no chain to validate
            Ok(Ok(()))
        } else {
            Ok(self.runner.is_certificate_chain_valid(last_time_point).await)
        }
    }

    fn transition_from_idle(
        &self,
        last_time_point: TimePoint,
        last_genesis_certificate_epoch: Option<Epoch>,
        chain_validity_result: StdResult<()>,
    ) -> Result<AggregatorState, RuntimeError> {
        if let Err(error) = chain_validity_result {
            match error.downcast_ref::<CertifierServiceError>() {
                Some(CertifierServiceError::CertificateEpochGap {
                    certificate_epoch, ..
                }) => {
                    info!(self.logger, "→ Epoch gap detected, transitioning to Blocked(epoch-gap)"; "epoch_of_last_certificate" => ?certificate_epoch);
                    Ok(AggregatorState::Blocked(BlockedState {
                        blocked_since_time_point: last_time_point,
                        reason: BlockedReason::EpochGap {
                            epoch_of_last_certificate: *certificate_epoch,
                        },
                    }))
                }
                _ => Err(RuntimeError::keep_state(
                    "certificate chain is invalid",
                    Some(error),
                )),
            }
        } else if let Some(genesis_epoch) = last_genesis_certificate_epoch
            && genesis_epoch == last_time_point.epoch
        {
            info!(
                self.logger,
                "→ Still in the epoch of the genesis certificate, transitioning to Blocked(genesis-epoch)"
            );
            Ok(AggregatorState::Blocked(BlockedState {
                blocked_since_time_point: last_time_point,
                reason: BlockedReason::GenesisEpoch,
            }))
        } else if last_genesis_certificate_epoch.is_none() {
            info!(
                self.logger,
                "→ No genesis certificate detected, transitioning to Blocked(no-genesis)"
            );
            Ok(AggregatorState::Blocked(BlockedState {
                blocked_since_time_point: last_time_point,
                reason: BlockedReason::NoGenesis,
            }))
        } else {
            Ok(AggregatorState::Ready(ReadyState {
                current_time_point: last_time_point,
            }))
        }
    }

    /// Perform a transition from the ` SIGNING ` state to the ` READY ` state when
    /// a new multi-signature is issued.
    async fn transition_from_signing_to_ready_multisignature(
        &self,
        state: &SigningState,
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
            current_time_point: state.current_time_point.clone(),
        })
    }

    /// Perform a transition from the ` SIGNING ` state to the ` IDLE ` state when
    /// a new epoch is detected.
    async fn transition_from_signing_to_idle(
        &self,
        state: &SigningState,
    ) -> Result<IdleState, RuntimeError> {
        trace!(
            self.logger,
            "Launching transition from SIGNING to IDLE state"
        );

        // Keep the time point from the previous epoch so the next IDLE cycle detects
        // the epoch change and runs epoch initialization tasks.
        Ok(IdleState {
            current_time_point: Some(state.current_time_point.clone()),
        })
    }

    /// Perform a transition from the ` SIGNING ` state to the ` READY ` state when
    /// a new open message is detected.
    async fn transition_from_signing_to_ready_new_open_message(
        &self,
        state: &SigningState,
    ) -> Result<ReadyState, RuntimeError> {
        trace!(
            self.logger,
            "Launching transition from SIGNING to READY state"
        );

        Ok(ReadyState {
            current_time_point: state.current_time_point.clone(),
        })
    }

    /// Perform a transition from the ` READY ` state to the ` SIGNING ` state when
    /// a new open message is opened.
    async fn transition_from_ready_to_signing(
        &self,
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
    use anyhow::anyhow;
    use mockall::predicate;
    use std::time::Duration;

    use mithril_common::entities::ImmutableFileNumber;
    use mithril_common::test::double::{Dummy, fake_data};

    use crate::entities::OpenMessage;
    use crate::test::TestLogger;

    use super::super::runner::MockAggregatorRunner;
    use super::*;

    fn time_point(epoch: Epoch, immutable_file_number: ImmutableFileNumber) -> TimePoint {
        TimePoint {
            epoch,
            immutable_file_number,
            ..Dummy::dummy()
        }
    }

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

    fn configure_get_time_point_from_chain(
        runner: &mut MockAggregatorRunner,
        time_point: &TimePoint,
    ) {
        let time_point = time_point.clone();
        runner
            .expect_get_time_point_from_chain()
            .once()
            .returning(move || Ok(time_point.clone()));
    }

    fn configure_runner_for_epoch_initialization_tasks(
        runner: &mut MockAggregatorRunner,
        time_point: &TimePoint,
    ) {
        runner
            .expect_update_stake_distribution()
            .with(predicate::eq(time_point.clone()))
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
            .expect_update_era_checker()
            .with(predicate::eq(time_point.epoch))
            .once()
            .returning(|_| Ok(()));
        runner
            .expect_inform_new_epoch()
            .with(predicate::eq(time_point.epoch))
            .once()
            .returning(|_| Ok(()));
        runner
            .expect_upkeep()
            .with(predicate::eq(time_point.epoch))
            .once()
            .returning(|_| Ok(()));
        runner
            .expect_increment_runtime_cycle_total_since_startup_counter()
            .once()
            .returning(|| ());
    }

    mod leader {
        use super::*;

        #[tokio::test]
        pub async fn idle_check_certificate_chain_is_not_valid() {
            let mut runner = MockAggregatorRunner::new();
            let time_point = time_point(Epoch(10), 100);
            configure_get_time_point_from_chain(&mut runner, &time_point);
            configure_runner_for_epoch_initialization_tasks(&mut runner, &time_point);
            runner
                .expect_last_genesis_certificate_epoch()
                .once()
                .returning(|| Ok(Some(Epoch(9))));
            runner.expect_precompute_epoch_data().once().returning(|| Ok(()));
            runner
                .expect_is_certificate_chain_valid()
                .once()
                .returning(|_| Err(anyhow!("error")));
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

            assert_eq!("idle", runtime.state_label());
        }

        #[tokio::test]
        pub async fn idle_check_certificate_chain_epoch_gap() {
            let mut runner = MockAggregatorRunner::new();
            let time_point = time_point(Epoch(10), 100);
            configure_get_time_point_from_chain(&mut runner, &time_point);
            configure_runner_for_epoch_initialization_tasks(&mut runner, &time_point);
            runner
                .expect_last_genesis_certificate_epoch()
                .once()
                .returning(|| Ok(Some(Epoch(9))));
            runner.expect_precompute_epoch_data().once().returning(|| Ok(()));
            runner.expect_is_certificate_chain_valid().once().returning(|_| {
                Err(anyhow!(CertifierServiceError::CertificateEpochGap {
                    certificate_epoch: Epoch(999),
                    current_epoch: Epoch(111),
                }))
            });
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

            assert_eq!("blocked-epoch-gap", runtime.state_label());
        }

        #[tokio::test]
        pub async fn idle_check_missing_genesis_certificate() {
            let mut runner = MockAggregatorRunner::new();
            let time_point = time_point(Epoch(10), 100);
            configure_get_time_point_from_chain(&mut runner, &time_point);
            configure_runner_for_epoch_initialization_tasks(&mut runner, &time_point);
            runner
                .expect_last_genesis_certificate_epoch()
                .once()
                .returning(|| Ok(None));
            runner.expect_precompute_epoch_data().never();
            runner.expect_is_certificate_chain_valid().never();
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

            assert_eq!("blocked-no-genesis", runtime.state_label());
        }

        #[tokio::test]
        pub async fn idle_check_epoch_is_same_as_genesis_certificate() {
            let mut runner = MockAggregatorRunner::new();
            let time_point = time_point(Epoch(10), 100);
            configure_get_time_point_from_chain(&mut runner, &time_point);
            configure_runner_for_epoch_initialization_tasks(&mut runner, &time_point);
            runner
                .expect_last_genesis_certificate_epoch()
                .once()
                .returning(|| Ok(Some(Epoch(10))));
            runner.expect_precompute_epoch_data().never();
            runner
                .expect_is_certificate_chain_valid()
                .once()
                .returning(|_| Ok(()));
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

            assert_eq!("blocked-genesis-epoch", runtime.state_label());
        }

        #[tokio::test]
        pub async fn idle_remain_if_genesis_epoch_but_chain_verification_fail() {
            let mut runner = MockAggregatorRunner::new();
            let time_point = time_point(Epoch(10), 100);
            configure_get_time_point_from_chain(&mut runner, &time_point);
            configure_runner_for_epoch_initialization_tasks(&mut runner, &time_point);
            runner
                .expect_last_genesis_certificate_epoch()
                .once()
                .returning(|| Ok(Some(Epoch(10))));
            runner.expect_precompute_epoch_data().never();
            runner
                .expect_is_certificate_chain_valid()
                .once()
                .returning(|_| Err(anyhow!("error")));
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

            assert_eq!("idle", runtime.state_label());
        }

        #[tokio::test]
        pub async fn idle_check_certificate_chain_is_valid() {
            let mut runner = MockAggregatorRunner::new();
            let time_point = time_point(Epoch(10), 100);
            configure_get_time_point_from_chain(&mut runner, &time_point);
            configure_runner_for_epoch_initialization_tasks(&mut runner, &time_point);
            runner
                .expect_last_genesis_certificate_epoch()
                .once()
                .returning(|| Ok(Some(Epoch(9))));
            runner.expect_precompute_epoch_data().once().returning(|| Ok(()));
            runner
                .expect_is_certificate_chain_valid()
                .once()
                .returning(|_| Ok(()));
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

            assert_eq!("ready", runtime.state_label());
        }

        #[tokio::test]
        pub async fn blocked_new_epoch_detected() {
            for reason in [
                BlockedReason::NoGenesis,
                BlockedReason::GenesisEpoch,
                BlockedReason::EpochGap {
                    epoch_of_last_certificate: Epoch(999),
                },
            ] {
                let mut runner = MockAggregatorRunner::new();
                let time_point = TimePoint::dummy();
                let new_time_point = TimePoint {
                    epoch: time_point.epoch + 1,
                    ..time_point.clone()
                };
                configure_get_time_point_from_chain(&mut runner, &new_time_point);
                runner
                    .expect_increment_runtime_cycle_total_since_startup_counter()
                    .once()
                    .returning(|| ());
                runner
                    .expect_increment_runtime_cycle_success_since_startup_counter()
                    .once()
                    .returning(|| ());

                let mut runtime = init_runtime(
                    Some(AggregatorState::Blocked(BlockedState {
                        blocked_since_time_point: time_point,
                        reason,
                    })),
                    runner,
                    false,
                )
                .await;
                runtime.cycle().await.unwrap();

                assert_eq!("idle", runtime.state_label());
            }
        }

        #[tokio::test]
        pub async fn blocked_state_remains_if_no_epoch_change() {
            for reason in [
                BlockedReason::NoGenesis,
                BlockedReason::GenesisEpoch,
                BlockedReason::EpochGap {
                    epoch_of_last_certificate: Epoch(999),
                },
            ] {
                let mut runner = MockAggregatorRunner::new();
                let time_point = TimePoint::dummy();
                let new_time_point = TimePoint {
                    immutable_file_number: time_point.immutable_file_number + 1,
                    ..time_point.clone()
                };
                configure_get_time_point_from_chain(&mut runner, &new_time_point);
                runner
                    .expect_increment_runtime_cycle_total_since_startup_counter()
                    .once()
                    .returning(|| ());
                runner
                    .expect_increment_runtime_cycle_success_since_startup_counter()
                    .once()
                    .returning(|| ());

                let mut runtime = init_runtime(
                    Some(AggregatorState::Blocked(BlockedState {
                        blocked_since_time_point: time_point,
                        reason,
                    })),
                    runner,
                    false,
                )
                .await;
                runtime.cycle().await.unwrap();

                assert!(
                    runtime.state_label().starts_with("blocked-"),
                    "state label: {}",
                    runtime.state_label()
                );
            }
        }

        #[tokio::test]
        pub async fn ready_new_epoch_detected() {
            let mut runner = MockAggregatorRunner::new();
            let time_point = TimePoint::dummy();
            let new_time_point = TimePoint {
                epoch: time_point.epoch + 1,
                ..time_point.clone()
            };
            configure_get_time_point_from_chain(&mut runner, &new_time_point);
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

            assert_eq!("idle", runtime.state_label());
        }

        #[tokio::test]
        pub async fn ready_open_message_not_exist() {
            let mut runner = MockAggregatorRunner::new();
            let time_point = time_point(Epoch(10), 100);
            let next_time_point = TimePoint {
                immutable_file_number: time_point.immutable_file_number + 1,
                ..time_point.clone()
            };
            let expected_time_point = next_time_point.clone();
            configure_get_time_point_from_chain(&mut runner, &next_time_point);
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

            assert_eq!("ready", runtime.state_label());
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
            let time_point = time_point(Epoch(10), 100);
            configure_get_time_point_from_chain(&mut runner, &time_point);
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
                    current_time_point: time_point,
                })),
                runner,
                false,
            )
            .await;
            runtime.cycle().await.unwrap();

            assert_eq!("signing", runtime.state_label());
        }

        #[tokio::test]
        async fn signing_changing_open_message_to_ready() {
            let mut runner = MockAggregatorRunner::new();
            let time_point = time_point(Epoch(10), 100);
            configure_get_time_point_from_chain(&mut runner, &time_point);
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
                current_time_point: time_point,
                open_message: OpenMessage::dummy(),
            });

            let mut runtime = init_runtime(Some(initial_state), runner, false).await;
            runtime.cycle().await.unwrap();

            assert_eq!("ready", runtime.state_label());
        }

        #[tokio::test]
        async fn signing_certificate_is_not_created() {
            let mut runner = MockAggregatorRunner::new();
            let time_point = time_point(Epoch(10), 100);
            configure_get_time_point_from_chain(&mut runner, &time_point);
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
                current_time_point: time_point,
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

            assert_eq!("signing", runtime.state_label());
        }

        #[tokio::test]
        async fn signing_artifact_not_created() {
            let mut runner = MockAggregatorRunner::new();
            let time_point = time_point(Epoch(10), 100);
            configure_get_time_point_from_chain(&mut runner, &time_point);
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
                current_time_point: time_point,
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

            assert_eq!("signing", runtime.state_label());
        }

        #[tokio::test]
        async fn signing_certificate_is_created() {
            let mut runner = MockAggregatorRunner::new();
            let time_point = time_point(Epoch(10), 100);
            configure_get_time_point_from_chain(&mut runner, &time_point);
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
                current_time_point: time_point,
                open_message: OpenMessage::dummy(),
            };
            let mut runtime =
                init_runtime(Some(AggregatorState::Signing(state)), runner, false).await;
            runtime.cycle().await.unwrap();

            assert_eq!("ready", runtime.state_label());
        }

        #[tokio::test]
        pub async fn critical_error() {
            let mut runner = MockAggregatorRunner::new();
            let time_point = time_point(Epoch(10), 100);
            configure_get_time_point_from_chain(&mut runner, &time_point);
            runner
                .expect_last_genesis_certificate_epoch()
                .once()
                .returning(|| Err(anyhow!("ERROR")));
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

            assert_eq!("idle", runtime.state_label());
        }
    }

    mod follower {
        use mockall::predicate::eq;

        use crate::services::CertificateChainSynchronizationOutcome;

        use super::*;

        #[tokio::test]
        pub async fn idle_no_epoch_and_leader_have_same_chain() {
            let mut runner = MockAggregatorRunner::new();
            let time_point = time_point(Epoch(10), 100);
            configure_get_time_point_from_chain(&mut runner, &time_point);
            configure_runner_for_epoch_initialization_tasks(&mut runner, &time_point);
            runner
                .expect_last_genesis_certificate_epoch()
                .times(2)
                .returning(|| Ok(Some(Epoch(9))));
            runner.expect_precompute_epoch_data().once().returning(|| Ok(()));
            runner
                .expect_is_follower_aggregator_at_same_epoch_as_leader()
                .once()
                .returning(|_| Ok(true));
            runner
                .expect_synchronize_follower_aggregator_signer_registration()
                .once()
                .returning(|| Ok(()));
            runner
                .expect_is_certificate_chain_valid()
                .once()
                .returning(|_| Ok(()));
            runner
                .expect_synchronize_follower_aggregator_certificate_chain()
                .once()
                .with(eq(false)) // Certificate chain valid so force_sync must be false
                .returning(|_| Ok(CertificateChainSynchronizationOutcome::AlreadyUpToDate));
            runner
                .expect_increment_runtime_cycle_success_since_startup_counter()
                .once()
                .returning(|| ());
            let mut runtime = init_runtime(
                Some(AggregatorState::Idle(IdleState {
                    current_time_point: None,
                })),
                runner,
                true,
            )
            .await;
            runtime.cycle().await.unwrap();

            assert_eq!("ready", runtime.state_label());
        }

        #[tokio::test]
        pub async fn idle_new_epoch_detected_and_leader_not_transitioned_to_epoch() {
            let mut runner = MockAggregatorRunner::new();
            let time_point = time_point(Epoch(10), 100);
            let new_time_point = TimePoint {
                epoch: time_point.epoch + 1,
                ..time_point.clone()
            };
            configure_get_time_point_from_chain(&mut runner, &new_time_point);
            runner
                .expect_last_genesis_certificate_epoch()
                .once()
                .returning(|| Ok(Some(Epoch(9))));
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

            assert_eq!("idle", runtime.state_label());
        }

        #[tokio::test]
        pub async fn idle_new_epoch_detected_and_leader_has_transitioned_to_epoch() {
            let mut runner = MockAggregatorRunner::new();
            let time_point = time_point(Epoch(10), 100);
            let new_time_point = TimePoint {
                epoch: time_point.epoch + 1,
                ..time_point.clone()
            };
            configure_get_time_point_from_chain(&mut runner, &new_time_point);
            configure_runner_for_epoch_initialization_tasks(&mut runner, &new_time_point);
            runner
                .expect_last_genesis_certificate_epoch()
                .times(2)
                .returning(|| Ok(Some(Epoch(9))));
            runner.expect_precompute_epoch_data().once().returning(|| Ok(()));
            runner
                .expect_is_follower_aggregator_at_same_epoch_as_leader()
                .once()
                .returning(|_| Ok(true));
            runner
                .expect_synchronize_follower_aggregator_signer_registration()
                .once()
                .returning(|| Ok(()));
            runner
                .expect_is_certificate_chain_valid()
                .once()
                .returning(|_| Ok(()));
            runner
                .expect_synchronize_follower_aggregator_certificate_chain()
                .once()
                .with(eq(false)) // Certificate chain valid so force_sync must be false
                .returning(|_| Ok(CertificateChainSynchronizationOutcome::Synchronized));
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

            assert_eq!("ready", runtime.state_label());
        }

        #[tokio::test]
        pub async fn idle_no_genesis_sync_leader_without_genesis() {
            let mut runner = MockAggregatorRunner::new();
            let time_point = time_point(Epoch(10), 100);
            let new_time_point = TimePoint {
                epoch: time_point.epoch + 1,
                ..time_point.clone()
            };
            configure_get_time_point_from_chain(&mut runner, &new_time_point);
            configure_runner_for_epoch_initialization_tasks(&mut runner, &new_time_point);
            runner
                .expect_last_genesis_certificate_epoch()
                .times(2)
                .returning(|| Ok(None));
            runner
                .expect_is_follower_aggregator_at_same_epoch_as_leader()
                .once()
                .returning(|_| Ok(true));
            runner
                .expect_synchronize_follower_aggregator_signer_registration()
                .once()
                .returning(|| Ok(()));
            runner.expect_is_certificate_chain_valid().never();
            runner
                .expect_synchronize_follower_aggregator_certificate_chain()
                .once()
                .with(eq(false)) // Certificate chain valid so force_sync must be false
                .returning(|_| {
                    Ok(CertificateChainSynchronizationOutcome::EmptyRemoteCertificateChain)
                });
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

            assert_eq!("blocked-no-genesis", runtime.state_label());
        }

        #[tokio::test]
        pub async fn idle_no_genesis_sync_leader_with_genesis_at_current_epoch() {
            let mut runner = MockAggregatorRunner::new();
            let time_point = time_point(Epoch(10), 100);
            let new_time_point = TimePoint {
                epoch: time_point.epoch + 1,
                ..time_point.clone()
            };
            configure_get_time_point_from_chain(&mut runner, &new_time_point);
            configure_runner_for_epoch_initialization_tasks(&mut runner, &new_time_point);
            runner
                .expect_last_genesis_certificate_epoch()
                .once()
                .returning(|| Ok(None));
            // Second call used to check the synchronized genesis certificate epoch
            runner
                .expect_last_genesis_certificate_epoch()
                .once()
                .returning(|| Ok(Some(Epoch(11))));
            runner
                .expect_is_follower_aggregator_at_same_epoch_as_leader()
                .once()
                .returning(|_| Ok(true));
            runner
                .expect_synchronize_follower_aggregator_signer_registration()
                .once()
                .returning(|| Ok(()));
            runner.expect_is_certificate_chain_valid().never();
            runner
                .expect_synchronize_follower_aggregator_certificate_chain()
                .once()
                .with(eq(false)) // Certificate chain valid so force_sync must be false
                .returning(|_| Ok(CertificateChainSynchronizationOutcome::Synchronized));
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

            assert_eq!("blocked-genesis-epoch", runtime.state_label());
        }
    }
}
