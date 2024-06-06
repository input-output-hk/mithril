use crate::{
    entities::OpenMessage,
    runtime::{AggregatorRunnerTrait, RuntimeError},
    AggregatorConfig,
};

use anyhow::Context;
use mithril_common::entities::TimePoint;
use slog_scope::{crit, info, trace, warn};
use std::fmt::Display;
use std::sync::Arc;
use tokio::time::sleep;

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
    /// Configuration
    config: AggregatorConfig,

    /// the internal state of the automate
    state: AggregatorState,

    /// specific runner for this state machine
    runner: Arc<dyn AggregatorRunnerTrait>,
}

impl AggregatorRuntime {
    /// Create a new instance of the state machine.
    pub async fn new(
        aggregator_config: AggregatorConfig,
        init_state: Option<AggregatorState>,
        runner: Arc<dyn AggregatorRunnerTrait>,
    ) -> Result<Self, RuntimeError> {
        info!("initializing runtime");

        let state = if let Some(init_state) = init_state {
            trace!("got initial state from caller");
            init_state
        } else {
            trace!("idle state, no current time point");
            AggregatorState::Idle(IdleState {
                current_time_point: None,
            })
        };

        Ok(Self {
            config: aggregator_config,
            state,
            runner,
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
        info!("STATE MACHINE: launching");

        loop {
            if let Err(e) = self.cycle().await {
                warn!("State machine issued an error: {e}");

                match &e {
                    RuntimeError::Critical {
                        message: _,
                        nested_error: _,
                    } => {
                        crit!("state machine: a critical error occurred: {e:?}");

                        return Err(e);
                    }
                    RuntimeError::KeepState {
                        message,
                        nested_error,
                    } => {
                        warn!(
                            "KeepState Error: {message}. Nested error: «{}».",
                            nested_error
                                .as_ref()
                                .map(|e| format!("{e:?}"))
                                .unwrap_or("None".into())
                        );
                    }
                    RuntimeError::ReInit {
                        message,
                        nested_error,
                    } => {
                        warn!(
                            "ReInit Error: {message}. Nested error: «{}».",
                            nested_error
                                .as_ref()
                                .map(|e| format!("{e:?}"))
                                .unwrap_or("None".into())
                        );
                        self.state = AggregatorState::Idle(IdleState {
                            current_time_point: None,
                        });
                    }
                }
            }

            info!(
                "… Cycle finished, Sleeping for {} ms",
                self.config.interval.as_millis()
            );
            sleep(self.config.interval).await;
        }
    }

    /// Perform one tick of the state machine.
    pub async fn cycle(&mut self) -> Result<(), RuntimeError> {
        info!("================================================================================");
        info!("STATE MACHINE: new cycle: {}", self.state);

        match self.state.clone() {
            AggregatorState::Idle(state) => {
                let last_time_point = self.runner.get_time_point_from_chain().await.with_context(
                    || "AggregatorRuntime in the state IDLE can not get current time point from chain",
                )?;

                info!("→ trying to transition to READY"; "last_time_point" => ?last_time_point);

                self.try_transition_from_idle_to_ready(
                    state.current_time_point,
                    last_time_point.clone(),
                )
                .await?;
                self.state = AggregatorState::Ready(ReadyState {
                    current_time_point: last_time_point,
                });
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
                    info!("→ Epoch has changed, transitioning to IDLE"; "last_time_point" => ?last_time_point);
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
                    info!("→ transitioning to SIGNING");
                    let new_state = self
                        .transition_from_ready_to_signing(last_time_point.clone(), open_message.clone())
                        .await.with_context(|| format!("AggregatorRuntime can not perform a transition from READY state to SIGNING with entity_type: '{:?}'", open_message.signed_entity_type))?;
                    self.state = AggregatorState::Signing(new_state);
                } else {
                    // READY > READY
                    info!(
                        " ⋅ no open message to certify, waiting…";
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
                let current_open_message = self
                    .runner
                    .get_current_open_message_for_signed_entity_type(
                        &state.open_message.signed_entity_type,
                    )
                    .await
                    .with_context(|| format!("AggregatorRuntime can not get the current open message for signed entity type: '{}'", &state.open_message.signed_entity_type))?;
                let is_expired_open_message = current_open_message
                    .as_ref()
                    .map(|om| om.is_expired)
                    .unwrap_or(false);
                let exists_newer_open_message = {
                    let new_signed_entity_type = self
                        .config
                        .signed_entity_config
                        .time_point_to_signed_entity(
                            &state.open_message.signed_entity_type,
                            &last_time_point,
                        );
                    new_signed_entity_type != state.open_message.signed_entity_type
                };

                if state.current_time_point.epoch < last_time_point.epoch {
                    // SIGNING > IDLE
                    info!("→ Epoch changed, transitioning to IDLE");
                    let new_state = self.transition_from_signing_to_idle(state).await?;
                    self.state = AggregatorState::Idle(new_state);
                } else if exists_newer_open_message || is_expired_open_message {
                    // SIGNING > READY
                    info!("→ Open message changed, transitioning to READY");
                    let new_state = self
                        .transition_from_signing_to_ready_new_open_message(state)
                        .await?;
                    self.state = AggregatorState::Ready(new_state);
                } else {
                    // SIGNING > READY
                    let new_state = self
                        .transition_from_signing_to_ready_multisignature(state)
                        .await?;
                    info!("→ a multi-signature have been created, build an artifact & a certificate and transitioning back to READY");
                    self.state = AggregatorState::Ready(new_state);
                }
            }
        }
        Ok(())
    }

    /// Perform a transition from `IDLE` state to `READY` state when
    /// the certificate chain is valid.
    async fn try_transition_from_idle_to_ready(
        &mut self,
        maybe_current_time_point: Option<TimePoint>,
        new_time_point: TimePoint,
    ) -> Result<(), RuntimeError> {
        trace!("trying transition from IDLE to READY state");

        if maybe_current_time_point.is_none()
            || maybe_current_time_point.unwrap().epoch < new_time_point.epoch
        {
            self.runner.close_signer_registration_round().await?;
            self.runner
                .update_era_checker(new_time_point.epoch)
                .await
                .map_err(|e| RuntimeError::critical("transiting IDLE → READY", Some(e)))?;
            self.runner.inform_new_epoch(new_time_point.epoch).await?;
            self.runner
                .update_stake_distribution(&new_time_point)
                .await?;
            self.runner
                .open_signer_registration_round(&new_time_point)
                .await?;
            self.runner.update_protocol_parameters().await?;
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
        trace!("launching transition from SIGNING to READY state");
        let certificate = self
            .runner
            .create_certificate(&state.open_message.signed_entity_type)
            .await?
            .ok_or_else(|| RuntimeError::KeepState {
                message: "not enough signature yet to create a certificate, waiting…".to_string(),
                nested_error: None,
            })?;
        self.runner
            .drop_pending_certificate()
            .await
            .map_err(|e| RuntimeError::ReInit {
                message: "transiting SIGNING → READY: failed to drop pending certificate"
                    .to_string(),
                nested_error: Some(e),
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
        trace!("launching transition from SIGNING to IDLE state");
        self.runner.drop_pending_certificate().await?;

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
        trace!("launching transition from SIGNING to READY state");
        self.runner.drop_pending_certificate().await?;

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
        trace!("launching transition from READY to SIGNING state");

        let certificate_pending = self
            .runner
            .create_new_pending_certificate(
                new_time_point.clone(),
                &open_message.signed_entity_type,
            )
            .await?;
        self.runner
            .save_pending_certificate(certificate_pending.clone())
            .await?;
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

    use mithril_common::entities::{Epoch, SignedEntityConfig, SignedEntityType};
    use mithril_common::test_utils::fake_data;

    use super::super::runner::MockAggregatorRunner;
    use super::*;

    async fn init_runtime(
        init_state: Option<AggregatorState>,
        runner: MockAggregatorRunner,
    ) -> AggregatorRuntime {
        AggregatorRuntime::new(
            AggregatorConfig::new(Duration::from_millis(20), SignedEntityConfig::dummy()),
            init_state,
            Arc::new(runner),
        )
        .await
        .unwrap()
    }

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
        runner
            .expect_update_protocol_parameters()
            .once()
            .returning(|| Ok(()));
        runner
            .expect_precompute_epoch_data()
            .once()
            .returning(|| Ok(()));

        let mut runtime = init_runtime(
            Some(AggregatorState::Idle(IdleState {
                current_time_point: None,
            })),
            runner,
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
        runner
            .expect_update_protocol_parameters()
            .once()
            .returning(|| Ok(()));
        runner
            .expect_precompute_epoch_data()
            .once()
            .returning(|| Ok(()));

        let mut runtime = init_runtime(
            Some(AggregatorState::Idle(IdleState {
                current_time_point: None,
            })),
            runner,
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
        let mut runtime = init_runtime(
            Some(AggregatorState::Ready(ReadyState {
                current_time_point: time_point,
            })),
            runner,
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
        let mut runtime = init_runtime(
            Some(AggregatorState::Ready(ReadyState {
                current_time_point: time_point.clone(),
            })),
            runner,
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
            .expect_create_new_pending_certificate()
            .once()
            .returning(|_, _| Ok(fake_data::certificate_pending()));
        runner
            .expect_save_pending_certificate()
            .once()
            .returning(|_| Ok(()));

        let mut runtime = init_runtime(
            Some(AggregatorState::Ready(ReadyState {
                current_time_point: TimePoint::dummy(),
            })),
            runner,
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
            .expect_get_current_open_message_for_signed_entity_type()
            .once()
            .returning(|_| {
                Ok(Some(OpenMessage {
                    signed_entity_type: SignedEntityType::MithrilStakeDistribution(Epoch(1)),
                    ..OpenMessage::dummy()
                }))
            });
        runner
            .expect_drop_pending_certificate()
            .once()
            .returning(|| Ok(Some(fake_data::certificate_pending())));

        let state = SigningState {
            current_time_point: TimePoint::dummy(),
            open_message: OpenMessage {
                signed_entity_type: SignedEntityType::MithrilStakeDistribution(Epoch(2)),
                ..OpenMessage::dummy()
            },
        };
        let mut runtime = init_runtime(Some(AggregatorState::Signing(state)), runner).await;
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
            .expect_get_current_open_message_for_signed_entity_type()
            .once()
            .returning(|_| Ok(Some(OpenMessage::dummy())));
        runner
            .expect_create_certificate()
            .once()
            .returning(|_| Ok(None));
        let state = SigningState {
            current_time_point: TimePoint::dummy(),
            open_message: OpenMessage::dummy(),
        };
        let mut runtime = init_runtime(Some(AggregatorState::Signing(state)), runner).await;
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
            .expect_get_current_open_message_for_signed_entity_type()
            .once()
            .returning(|_| Ok(Some(OpenMessage::dummy())));
        runner
            .expect_create_certificate()
            .return_once(move |_| Ok(Some(fake_data::certificate("whatever".to_string()))));
        runner
            .expect_drop_pending_certificate()
            .once()
            .returning(|| Ok(Some(fake_data::certificate_pending())));
        runner
            .expect_create_artifact()
            .once()
            .returning(|_, _| Err(anyhow!("whatever")));
        let state = SigningState {
            current_time_point: TimePoint::dummy(),
            open_message: OpenMessage::dummy(),
        };
        let mut runtime = init_runtime(Some(AggregatorState::Signing(state)), runner).await;
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
            .expect_get_current_open_message_for_signed_entity_type()
            .once()
            .returning(|_| Ok(Some(OpenMessage::dummy())));
        runner
            .expect_create_certificate()
            .return_once(move |_| Ok(Some(fake_data::certificate("whatever".to_string()))));
        runner
            .expect_drop_pending_certificate()
            .once()
            .returning(|| Ok(Some(fake_data::certificate_pending())));
        runner
            .expect_create_artifact()
            .once()
            .returning(|_, _| Ok(()));

        let state = SigningState {
            current_time_point: TimePoint::dummy(),
            open_message: OpenMessage::dummy(),
        };
        let mut runtime = init_runtime(Some(AggregatorState::Signing(state)), runner).await;
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

        let mut runtime = init_runtime(
            Some(AggregatorState::Idle(IdleState {
                current_time_point: None,
            })),
            runner,
        )
        .await;
        runtime.cycle().await.unwrap_err();

        assert_eq!("idle".to_string(), runtime.get_state());
    }
}
