use super::{AggregatorRunnerTrait, RuntimeError};

use mithril_common::entities::{Beacon, CertificatePending};
use slog_scope::{debug, error, info, trace};
use std::fmt::Display;
use std::sync::Arc;
use tokio::time::{sleep, Duration};

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct IdleState {
    current_beacon: Option<Beacon>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct SigningState {
    current_beacon: Beacon,
    certificate_pending: CertificatePending,
}

#[derive(Clone, Debug, PartialEq)]
pub enum AggregatorState {
    Idle(IdleState),
    Signing(SigningState),
}

impl Display for AggregatorState {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match *self {
            AggregatorState::Idle(_) => write!(f, "idle"),
            AggregatorState::Signing(_) => write!(f, "signing"),
        }
    }
}

/// AggregatorRuntime
pub struct AggregatorRuntime {
    /// the internal state of the automate
    state: AggregatorState,

    /// time between each state machine execution
    state_sleep: Duration,

    /// specific runner for this state machine
    runner: Arc<dyn AggregatorRunnerTrait>,
}

impl AggregatorRuntime {
    pub fn get_state(&self) -> String {
        self.state.to_string()
    }

    pub async fn new(
        state_sleep: Duration,
        init_state: Option<AggregatorState>,
        runner: Arc<dyn AggregatorRunnerTrait>,
    ) -> Result<Self, RuntimeError> {
        info!("initializing runtime");

        let state = if init_state.is_none() {
            trace!("idle state, no current beacon");
            AggregatorState::Idle(IdleState {
                current_beacon: None,
            })
        } else {
            trace!("got initial state from caller");
            init_state.unwrap()
        };

        Ok::<Self, RuntimeError>(Self {
            state_sleep,
            state,
            runner,
        })
    }

    /// run
    ///
    /// launches an infinite loop ticking the state machine
    pub async fn run(&mut self) {
        info!("Starting runtime");
        debug!("current state: {}", self.state);

        loop {
            if let Err(e) = self.cycle().await {
                error!("{:?}", e)
            }

            info!("Sleeping for {} ms", self.state_sleep.as_millis());
            sleep(self.state_sleep).await;
        }
    }

    /// cycle
    ///
    /// one tick of the state machine
    pub async fn cycle(&mut self) -> Result<(), RuntimeError> {
        info!("================================================================================");
        info!("new cycle");
        match self.state.clone() {
            AggregatorState::Idle(state) => {
                info!("state IDLE");

                if let Some(beacon) = self
                    .runner
                    .is_new_beacon(state.current_beacon.clone())
                    .await?
                {
                    trace!("new beacon found = {:?}", beacon);
                    let new_state = self
                        .transition_from_idle_to_signing(state.current_beacon.clone(), beacon)
                        .await?;
                    self.state = AggregatorState::Signing(new_state);
                } else {
                    trace!("nothing to do in IDLE state")
                }
            }
            AggregatorState::Signing(state) => {
                info!("state SIGNING");

                if let Some(beacon) = self
                    .runner
                    .is_new_beacon(Some(state.current_beacon.clone()))
                    .await?
                {
                    trace!(
                        "new beacon found, immutable file number = {}",
                        beacon.immutable_file_number
                    );
                    let new_state = self
                        .transition_from_signing_to_idle_new_beacon(state)
                        .await?;
                    self.state = AggregatorState::Idle(new_state);
                } else if self.runner.is_multisig_created().await? {
                    trace!("new multisignature found");
                    let new_state = self
                        .transition_from_signing_to_idle_multisignature(state)
                        .await?;
                    self.state = AggregatorState::Idle(new_state);
                } else {
                    trace!("nothing to do in SIGNING state")
                }
            }
        }
        Ok(())
    }

    /// transition
    ///
    /// from SIGNING to IDLE because NEW MULTISIGNATURE
    async fn transition_from_signing_to_idle_multisignature(
        &self,
        state: SigningState,
    ) -> Result<IdleState, RuntimeError> {
        self.runner.drop_pending_certificate().await?;
        let ongoing_snapshot = self.runner.create_snapshot_archive().await?;
        let locations = self
            .runner
            .upload_snapshot_archive(&ongoing_snapshot)
            .await?;
        let certificate = self
            .runner
            .create_and_save_certificate(&state.current_beacon)
            .await?;
        let _ = self
            .runner
            .create_and_save_snapshot(certificate, &ongoing_snapshot, locations)
            .await?;

        Ok(IdleState {
            current_beacon: Some(state.current_beacon),
        })
    }

    /// transition
    ///
    /// from SIGNING to IDLE because NEW BEACON
    async fn transition_from_signing_to_idle_new_beacon(
        &self,
        state: SigningState,
    ) -> Result<IdleState, RuntimeError> {
        self.runner.drop_pending_certificate().await?;

        Ok(IdleState {
            current_beacon: Some(state.current_beacon),
        })
    }

    /// transition
    ///
    /// from IDLE state to SIGNING because NEW BEACON
    async fn transition_from_idle_to_signing(
        &mut self,
        maybe_current_beacon: Option<Beacon>,
        new_beacon: Beacon,
    ) -> Result<SigningState, RuntimeError> {
        debug!("launching transition from IDLE to SIGNING state");
        self.runner.update_beacon(&new_beacon).await?;

        if maybe_current_beacon.is_none() || maybe_current_beacon.unwrap().epoch < new_beacon.epoch
        {
            self.runner.update_stake_distribution(&new_beacon).await?;
            self.runner
                .update_protocol_parameters_in_multisigner(&new_beacon)
                .await?;
        }
        let digester_result = self.runner.compute_digest(&new_beacon).await?;
        self.runner
            .update_message_in_multisigner(digester_result)
            .await?;
        let certificate_pending = self
            .runner
            .create_new_pending_certificate_from_multisigner(new_beacon.clone())
            .await?;
        self.runner
            .save_pending_certificate(certificate_pending.clone())
            .await?;
        let state = SigningState {
            current_beacon: new_beacon,
            certificate_pending,
        };

        Ok(state)
    }
}

#[cfg(test)]
mod tests {
    use std::path::Path;

    use crate::snapshotter::OngoingSnapshot;

    use super::super::runner::MockAggregatorRunner;
    use super::*;
    use mithril_common::fake_data;
    use mockall::predicate;

    async fn init_runtime(
        init_state: Option<AggregatorState>,
        runner: MockAggregatorRunner,
    ) -> AggregatorRuntime {
        AggregatorRuntime::new(Duration::from_millis(100), init_state, Arc::new(runner))
            .await
            .unwrap()
    }

    #[tokio::test]
    pub async fn idle_check_no_new_beacon_with_current_beacon() {
        let mut runner = MockAggregatorRunner::new();
        runner
            .expect_is_new_beacon()
            .times(1)
            .returning(|_| Ok(None));
        let mut runtime = init_runtime(
            Some(AggregatorState::Idle(IdleState {
                current_beacon: Some(fake_data::beacon()),
            })),
            runner,
        )
        .await;
        runtime.cycle().await.unwrap();

        assert_eq!("idle".to_string(), runtime.get_state());
    }

    #[tokio::test]
    pub async fn idle_check_no_new_beacon_with_no_current_beacon() {
        let mut runner = MockAggregatorRunner::new();
        runner
            .expect_is_new_beacon()
            .times(1)
            .returning(|_| Ok(Some(fake_data::beacon())));
        runner
            .expect_compute_digest()
            .times(1)
            .returning(|_| Ok("whatever".to_string()));
        runner
            .expect_update_beacon()
            .with(predicate::eq(fake_data::beacon()))
            .times(1)
            .returning(|_| Ok(()));
        runner
            .expect_update_stake_distribution()
            .with(predicate::eq(fake_data::beacon()))
            .times(1)
            .returning(|_| Ok(()));
        runner
            .expect_update_protocol_parameters_in_multisigner()
            .with(predicate::eq(fake_data::beacon()))
            .times(1)
            .returning(|_| Ok(()));
        runner
            .expect_update_message_in_multisigner()
            .with(predicate::eq("whatever".to_string()))
            .times(1)
            .returning(|_| Ok(()));
        runner
            .expect_create_new_pending_certificate_from_multisigner()
            .with(predicate::eq(fake_data::beacon()))
            .times(1)
            .returning(|_| Ok(fake_data::certificate_pending()));
        runner
            .expect_save_pending_certificate()
            .times(1)
            .returning(|_| Ok(()));

        let mut runtime = init_runtime(
            Some(AggregatorState::Idle(IdleState {
                current_beacon: None,
            })),
            runner,
        )
        .await;
        runtime.cycle().await.unwrap();

        assert_eq!("signing".to_string(), runtime.get_state());
    }

    #[tokio::test]
    async fn signing_changing_beacon_to_idle() {
        let mut runner = MockAggregatorRunner::new();
        runner
            .expect_is_new_beacon()
            .times(1)
            .returning(|_| Ok(Some(fake_data::beacon())));
        runner
            .expect_drop_pending_certificate()
            .times(1)
            .returning(|| Ok(Some(fake_data::certificate_pending())));

        let state = SigningState {
            // this current beacon must be outdated so the state machine will
            // return to idle state
            current_beacon: {
                let mut beacon = fake_data::beacon();
                beacon.immutable_file_number -= 1;

                beacon
            },
            certificate_pending: fake_data::certificate_pending(),
        };
        let mut runtime = init_runtime(Some(AggregatorState::Signing(state)), runner).await;
        runtime.cycle().await.unwrap();

        assert_eq!("idle".to_string(), runtime.get_state());
    }

    #[tokio::test]
    async fn signing_same_beacon_to_signing() {
        let mut runner = MockAggregatorRunner::new();
        runner
            .expect_is_new_beacon()
            .times(1)
            .returning(|_| Ok(None));
        runner
            .expect_is_multisig_created()
            .times(1)
            .returning(|| Ok(false));
        let state = SigningState {
            current_beacon: fake_data::beacon(),
            certificate_pending: fake_data::certificate_pending(),
        };
        let mut runtime = init_runtime(Some(AggregatorState::Signing(state)), runner).await;
        runtime.cycle().await.unwrap();

        assert_eq!("signing".to_string(), runtime.get_state());
    }

    #[tokio::test]
    async fn signing_multisig_ready_to_idle() {
        let mut runner = MockAggregatorRunner::new();
        runner
            .expect_is_new_beacon()
            .times(1)
            .returning(|_| Ok(None));
        runner
            .expect_is_multisig_created()
            .times(1)
            .returning(|| Ok(true));
        runner
            .expect_drop_pending_certificate()
            .times(1)
            .returning(|| Ok(Some(fake_data::certificate_pending())));
        runner
            .expect_create_snapshot_archive()
            .times(1)
            .returning(|| {
                Ok(OngoingSnapshot::new(
                    Path::new("/tmp/archive.zip").to_path_buf(),
                    1234,
                ))
            });
        runner
            .expect_upload_snapshot_archive()
            .times(1)
            .returning(|_path| Ok(vec!["locA".to_string(), "locB".to_string()]));
        runner
            .expect_create_and_save_certificate()
            .times(1)
            .returning(|_| Ok(fake_data::certificate("whatever".to_string())));
        runner
            .expect_create_and_save_snapshot()
            .times(1)
            .returning(|_, _, _| Ok(fake_data::snapshots(1)[0].clone()));

        let state = SigningState {
            current_beacon: fake_data::beacon(),
            certificate_pending: fake_data::certificate_pending(),
        };
        let mut runtime = init_runtime(Some(AggregatorState::Signing(state)), runner).await;
        runtime.cycle().await.unwrap();

        assert_eq!("idle".to_string(), runtime.get_state());
    }
}
