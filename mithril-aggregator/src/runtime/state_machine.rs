use crate::runtime::{AggregatorRunnerTrait, RuntimeError, WorkingCertificate};

use mithril_common::entities::{Beacon, SignedEntityType};
use slog_scope::{crit, info, trace, warn};
use std::fmt::Display;
use std::sync::Arc;
use tokio::time::{sleep, Duration};

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct IdleState {
    current_beacon: Option<Beacon>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ReadyState {
    current_beacon: Beacon,
}

#[derive(Clone, Debug, PartialEq)]
pub struct SigningState {
    current_beacon: Beacon,
    working_certificate: WorkingCertificate,
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
                match &state.current_beacon {
                    None => "No Beacon".to_string(),
                    Some(b) => b.to_string(),
                }
            ),
            AggregatorState::Ready(state) => write!(f, "Ready - {}", state.current_beacon),
            AggregatorState::Signing(state) => write!(f, "Signing - {}", state.current_beacon),
        }
    }
}

/// The AggregatorRuntime responsibility is to create a state machine to handle
/// all actions required by the process of getting multi-signatures.
/// See the [documentation](https://mithril.network/doc/mithril/mithril-network/aggregator#under-the-hood) for more explanations about the Aggregator state machine.
pub struct AggregatorRuntime {
    /// the internal state of the automate
    state: AggregatorState,

    /// time between each state machine execution
    state_sleep: Duration,

    /// specific runner for this state machine
    runner: Arc<dyn AggregatorRunnerTrait>,
}

impl AggregatorRuntime {
    /// Create a new instance of the state machine.
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
                match &e {
                    RuntimeError::Critical {
                        message: _,
                        nested_error: _,
                    } => {
                        crit!("state machine: a critical error occurred: {e}");

                        return Err(e);
                    }
                    _ => {
                        warn!("state machine: cycle returned an error: '{e}'.");
                    }
                }
            }

            info!(
                "… Cycle finished, Sleeping for {} ms",
                self.state_sleep.as_millis()
            );
            sleep(self.state_sleep).await;
        }
    }

    /// Perform one tick of the state machine.
    pub async fn cycle(&mut self) -> Result<(), RuntimeError> {
        info!("================================================================================");
        info!("STATE MACHINE: new cycle: {}", self.state);

        match self.state.clone() {
            AggregatorState::Idle(state) => {
                let chain_beacon = self.runner.get_beacon_from_chain().await?;

                if state.current_beacon.is_none()
                    || chain_beacon
                        .compare_to_older(state.current_beacon.as_ref().unwrap())
                        .map_err(|e|
                            RuntimeError::keep_state(
                                &format!("Beacon in the state ({:?}) is newer than the beacon read on chain '{:?})", state.current_beacon, chain_beacon), Some(e.into())))?
                        .is_new_beacon()
                {
                    info!(
                        "→ new Beacon settings found, trying to transition to READY";
                        "new_beacon" => ?chain_beacon
                    );

                    if self
                        .try_transition_from_idle_to_ready(
                            state.current_beacon,
                            chain_beacon.clone(),
                        )
                        .await?
                    {
                        self.state = AggregatorState::Ready(ReadyState {
                            current_beacon: chain_beacon,
                        });
                    } else {
                        info!(" ⋅ could not transition from IDLE to READY");
                    }
                } else {
                    info!(" ⋅ Beacon didn't change, waiting…")
                }
            }
            AggregatorState::Ready(state) => {
                let chain_beacon: Beacon = self.runner.get_beacon_from_chain().await?;

                if chain_beacon
                    .compare_to_older(&state.current_beacon)
                    .map_err(|e|
                            RuntimeError::keep_state(
                                &format!("Beacon in the state ({:?}) is newer than the beacon read on chain '{:?})", state.current_beacon, chain_beacon), Some(e.into())))?
                    .is_new_epoch()
                {
                    // transition READY > IDLE
                    info!("→ Epoch has changed, transitioning to IDLE"; "new_beacon" => ?chain_beacon);
                    self.state = AggregatorState::Idle(IdleState {
                        current_beacon: Some(state.current_beacon),
                    });
                } else if self
                    .runner
                    .does_certificate_exist_for_beacon(&state.current_beacon)
                    .await?
                {
                    // READY > READY
                    info!(
                        " ⋅ a certificate already exists for this beacon, waiting…";
                        "beacon" => ?state.current_beacon
                    );
                    self.state = AggregatorState::Ready(ReadyState {
                        current_beacon: chain_beacon,
                    });
                } else {
                    // transition READY > SIGNING
                    info!("→ transitioning to SIGNING");
                    let new_state = self
                        .transition_from_ready_to_signing(state.current_beacon)
                        .await?;
                    self.state = AggregatorState::Signing(new_state);
                }
            }
            AggregatorState::Signing(state) => {
                let chain_beacon: Beacon = self.runner.get_beacon_from_chain().await?;

                if chain_beacon
                    .compare_to_older(&state.current_beacon)
                    .map_err(|e|
                            RuntimeError::keep_state(
                                &format!("Beacon in the state ({:?}) is newer than the beacon read on chain '{:?})", state.current_beacon, chain_beacon), Some(e.into())))?
                    .is_new_beacon()
                {
                    info!("→ Beacon changed, transitioning to IDLE"; "new_beacon" => ?chain_beacon);
                    let new_state = self
                        .transition_from_signing_to_idle_new_beacon(state)
                        .await?;
                    self.state = AggregatorState::Idle(new_state);
                } else {
                    let new_state = self
                            .transition_from_signing_to_idle_multisignature(state)
                            .await?;
                        info!("→ a multi-signature have been created, build a snapshot & a certificate and transitioning back to IDLE");
                        self.state = AggregatorState::Idle(new_state);
                }
            }
        }
        Ok(())
    }

    /// Perform a transition from `IDLE` state to `READY` state when
    /// the certificate chain is valid.
    async fn try_transition_from_idle_to_ready(
        &mut self,
        maybe_current_beacon: Option<Beacon>,
        new_beacon: Beacon,
    ) -> Result<bool, RuntimeError> {
        trace!("trying transition from IDLE to READY state");

        self.runner.update_beacon(&new_beacon).await?;

        if maybe_current_beacon.is_none() || maybe_current_beacon.unwrap().epoch < new_beacon.epoch
        {
            self.runner.close_signer_registration_round().await?;
            self.runner
                .update_era_checker(&new_beacon)
                .await
                .map_err(|e| RuntimeError::critical("transiting IDLE → READY", Some(e)))?;
            self.runner
                .certifier_inform_new_epoch(&new_beacon.epoch)
                .await?;
            self.runner.update_stake_distribution(&new_beacon).await?;
            self.runner
                .open_signer_registration_round(&new_beacon)
                .await?;
            self.runner
                .update_protocol_parameters_in_multisigner(&new_beacon)
                .await?;
        }

        let is_chain_valid = self.runner.is_certificate_chain_valid().await?;
        if !is_chain_valid {
            warn!(" > the certificate chain is invalid");
        }
        Ok(is_chain_valid)
    }

    /// Perform a transition from `SIGNING` state to `IDLE` state when a new
    /// multi-signature is issued.
    async fn transition_from_signing_to_idle_multisignature(
        &self,
        state: SigningState,
    ) -> Result<IdleState, RuntimeError> {
        trace!("launching transition from SIGNING to IDLE state");
        // TODO: Temporary, we need to compute the signed entity type from the current open message
        let signed_entity_type =
            SignedEntityType::CardanoImmutableFilesFull(state.current_beacon.clone());
        let certificate = self
            .runner
            .create_certificate(&signed_entity_type)
            .await?
            .ok_or_else(|| RuntimeError::KeepState {
                message: "not enough signature yet to create a certificate, waiting…".to_string(),
                nested_error: None,
            })?;

        self.runner.drop_pending_certificate().await?;
        let ongoing_snapshot = self
            .runner
            .create_snapshot_archive(&state.current_beacon)
            .await?;
        let locations = self
            .runner
            .upload_snapshot_archive(&ongoing_snapshot)
            .await?;

        let _ = self
            .runner
            .create_and_save_snapshot(certificate, &ongoing_snapshot, locations)
            .await?;

        Ok(IdleState {
            current_beacon: Some(state.current_beacon),
        })
    }

    /// Perform a transition from `SIGNING` state to `IDLE` state when a new
    /// beacon is detected.
    async fn transition_from_signing_to_idle_new_beacon(
        &self,
        state: SigningState,
    ) -> Result<IdleState, RuntimeError> {
        trace!("launching transition from SIGNING to IDLE state");
        self.runner.drop_pending_certificate().await?;

        Ok(IdleState {
            current_beacon: Some(state.current_beacon),
        })
    }

    /// Perform a transition from `READY` state to `SIGNING` state when a new
    /// beacon is detected.
    async fn transition_from_ready_to_signing(
        &mut self,
        new_beacon: Beacon,
    ) -> Result<SigningState, RuntimeError> {
        trace!("launching transition from READY to SIGNING state");
        // TODO: Temporary, we need to compute the signed entity type from the current open message
        let signed_entity_type = SignedEntityType::CardanoImmutableFilesFull(new_beacon.clone());
        self.runner.update_beacon(&new_beacon).await?;

        let digester_result = self.runner.compute_digest(&new_beacon).await?;
        let protocol_message = self
            .runner
            .update_message_in_multisigner(digester_result)
            .await?;
        let _open_message = self
            .runner
            .create_open_message(&signed_entity_type, &protocol_message)
            .await?;
        let certificate_pending = self
            .runner
            .create_new_pending_certificate_from_multisigner(
                new_beacon.clone(),
                &signed_entity_type,
            )
            .await?;
        self.runner
            .save_pending_certificate(certificate_pending.clone())
            .await?;
        let working_certificate = self
            .runner
            .create_new_working_certificate(&certificate_pending)
            .await?;
        let state = SigningState {
            current_beacon: new_beacon,
            working_certificate,
        };

        Ok(state)
    }
}

#[cfg(test)]
mod tests {

    use crate::database::provider::OpenMessage;

    use super::super::runner::MockAggregatorRunner;
    use super::*;

    use mithril_common::entities::ProtocolMessage;
    use mithril_common::era::UnsupportedEraError;
    use mithril_common::test_utils::fake_data;
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
            .expect_get_beacon_from_chain()
            .once()
            .returning(|| Ok(fake_data::beacon()));
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
    pub async fn idle_check_certificate_chain_is_not_valid() {
        let mut runner = MockAggregatorRunner::new();
        runner
            .expect_get_beacon_from_chain()
            .once()
            .returning(|| Ok(fake_data::beacon()));
        runner
            .expect_update_stake_distribution()
            .with(predicate::eq(fake_data::beacon()))
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
            .expect_update_protocol_parameters_in_multisigner()
            .with(predicate::eq(fake_data::beacon()))
            .once()
            .returning(|_| Ok(()));
        runner
            .expect_update_beacon()
            .with(predicate::eq(fake_data::beacon()))
            .once()
            .returning(|_| Ok(()));
        runner
            .expect_is_certificate_chain_valid()
            .once()
            .returning(|| Ok(false));
        runner
            .expect_update_era_checker()
            .with(predicate::eq(fake_data::beacon()))
            .once()
            .returning(|_| Ok(()));
        runner
            .expect_certifier_inform_new_epoch()
            .with(predicate::eq(fake_data::beacon().epoch))
            .once()
            .returning(|_| Ok(()));

        let mut runtime = init_runtime(
            Some(AggregatorState::Idle(IdleState {
                current_beacon: None,
            })),
            runner,
        )
        .await;
        runtime.cycle().await.unwrap();

        assert_eq!("idle".to_string(), runtime.get_state());
    }

    #[tokio::test]
    pub async fn idle_check_certificate_chain_is_valid() {
        let mut runner = MockAggregatorRunner::new();
        runner
            .expect_get_beacon_from_chain()
            .once()
            .returning(|| Ok(fake_data::beacon()));
        runner
            .expect_update_stake_distribution()
            .with(predicate::eq(fake_data::beacon()))
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
            .expect_update_protocol_parameters_in_multisigner()
            .with(predicate::eq(fake_data::beacon()))
            .once()
            .returning(|_| Ok(()));
        runner
            .expect_update_beacon()
            .with(predicate::eq(fake_data::beacon()))
            .once()
            .returning(|_| Ok(()));
        runner
            .expect_is_certificate_chain_valid()
            .once()
            .returning(|| Ok(true));
        runner
            .expect_update_era_checker()
            .with(predicate::eq(fake_data::beacon()))
            .once()
            .returning(|_| Ok(()));
        runner
            .expect_certifier_inform_new_epoch()
            .with(predicate::eq(fake_data::beacon().epoch))
            .once()
            .returning(|_| Ok(()));

        let mut runtime = init_runtime(
            Some(AggregatorState::Idle(IdleState {
                current_beacon: None,
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
        let beacon = fake_data::beacon();
        let new_beacon = Beacon {
            epoch: beacon.epoch + 1,
            ..beacon.clone()
        };
        runner
            .expect_get_beacon_from_chain()
            .once()
            .returning(move || Ok(new_beacon.clone()));
        let mut runtime = init_runtime(
            Some(AggregatorState::Ready(ReadyState {
                current_beacon: beacon,
            })),
            runner,
        )
        .await;
        runtime.cycle().await.unwrap();

        assert_eq!("idle".to_string(), runtime.get_state());
    }

    #[tokio::test]
    pub async fn ready_certificate_already_exist_for_beacon() {
        let mut runner = MockAggregatorRunner::new();
        let beacon = fake_data::beacon();
        let next_beacon = Beacon {
            immutable_file_number: beacon.immutable_file_number + 1,
            ..beacon.clone()
        };
        let expected_beacon = next_beacon.clone();
        runner
            .expect_get_beacon_from_chain()
            .once()
            .returning(move || Ok(next_beacon.clone()));
        runner
            .expect_does_certificate_exist_for_beacon()
            .once()
            .returning(|_| Ok(true));
        let mut runtime = init_runtime(
            Some(AggregatorState::Ready(ReadyState {
                current_beacon: beacon.clone(),
            })),
            runner,
        )
        .await;
        runtime.cycle().await.unwrap();

        assert_eq!("ready".to_string(), runtime.get_state());
        assert_eq!(
            AggregatorState::Ready(ReadyState {
                current_beacon: expected_beacon,
            }),
            runtime.state
        );
    }

    #[tokio::test]
    pub async fn ready_certificate_does_not_exist_for_beacon() {
        let mut runner = MockAggregatorRunner::new();
        runner
            .expect_get_beacon_from_chain()
            .once()
            .returning(|| Ok(fake_data::beacon()));
        runner
            .expect_does_certificate_exist_for_beacon()
            .once()
            .returning(|_| Ok(false));
        runner
            .expect_compute_digest()
            .once()
            .returning(|_| Ok("whatever".to_string()));
        runner
            .expect_update_beacon()
            .with(predicate::eq(fake_data::beacon()))
            .once()
            .returning(|_| Ok(()));
        runner
            .expect_update_message_in_multisigner()
            .with(predicate::eq("whatever".to_string()))
            .once()
            .returning(|_| Ok(ProtocolMessage::new()));
        runner
            .expect_create_new_pending_certificate_from_multisigner()
            //.with(predicate::eq(fake_data::beacon()))
            .once()
            .returning(|_, _| Ok(fake_data::certificate_pending()));
        runner
            .expect_create_new_working_certificate()
            .once()
            .returning(|_| Ok(WorkingCertificate::fake()));
        runner
            .expect_save_pending_certificate()
            .once()
            .returning(|_| Ok(()));
        runner
            .expect_create_open_message()
            .once()
            .returning(|_, _| Ok(OpenMessage::dummy()));

        let mut runtime = init_runtime(
            Some(AggregatorState::Ready(ReadyState {
                current_beacon: fake_data::beacon(),
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
            .expect_get_beacon_from_chain()
            .once()
            .returning(|| Ok(fake_data::beacon()));
        runner
            .expect_drop_pending_certificate()
            .once()
            .returning(|| Ok(Some(fake_data::certificate_pending())));

        let state = SigningState {
            // this current beacon must be outdated so the state machine will
            // return to idle state
            current_beacon: {
                let mut beacon = fake_data::beacon();
                beacon.immutable_file_number -= 1;

                beacon
            },
            working_certificate: WorkingCertificate::fake(),
        };
        let mut runtime = init_runtime(Some(AggregatorState::Signing(state)), runner).await;
        runtime.cycle().await.unwrap();

        assert_eq!("idle".to_string(), runtime.get_state());
    }

    #[tokio::test]
    async fn signing_multisig_is_not_created() {
        let mut runner = MockAggregatorRunner::new();
        runner
            .expect_get_beacon_from_chain()
            .once()
            .returning(|| Ok(fake_data::beacon()));
        runner
            .expect_create_certificate()
            .once()
            .returning(|_| Ok(None));
        let state = SigningState {
            current_beacon: fake_data::beacon(),
            working_certificate: WorkingCertificate::fake(),
        };
        let mut runtime = init_runtime(Some(AggregatorState::Signing(state)), runner).await;
        runtime
            .cycle()
            .await
            .expect_err("cycle should have returned an error");

        assert_eq!("signing".to_string(), runtime.get_state());
    }

    /* TODO: create a fake certificate to test the certificate creation.
    #[tokio::test]
    async fn signing_multisig_is_created() {
        let (certificate_chain, _) = setup_certificate_chain(5, 1);
        let first_certificate = certificate_chain[0].clone();
        let multi_signature: ProtocolMultiSignature =
            key_decode_hex(&first_certificate.multi_signature as &HexEncodedKey).unwrap();
        let mut runner = MockAggregatorRunner::new();
        runner
            .expect_get_beacon_from_chain()
            .once()
            .returning(|| Ok(fake_data::beacon()));
        runner
            .expect_create_certificate()
            .return_once(move |_| Ok(Some(multi_signature)));
        runner
            .expect_drop_pending_certificate()
            .once()
            .returning(|| Ok(Some(fake_data::certificate_pending())));
        runner
            .expect_create_snapshot_archive()
            .once()
            .returning(|_| {
                Ok(OngoingSnapshot::new(
                    Path::new("/tmp/archive.zip").to_path_buf(),
                    1234,
                ))
            });
        runner
            .expect_upload_snapshot_archive()
            .once()
            .returning(|_path| Ok(vec!["locA".to_string(), "locB".to_string()]));
        runner
            .expect_create_and_save_certificate()
            .once()
            .returning(|_, _| Ok(fake_data::certificate("whatever".to_string())));
        runner
            .expect_create_and_save_snapshot()
            .once()
            .returning(|_, _, _| Ok(fake_data::snapshots(1)[0].clone()));

        let state = SigningState {
            current_beacon: fake_data::beacon(),
            working_certificate: WorkingCertificate::fake(),
        };
        let mut runtime = init_runtime(Some(AggregatorState::Signing(state)), runner).await;
        runtime.cycle().await.unwrap();

        assert_eq!("idle".to_string(), runtime.get_state());
    }
    */

    #[tokio::test]
    pub async fn critical_error() {
        let mut runner = MockAggregatorRunner::new();
        runner
            .expect_get_beacon_from_chain()
            .once()
            .returning(|| Ok(fake_data::beacon()));
        runner
            .expect_update_beacon()
            .with(predicate::eq(fake_data::beacon()))
            .once()
            .returning(|_| Ok(()));
        runner
            .expect_update_era_checker()
            .with(predicate::eq(fake_data::beacon()))
            .once()
            .returning(|_| Err(UnsupportedEraError::new("whatever").into()));
        runner
            .expect_close_signer_registration_round()
            .once()
            .returning(|| Ok(()));

        let mut runtime = init_runtime(
            Some(AggregatorState::Idle(IdleState {
                current_beacon: None,
            })),
            runner,
        )
        .await;
        runtime.cycle().await.unwrap_err();

        assert_eq!("idle".to_string(), runtime.get_state());
    }
}
