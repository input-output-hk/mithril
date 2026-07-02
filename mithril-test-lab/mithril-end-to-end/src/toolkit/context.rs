use std::time::Duration;

use crate::utils::Backoff;

/// Context shared across scenario toolkits, carrying the polling policy derived from the Cardano
/// epoch duration.
#[derive(Debug, Clone)]
pub struct ScenarioToolkitContext {
    /// Policy used to derive polling timeouts from the epoch duration.
    attempt_policy: AttemptPolicy,
}

impl ScenarioToolkitContext {
    /// Builds a context from the given attempt policy.
    pub fn new(attempt_policy: AttemptPolicy) -> Self {
        Self { attempt_policy }
    }

    /// Builds a context from the Cardano slot length and number of slots per epoch.
    pub fn new_from_cardano_epoch(slot_length_in_s: f64, number_of_slot_per_epoch: f64) -> Self {
        Self {
            attempt_policy: AttemptPolicy::from_cardano_epoch(
                slot_length_in_s,
                number_of_slot_per_epoch,
            ),
        }
    }

    /// Backoff spacing out polling attempts while waiting for a condition.
    pub fn poll_backoff(&self) -> Backoff {
        Backoff::default()
    }

    /// Timeout covering the given number of Cardano epochs.
    pub fn timeout_for_epochs(&self, epochs: u32) -> Duration {
        self.attempt_policy.timeout_for_epochs(epochs)
    }
}

/// Policy deriving polling timeouts from the Cardano epoch duration.
#[derive(Debug, Clone, Copy)]
pub struct AttemptPolicy {
    /// Wall-clock duration of a single Cardano epoch.
    epoch_duration: Duration,
}

impl AttemptPolicy {
    /// Builds a policy from the given epoch duration.
    pub const fn new(base_duration: Duration) -> Self {
        Self {
            epoch_duration: base_duration,
        }
    }

    /// Builds a policy from the Cardano slot length and number of slots per epoch.
    pub fn from_cardano_epoch(slot_length_in_s: f64, number_of_slot_per_epoch: f64) -> Self {
        Self::new(Duration::from_secs_f64(
            slot_length_in_s * number_of_slot_per_epoch,
        ))
    }

    /// Returns a timeout covering the given number of epochs, saturating at [`Duration::MAX`].
    pub fn timeout_for_epochs(self, epochs: u32) -> Duration {
        self.epoch_duration.checked_mul(epochs).unwrap_or(Duration::MAX)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn building_attempt_policy_from_cardano_epoch() {
        let slot_length_in_s = 0.5;
        let number_of_slot_per_epoch = 10.0;
        let policy = AttemptPolicy::from_cardano_epoch(slot_length_in_s, number_of_slot_per_epoch);
        assert_eq!(policy.epoch_duration, Duration::from_secs(5));
    }

    #[test]
    fn timeout_for_epochs_scales_with_epoch_duration() {
        let policy = AttemptPolicy::new(Duration::from_secs(10));

        assert_eq!(policy.timeout_for_epochs(0), Duration::from_secs(0));
        assert_eq!(policy.timeout_for_epochs(1), Duration::from_secs(10));
        assert_eq!(policy.timeout_for_epochs(5), Duration::from_secs(50));
    }

    #[test]
    fn timeout_for_epochs_saturates_on_overflow() {
        let policy = AttemptPolicy::new(Duration::MAX);

        assert_eq!(policy.timeout_for_epochs(2), Duration::MAX);
    }
}
