use std::time::Duration;

#[derive(Debug, Clone)]
pub struct ScenarioToolkitContext {
    attempt_policy: AttemptPolicy,
}

impl ScenarioToolkitContext {
    pub fn new(attempt_policy: AttemptPolicy) -> Self {
        Self { attempt_policy }
    }

    pub fn new_from_cardano_epoch(slot_length_in_s: f64, number_of_slot_per_epoch: f64) -> Self {
        Self {
            attempt_policy: AttemptPolicy::from_cardano_epoch(
                slot_length_in_s,
                number_of_slot_per_epoch,
            ),
        }
    }

    pub fn attempt_policy(&self) -> AttemptPolicy {
        self.attempt_policy
    }

    pub fn tenth_epoch_delay(&self) -> Duration {
        self.attempt_policy.delay(0.10)
    }

    pub fn half_epoch_delay(&self) -> Duration {
        self.attempt_policy.delay(0.5)
    }

    pub fn full_epoch_delay(&self) -> Duration {
        self.attempt_policy.epoch_duration
    }
}

#[derive(Debug, Clone, Copy)]
pub struct AttemptPolicy {
    epoch_duration: Duration,
}

impl AttemptPolicy {
    pub const fn new(base_duration: Duration) -> Self {
        Self {
            epoch_duration: base_duration,
        }
    }

    pub fn from_cardano_epoch(slot_length_in_s: f64, number_of_slot_per_epoch: f64) -> Self {
        Self::new(Duration::from_secs_f64(
            slot_length_in_s * number_of_slot_per_epoch,
        ))
    }

    pub fn delay(self, multiplier: f32) -> Duration {
        self.epoch_duration.mul_f32(multiplier)
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
    fn delay_calculation() {
        let policy = AttemptPolicy::new(Duration::from_secs(10));
        assert_eq!(policy.delay(0.5), Duration::from_secs(5));
        assert_eq!(policy.delay(2.0), Duration::from_secs(20));
    }
}
