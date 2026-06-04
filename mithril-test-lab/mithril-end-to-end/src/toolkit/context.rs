use std::time::Duration;

#[derive(Debug, Clone, Default)]
pub struct ScenarioToolkitContext {
    attempt_policy: AttemptPolicy,
}

impl ScenarioToolkitContext {
    pub fn new(attempt_policy: AttemptPolicy) -> Self {
        Self { attempt_policy }
    }

    pub fn with_base_duration(base_duration: Duration) -> Self {
        Self::new(AttemptPolicy::new(base_duration))
    }

    pub fn attempt_policy(&self) -> AttemptPolicy {
        self.attempt_policy
    }

    pub fn short_delay(self) -> Duration {
        self.attempt_policy.delay(1)
    }

    pub fn artifact_delay(self) -> Duration {
        self.attempt_policy.delay(2)
    }

    pub fn long_delay(self) -> Duration {
        self.attempt_policy.delay(5)
    }
}

#[derive(Debug, Clone, Copy)]
pub struct AttemptPolicy {
    base_duration: Duration,
}

impl AttemptPolicy {
    pub const fn new(base_duration: Duration) -> Self {
        Self { base_duration }
    }

    pub fn delay(self, multiplier: u32) -> Duration {
        self.base_duration * multiplier
    }
}

impl Default for AttemptPolicy {
    fn default() -> Self {
        Self::new(Duration::from_secs(1))
    }
}
