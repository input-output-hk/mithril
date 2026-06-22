use std::time::Duration;

#[derive(Clone, PartialEq, PartialOrd, Eq, Ord, Debug, Hash)]
pub enum AttemptResult<T, E> {
    Ok(T),
    Err(E),
    Timeout(TimeoutReason<T>),
}

#[derive(Clone, PartialEq, PartialOrd, Eq, Ord, Debug, Hash)]
pub enum TimeoutReason<T> {
    /// No response was received after the given number of attempts
    NoResponse,
    /// The predicate was not satisfied after the given number of attempts, containing the last
    /// response received
    PredicateNotSatisfied(T),
}

/// Exponential backoff bounded between a floor and a ceiling, used to space out polling attempts.
#[derive(Debug, Clone, Copy)]
pub struct Backoff {
    /// Delay returned by the next call to [`Backoff::next_delay`].
    current: Duration,
    /// Maximum delay the backoff grows to.
    max: Duration,
    /// Factor applied to the delay after each attempt.
    factor: u32,
}

impl Backoff {
    /// Floor of the polling delay.
    const DEFAULT_MIN: Duration = Duration::from_secs(1);
    /// Ceiling of the polling delay.
    const DEFAULT_MAX: Duration = Duration::from_secs(30);
    /// Growth factor applied after each attempt.
    const DEFAULT_FACTOR: u32 = 2;

    /// Builds a backoff growing from `min` to `max` by the given `factor`.
    ///
    /// A `factor` of `0` is treated as `1` so the delay never collapses to zero.
    pub const fn new(min: Duration, max: Duration, factor: u32) -> Self {
        Self {
            current: min,
            max,
            factor: if factor == 0 { 1 } else { factor },
        }
    }

    /// Returns the current delay, then grows it toward the ceiling.
    pub fn next_delay(&mut self) -> Duration {
        let delay = self.current;
        self.current = (self.current * self.factor).min(self.max);
        delay
    }
}

impl Default for Backoff {
    fn default() -> Self {
        Self::new(Self::DEFAULT_MIN, Self::DEFAULT_MAX, Self::DEFAULT_FACTOR)
    }
}

/// Polls `$block` until it satisfies `$predicate` or `$timeout` elapses, sleeping between attempts
/// according to `$backoff`.
#[macro_export]
macro_rules! poll_until {
    ( $timeout:expr, $backoff:expr, $block:block, until $predicate:expr ) => {{
        let deadline = tokio::time::Instant::now() + $timeout;
        let mut backoff = $backoff;
        let mut last_received_response = None;

        loop {
            let res = $block;

            match res {
                Ok(Some(value)) => {
                    if $predicate(&value) {
                        break $crate::utils::AttemptResult::Ok(value);
                    } else {
                        last_received_response = Some(value);
                    }
                }
                Ok(None) => (),
                Err(error) => break $crate::utils::AttemptResult::Err(error),
            }

            let now = tokio::time::Instant::now();
            if now >= deadline {
                break match last_received_response {
                    Some(value) => $crate::utils::AttemptResult::Timeout(
                        $crate::utils::TimeoutReason::PredicateNotSatisfied(value),
                    ),
                    None => $crate::utils::AttemptResult::Timeout(
                        $crate::utils::TimeoutReason::NoResponse,
                    ),
                };
            }

            tokio::time::sleep(backoff.next_delay().min(deadline - now)).await;
        }
    }};
    ( $timeout:expr, $backoff:expr, $block:block ) => {{ $crate::poll_until!($timeout, $backoff, $block, until | _ | true) }};
}

#[cfg(test)]
mod tests {
    use std::time::Duration;

    use super::*;

    const EMPTY_RESULT: Result<Option<()>, String> = Ok(None);

    #[test]
    fn backoff_grows_until_ceiling() {
        let mut backoff = Backoff::new(Duration::from_secs(1), Duration::from_secs(8), 2);

        assert_eq!(backoff.next_delay(), Duration::from_secs(1));
        assert_eq!(backoff.next_delay(), Duration::from_secs(2));
        assert_eq!(backoff.next_delay(), Duration::from_secs(4));
        assert_eq!(backoff.next_delay(), Duration::from_secs(8));
        assert_eq!(backoff.next_delay(), Duration::from_secs(8));
    }

    #[tokio::test]
    async fn poll_until_returns_ok_value() {
        let expected = "correct";

        assert_eq!(
            AttemptResult::Ok(expected),
            poll_until!(Duration::from_millis(50), Backoff::default(), {
                let result: Result<Option<&str>, String> = Ok(Some(expected));
                result
            })
        );
    }

    #[tokio::test]
    async fn poll_until_retries_until_predicate_satisfied() {
        let mut attempt_count = 0;

        assert_eq!(
            AttemptResult::Ok(3),
            poll_until!(
                Duration::from_millis(500),
                Backoff::new(Duration::from_millis(1), Duration::from_millis(1), 1),
                {
                    attempt_count += 1;
                    let result: Result<Option<i32>, String> = Ok(Some(attempt_count));
                    result
                },
                until |value: &i32| *value == 3
            )
        );
    }

    #[tokio::test]
    async fn poll_until_times_out_when_no_value_received() {
        assert_eq!(
            AttemptResult::Timeout(TimeoutReason::NoResponse),
            poll_until!(
                Duration::from_millis(5),
                Backoff::new(Duration::from_millis(1), Duration::from_millis(1), 1),
                { EMPTY_RESULT }
            )
        );
    }

    #[tokio::test]
    async fn poll_until_times_out_with_last_response_when_predicate_unsatisfied() {
        let result = poll_until!(
            Duration::from_millis(5),
            Backoff::new(Duration::from_millis(1), Duration::from_millis(1), 1),
            {
                let result: Result<Option<i32>, String> = Ok(Some(42));
                result
            },
            until | _ | false
        );

        assert!(matches!(
            result,
            AttemptResult::Timeout(TimeoutReason::PredicateNotSatisfied(42))
        ));
    }

    #[tokio::test]
    async fn poll_until_returns_after_error() {
        assert_eq!(
            AttemptResult::Err("test error".to_string()),
            poll_until!(Duration::from_millis(50), Backoff::default(), {
                let err_result: Result<Option<()>, String> = Err("test error".to_string());
                err_result
            })
        );
    }

    #[test]
    fn backoff_zero_factor_does_not_collapse_delay() {
        let mut backoff = Backoff::new(Duration::from_secs(1), Duration::from_secs(8), 0);

        assert_eq!(backoff.next_delay(), Duration::from_secs(1));
        assert_eq!(backoff.next_delay(), Duration::from_secs(1));
    }

    #[tokio::test]
    async fn poll_until_does_not_sleep_past_the_deadline() {
        let start = std::time::Instant::now();

        let _: AttemptResult<(), String> = poll_until!(
            Duration::from_millis(5),
            Backoff::new(Duration::from_secs(10), Duration::from_secs(10), 1),
            { EMPTY_RESULT }
        );

        assert!(start.elapsed() < Duration::from_secs(1));
    }
}
