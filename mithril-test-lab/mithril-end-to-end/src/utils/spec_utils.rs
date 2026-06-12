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

#[macro_export]
macro_rules! attempt {
    ( $remaining_attempts:expr, $sleep_duration:expr, $block:block, until $predicate:expr ) => {{
        let mut remaining_attempts = $remaining_attempts;
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

            if remaining_attempts > 1 {
                tokio::time::sleep($sleep_duration).await;
                remaining_attempts -= 1;
                continue;
            }

            break match last_received_response {
                Some(value) => $crate::utils::AttemptResult::Timeout(
                    $crate::utils::TimeoutReason::PredicateNotSatisfied(value),
                ),
                None => {
                    $crate::utils::AttemptResult::Timeout($crate::utils::TimeoutReason::NoResponse)
                }
            };
        }
    }};
    ( $remaining_attempts:expr, $sleep_duration:expr, $block:block ) => {{
        $crate::attempt!(
            $remaining_attempts,
            $sleep_duration,
            $block,
            until | _ | true
        )
    }};
}

#[cfg(test)]
mod tests {
    use std::time::Duration;
    use tokio::time::Instant;

    use super::*;

    const EMPTY_RESULT: Result<Option<()>, String> = Ok(None);

    #[tokio::test]
    async fn returns_ok_value() {
        let expected = "correct";

        assert_eq!(
            AttemptResult::Ok(expected),
            attempt!(1, Duration::from_millis(2), {
                let result: Result<Option<&str>, String> = Ok(Some(expected));
                result
            })
        );
    }

    #[tokio::test]
    async fn returns_ok_when_response_satisfies_predicate() {
        assert_eq!(
            AttemptResult::Ok(42),
            attempt!(
                3,
                Duration::from_millis(2),
                {
                    let result: Result<Option<i32>, String> = Ok(Some(42));
                    result
                },
                until |value: &i32| *value == 42
            )
        );
    }

    #[tokio::test]
    async fn retries_until_response_satisfies_predicate() {
        let mut attempt_count = 0;
        assert_eq!(
            AttemptResult::Ok(3),
            attempt!(
                5,
                Duration::from_millis(2),
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
    async fn timeout_is_no_response_when_no_value_was_received() {
        assert_eq!(
            AttemptResult::Timeout(TimeoutReason::NoResponse),
            attempt!(1, Duration::from_millis(2), { EMPTY_RESULT })
        );
    }

    #[tokio::test]
    async fn timeout_when_no_response_satisfies_predicate() {
        let mut attempt_count = 0;
        assert_eq!(
            AttemptResult::Timeout(TimeoutReason::PredicateNotSatisfied(3)),
            attempt!(
                3,
                Duration::from_millis(2),
                {
                    attempt_count += 1;
                    let result: Result<Option<i32>, String> = Ok(Some(attempt_count));
                    result
                },
                until | _ | false
            )
        );
    }

    #[tokio::test]
    async fn returns_after_error() {
        assert_eq!(
            AttemptResult::Err("test error".to_string()),
            attempt!(1, Duration::from_millis(2), {
                let err_result: Result<Option<()>, String> = Err("test error".to_string());
                err_result
            })
        );
    }

    #[tokio::test]
    async fn do_the_expected_number_of_loop() {
        let expected_number_of_loop = 5;
        let mut number_of_loop = 0;

        assert_eq!(
            AttemptResult::Timeout(TimeoutReason::NoResponse),
            attempt!(expected_number_of_loop, Duration::from_millis(2), {
                number_of_loop += 1;
                EMPTY_RESULT
            })
        );
        assert_eq!(expected_number_of_loop, number_of_loop);
    }

    #[tokio::test]
    async fn wait_for_the_expected_time() {
        let now = Instant::now();

        assert_eq!(
            AttemptResult::Timeout(TimeoutReason::NoResponse),
            // Note: the attempt! macro wait only after the first loop so we must attempt to two times
            // to have it wait its given duration once.
            attempt!(2, Duration::from_millis(10), { EMPTY_RESULT })
        );

        let elapsed = now.elapsed().as_millis();
        assert!(
            (10..=30).contains(&elapsed),
            "Failure, after one loop the elapsed time was not ~10ms, elapsed: {elapsed}"
        );
    }
}
