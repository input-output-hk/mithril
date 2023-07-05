#[derive(PartialEq, PartialOrd, Eq, Ord, Debug, Hash)]
pub enum AttemptResult<T, E> {
    Ok(T),
    Err(E),
    Timeout(),
}

#[macro_export]
macro_rules! attempt {
    ( $remaining_attempts:expr, $sleep_duration:expr, $block:block ) => {{
        let mut remaining_attempts = $remaining_attempts;
        loop {
            let res = $block;
            if let Ok(None) = res {
                if remaining_attempts > 1 {
                    tokio::time::sleep($sleep_duration).await;
                    remaining_attempts -= 1;
                    continue;
                }
            }

            break match res {
                Ok(Some(value)) => AttemptResult::Ok(value),
                Err(error) => AttemptResult::Err(error),
                Ok(None) => AttemptResult::Timeout(),
            };
        }
    }};
}

#[cfg(test)]
mod tests {
    use crate::attempt;
    use crate::utils::AttemptResult;
    use std::time::Duration;
    use tokio::time::Instant;

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
    async fn can_timeout() {
        assert_eq!(
            AttemptResult::Timeout(),
            attempt!(1, Duration::from_millis(2), { EMPTY_RESULT })
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
            AttemptResult::Timeout(),
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
            AttemptResult::Timeout(),
            // Note: the attempt! macro wait only after the first loop so we must attempt to two times
            // to have it wait its given duration once.
            attempt!(2, Duration::from_millis(10), { EMPTY_RESULT })
        );

        let elapsed = now.elapsed().as_millis();
        assert!(
            (10..=15).contains(&elapsed),
            "Failure, after one loop the elapsed time was not ~10ms, elapsed: {elapsed}"
        );
    }
}
