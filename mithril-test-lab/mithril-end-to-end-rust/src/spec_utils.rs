#[derive(Debug)]
pub enum AttemptResult<T, E> {
    Ok(T),
    Err(E),
    Timeout(),
}

impl<T, E> AttemptResult<T, E> {
    pub fn ok(value: T) -> Self {
        Self::Ok(value)
    }

    pub fn err(error: E) -> Self {
        Self::Err(error)
    }

    pub fn timeout() -> Self {
        Self::Timeout()
    }
}

#[macro_export]
macro_rules! attempt {
    ( $remaining_attempts:expr, $sleep_duration:expr, $block:block ) => {{
        let mut remaining_attempts = $remaining_attempts;
        loop {
            let res = $block;
            if let Ok(None) = res {
                if remaining_attempts > 0 {
                    tokio::time::sleep($sleep_duration).await;
                    remaining_attempts -= 1;
                    continue;
                }
            }

            break match res {
                Ok(Some(value)) => AttemptResult::ok(value),
                Err(error) => AttemptResult::err(error),
                Ok(None) => AttemptResult::timeout(),
            };
        }
    }};
}
