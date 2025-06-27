#[macro_export]
macro_rules! spin_while_waiting {
    ($block:block, $timeout:expr_2021, $wait_message:expr_2021, $timeout_message:expr_2021) => {{
        slog_scope::info!("⇄ {}", $wait_message);
        let progress_bar = indicatif::ProgressBar::new_spinner().with_message($wait_message);

        let spinner = async move {
            loop {
                progress_bar.tick();
                tokio::time::sleep(std::time::Duration::from_millis(50)).await;
            }
        };
        let probe = async move { $block };

        tokio::select! {
        _ = spinner => Err(anyhow::anyhow!("")),
        _ = tokio::time::sleep($timeout) => Err(anyhow::anyhow!($timeout_message)),
        res = probe => res
        }
    }};
}
