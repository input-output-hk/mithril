use std::process::ExitStatus;
use std::time::Duration;

use slog::{Logger, info};
use tokio::io::{AsyncBufReadExt, BufReader};
use tokio::process::Child;
use tokio::time::timeout;

use mithril_common::StdResult;

/// Timeout for joining the output forwarder thread, to avoid potential deadlocks.
const OUTPUT_FORWARDER_JOIN_TIMEOUT: Duration = Duration::from_millis(500);

/// Extension trait over a spawned [tokio Child][Child] to forward its output to a [slog Logger][Logger].
#[async_trait::async_trait]
pub trait ChildLoggerExt {
    /// Run the child to completion, forwarding its stdout/stderr to `logger` line by line.
    ///
    /// The child must have been spawned with `stdout(Stdio::piped())` and `stderr(Stdio::piped())`,
    /// otherwise the corresponding stream is simply ignored.
    ///
    /// This waits for the process to exit AND for all output to be forwarded to slog before
    /// returning the process [ExitStatus](ExitStatus).
    async fn wait_forwarding_output_to_slog(
        self,
        logger: &Logger,
        context: &str,
    ) -> StdResult<ExitStatus>;

    /// Run the child to completion, forwarding its stdout/stderr to `slog_scope` global logger line by line.
    ///
    /// The child must have been spawned with `stdout(Stdio::piped())` and `stderr(Stdio::piped())`,
    /// otherwise the corresponding stream is simply ignored.
    ///
    /// This waits for the process to exit AND for all output to be forwarded to slog before
    /// returning the process [ExitStatus](ExitStatus).
    async fn wait_forwarding_output_to_slog_scope(self, context: &str) -> StdResult<ExitStatus>;
}

#[async_trait::async_trait]
impl ChildLoggerExt for Child {
    async fn wait_forwarding_output_to_slog(
        mut self,
        logger: &Logger,
        context: &str,
    ) -> StdResult<ExitStatus> {
        let stdout_handle = self.stdout.take().map(|stdout| {
            let logger = logger.clone();
            let context = context.to_owned();
            tokio::spawn(async move {
                let mut lines = BufReader::new(stdout).lines();
                while let Ok(Some(line)) = lines.next_line().await {
                    info!(logger, "{line}"; "context" => &context, "stream" => "stdout");
                }
            })
        });

        let stderr_handle = self.stderr.take().map(|stderr| {
            let logger = logger.clone();
            let context = context.to_owned();
            tokio::spawn(async move {
                let mut lines = BufReader::new(stderr).lines();
                while let Ok(Some(line)) = lines.next_line().await {
                    info!(logger, "{line}"; "context" => &context, "stream" => "stderr");
                }
            })
        });

        let status = self.wait().await?;

        // The pipes reach EOF once the child exits, so these tasks finish on their own.
        // We still join them here to guarantee every line is flushed to slog before returning,
        // but only wait briefly to avoid blocking indefinitely.
        if let Some(handle) = stdout_handle {
            let _ = timeout(OUTPUT_FORWARDER_JOIN_TIMEOUT, handle).await;
        }
        if let Some(handle) = stderr_handle {
            let _ = timeout(OUTPUT_FORWARDER_JOIN_TIMEOUT, handle).await;
        }

        Ok(status)
    }

    async fn wait_forwarding_output_to_slog_scope(self, context: &str) -> StdResult<ExitStatus> {
        self.wait_forwarding_output_to_slog(&slog_scope::logger(), context)
            .await
    }
}

#[cfg(unix)]
#[cfg(test)]
mod tests {
    use std::process::Stdio;

    use tokio::process::Command;

    use crate::test::TestLogger;

    use super::*;

    #[tokio::test]
    async fn forwards_stdout_and_stderr_to_slog_and_returns_exit_status() {
        let (logger, log_inspector) = TestLogger::memory();

        let mut command = Command::new("sh");
        command
            .arg("-c")
            .arg("echo hello-standard-output; echo hello-standard-error 1>&2")
            .stdout(Stdio::piped())
            .stderr(Stdio::piped());

        let status = command
            .spawn()
            .expect("failed to spawn test command")
            .wait_forwarding_output_to_slog(&logger, "test-context")
            .await
            .expect("waiting for the child should not fail");

        assert!(status.success());
        assert!(log_inspector.contains_log("hello-standard-output"));
        assert!(log_inspector.contains_log("hello-standard-error"));
        assert!(log_inspector.contains_log("test-context"));
        assert!(log_inspector.contains_log("stdout"));
        assert!(log_inspector.contains_log("stderr"));
    }

    #[tokio::test]
    async fn returns_non_zero_exit_status_and_still_forwards_output() {
        let (logger, log_inspector) = TestLogger::memory();

        let mut command = Command::new("sh");
        command
            .arg("-c")
            .arg("echo before-exit; exit 3")
            .stdout(Stdio::piped())
            .stderr(Stdio::piped());

        let status = command
            .spawn()
            .expect("failed to spawn test command")
            .wait_forwarding_output_to_slog(&logger, "test-context")
            .await
            .expect("waiting for the child should not fail");

        assert_eq!(Some(3), status.code());
        assert!(log_inspector.contains_log("before-exit"));
    }

    #[tokio::test]
    async fn unpiped_stream_are_not_redirected() {
        let (logger, log_inspector) = TestLogger::memory();
        let mut command = Command::new("sh");
        command.arg("-c").arg("echo hello-standard-output");

        let status = command
            .spawn()
            .expect("failed to spawn test command")
            .wait_forwarding_output_to_slog(&logger, "test-context")
            .await
            .expect("waiting for the child should not fail");

        assert!(status.success());
        assert!(log_inspector.to_string().is_empty());
    }
}
