use std::pin::Pin;
use std::process::ExitStatus;
use std::task::Poll;
use std::time::Duration;

use slog::{Logger, info};
use tokio::io::{AsyncRead, AsyncReadExt, ReadBuf};
use tokio::process::Child;
use tokio::time::timeout;

use mithril_common::StdResult;

/// Timeout for joining the output forwarder thread, to avoid potential deadlocks.
const OUTPUT_FORWARDER_JOIN_TIMEOUT: Duration = Duration::from_millis(500);

const OUTPUT_READ_BUFFER_SIZE: usize = 8 * 1024;

/// Extension trait over a spawned [tokio Child][Child] to forward its output to a [slog Logger][Logger].
#[async_trait::async_trait]
pub trait ChildLoggerExt {
    /// Run the child to completion, forwarding its stdout/stderr to `logger`.
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

    /// Run the child to completion, forwarding its stdout/stderr to `slog_scope` global logger.
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
                forward_output_to_slog(stdout, &logger, &context, "stdout").await;
            })
        });

        let stderr_handle = self.stderr.take().map(|stderr| {
            let logger = logger.clone();
            let context = context.to_owned();
            tokio::spawn(async move {
                forward_output_to_slog(stderr, &logger, &context, "stderr").await;
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

/// Forward an asynchronous output stream to a slog logger.
///
/// The stream is read in batches:
/// - waits for at least one chunk of data;
/// - immediately drains any additional data already available;
/// - logs the collected batch with the given `context` and `stream_name`.
///
/// The function stops when the stream reaches EOF or when a read error occurs.
/// Read errors are ignored and are not logged.
async fn forward_output_to_slog<R>(
    mut stream: R,
    logger: &Logger,
    context: &str,
    stream_name: &'static str,
) where
    R: AsyncRead + Unpin,
{
    let mut pending_logs: Vec<u8> = Vec::with_capacity(OUTPUT_READ_BUFFER_SIZE);
    let mut first_chunk_buffer = vec![0; OUTPUT_READ_BUFFER_SIZE];
    let mut immediate_buffer = vec![0; OUTPUT_READ_BUFFER_SIZE];

    loop {
        pending_logs.clear();

        // Wait for the first chunk of the next burst.
        let bytes_read: usize = match stream.read(&mut first_chunk_buffer).await {
            Ok(0) => break,
            Ok(bytes_read) => bytes_read,
            Err(_) => break,
        };

        pending_logs.extend_from_slice(&first_chunk_buffer[..bytes_read]);

        // Drain whatever is immediately available
        loop {
            let read_result = std::future::poll_fn(|cx| {
                let mut read_buf = ReadBuf::new(&mut immediate_buffer);

                match Pin::new(&mut stream).poll_read(cx, &mut read_buf) {
                    Poll::Ready(Ok(())) => Poll::Ready(Ok(read_buf.filled().len())),
                    Poll::Ready(Err(error)) => Poll::Ready(Err(error)),
                    // No more data immediately available meaning the end of the current batch
                    Poll::Pending => Poll::Ready(Ok(0)),
                }
            })
            .await;

            match read_result {
                Ok(0) => break,
                Ok(bytes_read) => pending_logs.extend_from_slice(&immediate_buffer[..bytes_read]),
                Err(_) => break,
            }
        }

        log_output_batch(logger, context, stream_name, &pending_logs);
    }
}

fn log_output_batch(logger: &Logger, context: &str, stream_name: &'static str, output: &[u8]) {
    let output = String::from_utf8_lossy(output);
    let output = output.trim_end_matches(['\r', '\n']);

    if !output.is_empty() {
        info!(logger, "{output}"; "context" => context, "stream" => stream_name);
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
