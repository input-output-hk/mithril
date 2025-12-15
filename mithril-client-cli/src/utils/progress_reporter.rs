use std::{
    fmt::Write,
    ops::Deref,
    sync::{Arc, RwLock},
    time::{Duration, Instant},
};

use chrono::Utc;
use indicatif::{MultiProgress, ProgressBar, ProgressDrawTarget, ProgressStyle};
use mithril_client::MithrilResult;
use slog::{Logger, warn};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
/// Output type of a [ProgressPrinter] or a [DownloadProgressReporter]
pub enum ProgressOutputType {
    /// Output to json
    JsonReporter,
    /// Output to tty
    Tty,
    /// No output
    Hidden,
}

impl From<ProgressOutputType> for ProgressDrawTarget {
    fn from(value: ProgressOutputType) -> Self {
        match value {
            ProgressOutputType::JsonReporter => ProgressDrawTarget::hidden(),
            ProgressOutputType::Tty => ProgressDrawTarget::stderr(),
            ProgressOutputType::Hidden => ProgressDrawTarget::hidden(),
        }
    }
}

/// Kind of progress bar
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ProgressBarKind {
    Bytes,
    Files,
}

/// Wrapper of a indicatif [MultiProgress] to allow reporting to json.
pub struct ProgressPrinter {
    multi_progress: MultiProgress,
    output_type: ProgressOutputType,
    number_of_steps: u16,
}

impl ProgressPrinter {
    /// Instantiate a new progress printer
    pub fn new(output_type: ProgressOutputType, number_of_steps: u16) -> Self {
        Self {
            multi_progress: MultiProgress::with_draw_target(output_type.into()),
            output_type,
            number_of_steps,
        }
    }

    /// Return the output type of the progress printer
    pub fn output_type(&self) -> ProgressOutputType {
        self.output_type
    }

    /// Report the current step
    pub fn report_step(&self, step_number: u16, text: &str) -> MithrilResult<()> {
        match self.output_type {
            ProgressOutputType::JsonReporter => eprintln!(
                r#"{{"timestamp": "{timestamp}", "step_num": {step_number}, "total_steps": {number_of_steps}, "message": "{text}"}}"#,
                timestamp = Utc::now().to_rfc3339(),
                number_of_steps = self.number_of_steps,
            ),
            ProgressOutputType::Tty => self
                .multi_progress
                .println(format!("{step_number}/{} - {text}", self.number_of_steps))?,
            ProgressOutputType::Hidden => (),
        };

        Ok(())
    }

    /// Print a message to the output
    pub fn print_message(&self, text: &str) -> MithrilResult<()> {
        match self.output_type {
            ProgressOutputType::JsonReporter => eprintln!(
                r#"{{"timestamp": "{timestamp}", "message": "{text}"}}"#,
                timestamp = Utc::now().to_rfc3339(),
            ),
            ProgressOutputType::Tty => self.multi_progress.println(text)?,
            ProgressOutputType::Hidden => (),
        };

        Ok(())
    }
}

impl Deref for ProgressPrinter {
    type Target = MultiProgress;

    fn deref(&self) -> &Self::Target {
        &self.multi_progress
    }
}

/// Utility to format a [ProgressBar] status as json
#[derive(Clone)]
pub struct ProgressBarJsonFormatter {
    label: String,
    kind: ProgressBarKind,
}

impl ProgressBarJsonFormatter {
    /// Instantiate a `ProgressBarJsonFormatter`
    pub fn new<T: Into<String>>(label: T, kind: ProgressBarKind) -> Self {
        Self {
            label: label.into(),
            kind,
        }
    }

    /// Get a json formatted string given the progress bar status
    pub fn format(&self, progress_bar: &ProgressBar) -> String {
        ProgressBarJsonFormatter::format_values(
            &self.label,
            self.kind,
            Utc::now().to_rfc3339(),
            progress_bar.position(),
            progress_bar.length().unwrap_or(0),
            progress_bar.eta(),
            progress_bar.elapsed(),
        )
    }

    fn format_values(
        label: &str,
        kind: ProgressBarKind,
        timestamp: String,
        amount_downloaded: u64,
        amount_total: u64,
        duration_left: Duration,
        duration_elapsed: Duration,
    ) -> String {
        let amount_prefix = match kind {
            ProgressBarKind::Bytes => "bytes",
            ProgressBarKind::Files => "files",
        };

        format!(
            r#"{{"label": "{}", "timestamp": "{}", "{}_downloaded": {}, "{}_total": {}, "seconds_left": {}.{:0>3}, "seconds_elapsed": {}.{:0>3}}}"#,
            label,
            timestamp,
            amount_prefix,
            amount_downloaded,
            amount_prefix,
            amount_total,
            duration_left.as_secs(),
            duration_left.subsec_millis(),
            duration_elapsed.as_secs(),
            duration_elapsed.subsec_millis(),
        )
    }
}

/// Wrapper of a indicatif [ProgressBar] to allow reporting to json.
#[derive(Clone)]
pub struct DownloadProgressReporter {
    progress_bar: ProgressBar,
    output_type: ProgressOutputType,
    json_reporter: ProgressBarJsonFormatter,
    last_json_report_instant: Arc<RwLock<Option<Instant>>>,
    logger: Logger,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct DownloadProgressReporterParams {
    pub label: String,
    pub output_type: ProgressOutputType,
    pub progress_bar_kind: ProgressBarKind,
    pub include_label_in_tty: bool,
}

impl DownloadProgressReporterParams {
    pub fn style(&self) -> ProgressStyle {
        ProgressStyle::with_template(&self.style_template())
            .unwrap()
            .with_key(
                "eta",
                |state: &indicatif::ProgressState, w: &mut dyn Write| {
                    write!(w, "{:.1}s", state.eta().as_secs_f64()).unwrap()
                },
            )
            .progress_chars("#>-")
    }

    fn style_template(&self) -> String {
        let label = if self.include_label_in_tty {
            &self.label
        } else {
            ""
        };

        match self.progress_bar_kind {
            ProgressBarKind::Bytes => {
                format!(
                    "{{spinner:.green}} {label} [{{elapsed_precise}}] [{{wide_bar:.cyan/blue}}] {{bytes}}/{{total_bytes}} ({{eta}})"
                )
            }
            ProgressBarKind::Files => {
                format!(
                    "{{spinner:.green}} {label} [{{elapsed_precise}}] [{{wide_bar:.cyan/blue}}] Files: {{human_pos}}/{{human_len}} ({{eta}})"
                )
            }
        }
    }
}

impl DownloadProgressReporter {
    /// Instantiate a new progress reporter
    pub fn new(
        progress_bar: ProgressBar,
        params: DownloadProgressReporterParams,
        logger: Logger,
    ) -> Self {
        progress_bar.set_style(params.style());

        Self {
            progress_bar,
            output_type: params.output_type,
            json_reporter: ProgressBarJsonFormatter::new(&params.label, params.progress_bar_kind),
            last_json_report_instant: Arc::new(RwLock::new(None)),
            logger,
        }
    }

    #[cfg(test)]
    /// Get the kind of the download progress bar
    pub fn kind(&self) -> ProgressBarKind {
        self.json_reporter.kind
    }

    /// Report the current progress, setting the actual position to the given value
    pub fn report(&self, actual_position: u64) {
        self.progress_bar.set_position(actual_position);
        self.report_json_progress();
    }

    /// Report the current progress, incrementing the actual position by the given delta
    pub fn inc(&self, delta: u64) {
        self.progress_bar.inc(delta);
        self.report_json_progress();
    }

    /// Report that the current download is finished and print the given message.
    pub fn finish(&self, message: &str) {
        self.progress_bar.finish_with_message(message.to_string());
    }

    /// Finish the progress bar and clear the line
    pub fn finish_and_clear(&self) {
        self.progress_bar.finish_and_clear();
    }

    fn get_remaining_time_since_last_json_report(&self) -> Option<Duration> {
        match self.last_json_report_instant.read() {
            Ok(instant) => (*instant).map(|instant| instant.elapsed()),
            Err(_) => None,
        }
    }

    fn report_json_progress(&self) {
        if let ProgressOutputType::JsonReporter = self.output_type {
            let should_report = match self.get_remaining_time_since_last_json_report() {
                Some(remaining_time) => remaining_time > Duration::from_millis(333),
                None => true,
            };

            if should_report {
                eprintln!("{}", self.json_reporter.format(&self.progress_bar));

                match self.last_json_report_instant.write() {
                    Ok(mut instant) => *instant = Some(Instant::now()),
                    Err(error) => {
                        warn!(self.logger, "failed to update last json report instant"; "error" => ?error)
                    }
                };
            }
        };
    }

    pub(crate) fn inner_progress_bar(&self) -> &ProgressBar {
        &self.progress_bar
    }
}

#[cfg(test)]
mod tests {
    use std::thread::sleep;

    use indicatif::ProgressBar;
    use serde_json::Value;

    use super::*;

    #[test]
    fn json_reporter_change_downloaded_and_total_key_prefix_based_on_progress_bar_kind() {
        fn run(kind: ProgressBarKind, expected_prefix: &str) {
            let json_string = ProgressBarJsonFormatter::format_values(
                "label",
                kind,
                "timestamp".to_string(),
                0,
                0,
                Duration::from_millis(1000),
                Duration::from_millis(2500),
            );

            assert!(
                json_string.contains(&format!(r#""{expected_prefix}_downloaded":"#)),
                "'{expected_prefix}_downloaded' key not found in json output: {json_string}",
            );
            assert!(
                json_string.contains(&format!(r#""{expected_prefix}_total":"#)),
                "'{expected_prefix}_total' key not found in json output: {json_string}",
            );
        }

        run(ProgressBarKind::Bytes, "bytes");
        run(ProgressBarKind::Files, "files");
    }

    #[test]
    fn json_report_include_label() {
        let json_string = ProgressBarJsonFormatter::format_values(
            "unique_label",
            ProgressBarKind::Bytes,
            "timestamp".to_string(),
            0,
            0,
            Duration::from_millis(7569),
            Duration::from_millis(5124),
        );

        assert!(
            json_string.contains(r#""label": "unique_label""#),
            "Label key and/or value not found in json output: {json_string}",
        );
    }

    #[test]
    fn check_seconds_formatting_in_json_report_with_more_than_100_milliseconds() {
        let json_string = ProgressBarJsonFormatter::format_values(
            "label",
            ProgressBarKind::Bytes,
            "timestamp".to_string(),
            0,
            0,
            Duration::from_millis(7569),
            Duration::from_millis(5124),
        );

        assert!(
            json_string.contains(r#""seconds_left": 7.569"#),
            "Not expected value in json output: {json_string}",
        );
        assert!(
            json_string.contains(r#""seconds_elapsed": 5.124"#),
            "Not expected value in json output: {json_string}",
        );
    }

    #[test]
    fn check_seconds_formatting_in_json_report_with_less_than_100_milliseconds() {
        let json_string = ProgressBarJsonFormatter::format_values(
            "label",
            ProgressBarKind::Bytes,
            "timestamp".to_string(),
            0,
            0,
            Duration::from_millis(7006),
            Duration::from_millis(5004),
        );

        assert!(
            json_string.contains(r#""seconds_left": 7.006"#),
            "Not expected value in json output: {json_string}"
        );
        assert!(
            json_string.contains(r#""seconds_elapsed": 5.004"#),
            "Not expected value in json output: {json_string}"
        );
    }

    #[test]
    fn check_seconds_formatting_in_json_report_with_milliseconds_ending_by_zeros() {
        let json_string = ProgressBarJsonFormatter::format_values(
            "label",
            ProgressBarKind::Bytes,
            "timestamp".to_string(),
            0,
            0,
            Duration::from_millis(7200),
            Duration::from_millis(5100),
        );

        assert!(
            json_string.contains(r#""seconds_left": 7.200"#),
            "Not expected value in json output: {json_string}"
        );
        assert!(
            json_string.contains(r#""seconds_elapsed": 5.100"#),
            "Not expected value in json output: {json_string}"
        );
    }

    #[test]
    fn check_seconds_left_and_elapsed_time_are_used_by_the_formatter() {
        fn format_duration(duration: &Duration) -> String {
            format!("{}.{}", duration.as_secs(), duration.subsec_nanos())
        }
        fn round_at_ms(duration: Duration) -> Duration {
            Duration::from_millis(duration.as_millis() as u64)
        }

        // 4 steps
        let progress_bar = ProgressBar::new(4);
        // 1 step done in 15 ms, left 45ms to finish the 4th steps
        sleep(Duration::from_millis(15));
        progress_bar.set_position(1);

        let duration_left_before = round_at_ms(progress_bar.eta());
        let duration_elapsed_before = round_at_ms(progress_bar.elapsed());

        let json_string =
            ProgressBarJsonFormatter::new("label", ProgressBarKind::Bytes).format(&progress_bar);

        let duration_left_after = round_at_ms(progress_bar.eta());
        let duration_elapsed_after = round_at_ms(progress_bar.elapsed());

        // Milliseconds in json may not be exactly the same as the one we get because of the test duration.
        let delta = 0.1;

        let json_value: Value = serde_json::from_str(&json_string).unwrap();
        let seconds_left = json_value["seconds_left"].as_f64().unwrap();
        let seconds_elapsed = json_value["seconds_elapsed"].as_f64().unwrap();

        // We check that we pass the right values to format checking that time left is 3 times the time elapsed
        assert!(
            seconds_elapsed * 3.0 - delta < seconds_left
                && seconds_left < seconds_elapsed * 3.0 + delta,
            "seconds_left should be close to 3*{} but it's {}.",
            &seconds_elapsed,
            &seconds_left
        );

        let duration_left = Duration::from_secs_f64(seconds_left);
        assert!(
            duration_left_before <= duration_left && duration_left <= duration_left_after,
            "Duration left: {} should be between {} and {}",
            format_duration(&duration_left),
            format_duration(&duration_left_before),
            format_duration(&duration_left_after),
        );

        let duration_elapsed = Duration::from_secs_f64(seconds_elapsed);
        assert!(
            duration_elapsed_before <= duration_elapsed
                && duration_elapsed <= duration_elapsed_after,
            "Duration elapsed: {} should be between {} and {}",
            format_duration(&duration_elapsed),
            format_duration(&duration_elapsed_before),
            format_duration(&duration_elapsed_after),
        );
    }

    #[test]
    fn style_of_download_progress_reporter_when_include_label_in_tty_is_false() {
        let params = DownloadProgressReporterParams {
            label: "label".to_string(),
            output_type: ProgressOutputType::Tty,
            progress_bar_kind: ProgressBarKind::Bytes,
            include_label_in_tty: false,
        };

        let style_template = params.style_template();
        assert!(
            !style_template.contains("label"),
            "Label should not be included in the style template, got: '{style_template}'"
        );
    }

    #[test]
    fn style_of_download_progress_reporter_when_include_label_in_tty_is_true() {
        let params = DownloadProgressReporterParams {
            label: "label".to_string(),
            output_type: ProgressOutputType::Tty,
            progress_bar_kind: ProgressBarKind::Bytes,
            include_label_in_tty: true,
        };

        let style_template = params.style_template();
        assert!(
            style_template.contains("label"),
            "Label should be included in the style template, got: '{style_template}'"
        );
    }
}
