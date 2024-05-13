use chrono::Utc;
use indicatif::{MultiProgress, ProgressBar, ProgressDrawTarget};
use mithril_client::MithrilResult;
use slog_scope::warn;
use std::{
    ops::Deref,
    sync::RwLock,
    time::{Duration, Instant},
};

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
            ProgressOutputType::Tty => ProgressDrawTarget::stdout(),
            ProgressOutputType::Hidden => ProgressDrawTarget::hidden(),
        }
    }
}

/// Wrapper of a indicatif [MultiProgress] to allow reporting to json.
pub struct ProgressPrinter {
    multi_progress: MultiProgress,
    output_type: ProgressOutputType,
    number_of_steps: u16,
}

impl ProgressPrinter {
    /// Instanciate a new progress printer
    pub fn new(output_type: ProgressOutputType, number_of_steps: u16) -> Self {
        Self {
            multi_progress: MultiProgress::with_draw_target(output_type.into()),
            output_type,
            number_of_steps,
        }
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
}

impl Deref for ProgressPrinter {
    type Target = MultiProgress;

    fn deref(&self) -> &Self::Target {
        &self.multi_progress
    }
}

/// Utility to format a [ProgressBar] status as json
pub struct ProgressBarJsonFormatter;

impl ProgressBarJsonFormatter {
    /// Get a json formatted string given the progress bar status
    pub fn format(progress_bar: &ProgressBar) -> String {
        ProgressBarJsonFormatter::format_values(
            Utc::now().to_rfc3339(),
            progress_bar.position(),
            progress_bar.length().unwrap_or(0),
            progress_bar.eta(),
            progress_bar.elapsed(),
        )
    }

    fn format_values(
        timestamp: String,
        bytes_downloaded: u64,
        bytes_total: u64,
        duration_left: Duration,
        duration_elapsed: Duration,
    ) -> String {
        format!(
            r#"{{"timestamp": "{}", "bytes_downloaded": {}, "bytes_total": {}, "seconds_left": {}.{:0>3}, "seconds_elapsed": {}.{:0>3}}}"#,
            timestamp,
            bytes_downloaded,
            bytes_total,
            duration_left.as_secs(),
            duration_left.subsec_millis(),
            duration_elapsed.as_secs(),
            duration_elapsed.subsec_millis(),
        )
    }
}

/// Wrapper of a indicatif [ProgressBar] to allow reporting to json.
pub struct DownloadProgressReporter {
    progress_bar: ProgressBar,
    output_type: ProgressOutputType,
    last_json_report_instant: RwLock<Option<Instant>>,
}

impl DownloadProgressReporter {
    /// Instantiate a new progress reporter
    pub fn new(progress_bar: ProgressBar, output_type: ProgressOutputType) -> Self {
        Self {
            progress_bar,
            output_type,
            last_json_report_instant: RwLock::new(None),
        }
    }

    /// Report the current progress
    pub fn report(&self, actual_position: u64) {
        self.progress_bar.set_position(actual_position);

        if let ProgressOutputType::JsonReporter = self.output_type {
            let should_report = match self.get_remaining_time_since_last_json_report() {
                Some(remaining_time) => remaining_time > Duration::from_millis(333),
                None => true,
            };

            if should_report {
                eprintln!("{}", ProgressBarJsonFormatter::format(&self.progress_bar));

                match self.last_json_report_instant.write() {
                    Ok(mut instant) => *instant = Some(Instant::now()),
                    Err(error) => {
                        warn!("failed to update last json report instant, error: {error:?}")
                    }
                };
            }
        };
    }

    /// Report that the current download is finished and print the given message.
    pub fn finish(&self, message: &str) {
        self.progress_bar.finish_with_message(message.to_string());
    }

    fn get_remaining_time_since_last_json_report(&self) -> Option<Duration> {
        match self.last_json_report_instant.read() {
            Ok(instant) => (*instant).map(|instant| instant.elapsed()),
            Err(_) => None,
        }
    }
}

#[cfg(test)]
mod tests {
    use std::thread::sleep;

    use super::*;
    use indicatif::ProgressBar;

    #[test]
    fn check_seconds_formatting_in_json_report_with_more_than_100_milliseconds() {
        let json_string = ProgressBarJsonFormatter::format_values(
            "".to_string(),
            0,
            0,
            Duration::from_millis(7569),
            Duration::from_millis(5124),
        );

        assert!(
            json_string.contains(r#""seconds_left": 7.569"#),
            "Not expected value in json output: {}",
            json_string
        );
        assert!(
            json_string.contains(r#""seconds_elapsed": 5.124"#),
            "Not expected value in json output: {}",
            json_string
        );
    }

    #[test]
    fn check_seconds_formatting_in_json_report_with_less_than_100_milliseconds() {
        let json_string = ProgressBarJsonFormatter::format_values(
            "".to_string(),
            0,
            0,
            Duration::from_millis(7006),
            Duration::from_millis(5004),
        );

        assert!(
            json_string.contains(r#""seconds_left": 7.006"#),
            "Not expected value in json output: {}",
            json_string
        );
        assert!(
            json_string.contains(r#""seconds_elapsed": 5.004"#),
            "Not expected value in json output: {}",
            json_string
        );
    }

    #[test]
    fn check_seconds_formatting_in_json_report_with_milliseconds_ending_by_zeros() {
        let json_string = ProgressBarJsonFormatter::format_values(
            "".to_string(),
            0,
            0,
            Duration::from_millis(7200),
            Duration::from_millis(5100),
        );

        assert!(
            json_string.contains(r#""seconds_left": 7.200"#),
            "Not expected value in json output: {}",
            json_string
        );
        assert!(
            json_string.contains(r#""seconds_elapsed": 5.100"#),
            "Not expected value in json output: {}",
            json_string
        );
    }

    #[test]
    fn check_seconds_left_and_elapsed_time_are_used_by_the_formatter() {
        let expected_milliseconds_left: u128 = 450;
        let expected_milliseconds_elapsed: u128 = 150;

        // 4 steps
        let progress_bar = ProgressBar::new(4);
        // 1 step done in 150 ms, left 450ms to finish the 4th steps
        sleep(Duration::from_millis(expected_milliseconds_elapsed as u64));
        progress_bar.set_position(1);

        let json_string = ProgressBarJsonFormatter::format(&progress_bar);

        let milliseconds_left = progress_bar.eta().as_millis();
        let milliseconds_elapsed = progress_bar.elapsed().as_millis();

        // Milliseconds in json may not be exactly the same as the one we get because of the test duration.
        // We need to have a difference not more than 49ms to keep the same 1 first milliseconds digits.
        let delta = 49;
        // TODO: to remove, it-s just to investigation on CI.
        let error_message = format!(
            "\n- milliseconds_left:{milliseconds_left}\n- milliseconds_elapsed:{milliseconds_elapsed}\n- json_string:{json_string}"
        );

        assert!(
            ((expected_milliseconds_left - delta)..=(expected_milliseconds_left + delta))
                .contains(&milliseconds_left),
            "milliseconds_left should be close to {} but it's {}. {}",
            &expected_milliseconds_left,
            &milliseconds_left,
            &error_message
        );
        assert!(
            json_string.contains(r#""seconds_left": 0.4"#), // Should be close to 0.450
            "Not expected value in json output: {}",
            json_string
        );

        assert!(
            ((expected_milliseconds_elapsed - delta)..=(expected_milliseconds_elapsed + delta))
                .contains(&milliseconds_elapsed),
            "milliseconds_elapsed should be close to {} but it's {}. {}",
            &expected_milliseconds_elapsed,
            &milliseconds_elapsed,
            &error_message
        );
        assert!(
            json_string.contains(r#""seconds_elapsed": 0.1"#), // Should be close to 0.150
            "Not expected value in json output: {}",
            json_string
        );
    }
}
