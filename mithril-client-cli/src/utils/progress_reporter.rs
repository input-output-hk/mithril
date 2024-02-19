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
            ProgressOutputType::JsonReporter => println!(
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
        format!(
            r#"{{"timestamp": "{}", "bytes_downloaded": {}, "bytes_total": {}, "seconds_left": {}.{:0>3}, "seconds_elapsed": {}.{:0>3}}}"#,
            Utc::now().to_rfc3339(),
            progress_bar.position(),
            progress_bar.length().unwrap_or(0),
            progress_bar.eta().as_secs(),
            progress_bar.eta().subsec_millis(),
            progress_bar.elapsed().as_secs(),
            progress_bar.elapsed().subsec_millis(),
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
                println!("{}", ProgressBarJsonFormatter::format(&self.progress_bar));

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
    fn check_seconds_elapsed_in_json_report_with_more_than_100_milliseconds() {
        let progress_bar = ProgressBar::new(10).with_elapsed(Duration::from_millis(5124));

        let json_string = ProgressBarJsonFormatter::format(&progress_bar);

        assert!(
            json_string.contains(r#""seconds_elapsed": 5.124"#),
            "Not expected value in json output: {}",
            json_string
        );
    }

    #[test]
    fn check_seconds_elapsed_in_json_report_with_less_than_100_milliseconds() {
        let progress_bar = ProgressBar::new(10).with_elapsed(Duration::from_millis(5004));

        let json_string = ProgressBarJsonFormatter::format(&progress_bar);

        assert!(
            json_string.contains(r#""seconds_elapsed": 5.004"#),
            "Not expected value in json output: {}",
            json_string
        );
    }

    #[test]
    fn check_seconds_left_in_json_report_with_more_than_100_milliseconds() {
        let half_position = 5;
        let progress_bar = ProgressBar::new(half_position * 2);
        sleep(Duration::from_millis(123));
        progress_bar.set_position(half_position);
        let json_string = ProgressBarJsonFormatter::format(&progress_bar);

        let milliseconds = progress_bar.eta().subsec_millis();
        assert!(milliseconds > 100);
        assert!(
            json_string.contains(&format!(r#""seconds_left": 0.{}"#, milliseconds)),
            "Not expected value in json output: {}",
            json_string
        );
    }

    #[test]
    fn check_seconds_left_in_json_report_with_less_than_100_milliseconds() {
        let half_position = 5;
        let progress_bar = ProgressBar::new(half_position * 2);
        sleep(Duration::from_millis(1));
        progress_bar.set_position(half_position);
        let json_string = ProgressBarJsonFormatter::format(&progress_bar);

        assert!(
            json_string.contains(r#""seconds_left": 0.0"#),
            "Not expected value in json output: {}",
            json_string
        );

        assert!(
            !json_string.contains(r#""seconds_left": 0.000"#),
            "Not expected value in json output: {}",
            json_string
        );
    }
}
