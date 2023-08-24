use indicatif::{MultiProgress, ProgressBar, ProgressDrawTarget};
use mithril_common::StdResult;
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
    TTY,
    /// No output
    Hidden,
}

impl From<ProgressOutputType> for ProgressDrawTarget {
    fn from(value: ProgressOutputType) -> Self {
        match value {
            ProgressOutputType::JsonReporter => ProgressDrawTarget::hidden(),
            ProgressOutputType::TTY => ProgressDrawTarget::stdout(),
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
    pub fn report_step(&self, step_number: u16, text: &str) -> StdResult<()> {
        match self.output_type {
            ProgressOutputType::JsonReporter => println!(
                r#"{{"step_num": {step_number}, "total_steps": {}, "message": "{text}"}}"#,
                self.number_of_steps
            ),
            ProgressOutputType::TTY => self
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

/// Wrapper of a indicatif [ProgressBar] to allow reporting to json.
pub struct DownloadProgressReporter {
    progress_bar: ProgressBar,
    output_type: ProgressOutputType,
    last_json_report_instant: RwLock<Option<Instant>>,
}

impl DownloadProgressReporter {
    /// Instanciate a new progress reporter
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
                println!(
                    r#"{{ "bytesDownloaded": {}, "bytesTotal": {}, "secondsLeft": {}.{}, "secondsElapsed": {}.{} }}"#,
                    self.progress_bar.position(),
                    self.progress_bar.length().unwrap_or(0),
                    self.progress_bar.eta().as_secs(),
                    self.progress_bar.eta().subsec_millis(),
                    self.progress_bar.elapsed().as_secs(),
                    self.progress_bar.elapsed().subsec_millis(),
                );

                match self.last_json_report_instant.write() {
                    Ok(mut instant) => *instant = Some(Instant::now()),
                    Err(error) => {
                        warn!("failed to update last json report instant, error: {error:?}")
                    }
                };
            }
        };
    }

    fn get_remaining_time_since_last_json_report(&self) -> Option<Duration> {
        match self.last_json_report_instant.read() {
            Ok(instant) => (*instant).map(|instant| instant.elapsed()),
            Err(_) => None,
        }
    }
}
