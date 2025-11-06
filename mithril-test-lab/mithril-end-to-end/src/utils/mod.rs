mod mithril_command;
#[macro_use]
mod spec_utils;
mod file_utils;
mod formatting;

pub use formatting::*;
pub use mithril_command::MithrilCommand;
pub use spec_utils::AttemptResult;

pub fn is_running_in_github_actions() -> bool {
    std::env::var("GITHUB_ACTIONS").is_ok()
}
