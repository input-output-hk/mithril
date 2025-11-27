mod mithril_command;
#[macro_use]
mod spec_utils;
mod compatibility_checker;
pub(crate) mod file_utils;
mod formatting;
mod version_req;

pub use compatibility_checker::*;
pub use formatting::*;
pub use mithril_command::MithrilCommand;
pub use spec_utils::AttemptResult;
pub use version_req::NodeVersion;

pub fn is_running_in_github_actions() -> bool {
    std::env::var("GITHUB_ACTIONS").is_ok()
}
