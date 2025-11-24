mod mithril_command;
#[macro_use]
mod spec_utils;
mod compatibility_checker;
mod file_utils;
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

fn get_process_path(
    bin_name: &str,
    bin_dir: &std::path::Path,
) -> mithril_common::StdResult<std::path::PathBuf> {
    use anyhow::Context;

    let process_path = bin_dir
        .canonicalize()
        .with_context(|| {
            format!(
                "expected '{bin_dir}/{bin_name}' to be an existing executable",
                bin_dir = bin_dir.display(),
            )
        })?
        .join(bin_name);

    if !process_path.exists() {
        anyhow::bail!(
            "cannot find '{bin_name}' executable in expected location '{bin_dir}'",
            bin_dir = bin_dir.display(),
        );
    }

    Ok(process_path)
}
