mod check;
mod context;
mod exec;
mod wait;

pub use check::*;
pub use context::*;
pub use exec::*;
pub use wait::*;

use std::time::Duration;

#[derive(Debug, Clone, Default)]
pub struct ScenarioToolkit {
    pub check: CheckToolkit,
    pub exec: ExecToolkit,
    pub wait: WaitToolkit,
}

impl ScenarioToolkit {
    pub fn new(context: ScenarioToolkitContext) -> Self {
        Self {
            check: CheckToolkit::new(context.clone()),
            exec: ExecToolkit::new(context.clone()),
            wait: WaitToolkit::new(context),
        }
    }

    pub fn with_base_duration(base_duration: Duration) -> Self {
        Self::new(ScenarioToolkitContext::with_base_duration(base_duration))
    }
}
