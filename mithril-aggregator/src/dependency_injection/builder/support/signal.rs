use tokio::sync::watch;

use crate::dependency_injection::{DependenciesBuilder, Result};
use crate::get_dependency;

impl DependenciesBuilder {
    /// Builds a stop signal channel
    pub async fn build_stop_signal_channel(
        &mut self,
    ) -> Result<(watch::Sender<()>, watch::Receiver<()>)> {
        Ok(watch::channel(()))
    }

    /// Get the stop signal channel
    pub async fn get_stop_signal_channel(
        &mut self,
    ) -> Result<(watch::Sender<()>, watch::Receiver<()>)> {
        get_dependency!(self.stop_signal_channel)
    }
}
