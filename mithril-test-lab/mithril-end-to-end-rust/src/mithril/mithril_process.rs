use slog_scope::info;
use std::collections::HashMap;
use std::path::{Path, PathBuf};
use tokio::io::AsyncWriteExt;
use tokio::process::{Child, Command};

#[derive(Debug)]
pub struct MithrilProcess {
    pub name: String,
    pub log_path: PathBuf,
    pub child: Child,
}

impl MithrilProcess {
    pub fn start(
        name: &str,
        work_dir: &Path,
        bin_dir: &Path,
        env_vars: HashMap<&str, &str>,
        args: &[&str],
    ) -> Result<Self, String> {
        let process_path = bin_dir.canonicalize().unwrap().join(name);
        let log_path = work_dir.join(format!("{}.log", name));

        info!("Starting {}", name; "work_dir" => work_dir.display(), "env" => #?env_vars, "args" => #?args);

        if !process_path.exists() {
            return Err(format!(
                "cannot find {} executable in expected location \"{}\"",
                name,
                bin_dir.display()
            ));
        }

        let log_file_stdout = std::fs::File::create(&log_path).unwrap();
        let log_file_stderr = log_file_stdout.try_clone().unwrap();
        let mut process = Command::new(process_path);
        process
            .current_dir(work_dir)
            .stdin(log_file_stdout)
            .stdout(log_file_stderr)
            .envs(env_vars)
            .args(args)
            .kill_on_drop(true);

        let child = process.spawn().unwrap();

        Ok(MithrilProcess {
            name: name.to_string(),
            child,
            log_path,
        })
    }

    pub async fn dump_logs_if_crashed(&mut self) -> Result<(), String> {
        match self.child.try_wait() {
            Ok(Some(status)) => {
                if !status.success() {
                    self.dump_logs_to_stdout().await?;
                }
                Ok(())
            }
            Ok(None) => Ok(()),
            Err(e) => Err(format!("failed get {} status: {}", &self.name, e)),
        }
    }

    async fn dump_logs_to_stdout(&self) -> Result<(), String> {
        let buffer = tokio::fs::read(&self.log_path).await.map_err(|e| {
            format!(
                "failed to read logfile `{}`: {}",
                self.log_path.display(),
                e
            )
        })?;

        tokio::io::stdout()
            .write_all(&buffer)
            .await
            .map_err(|e| format!("failed to dump {} logs: {}", &self.name, e))?;

        Ok(())
    }
}
