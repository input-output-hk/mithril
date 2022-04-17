use log::error;
use log::info;
use std::process::Command;
use std::sync::mpsc;
use std::sync::mpsc::{Receiver, Sender};
use std::time::Duration;

/// Message sent to Snapshotter
pub enum Snapshot {
    Stop,
}

/// Snapshotter
pub struct Snapshotter {
    /// Interval between each snapshot, in seconds
    interval: u32,

    /// DB directory to snapshot
    db_directory: String,

    /// For sending instructions to the snapshotter
    tx: Sender<Snapshot>,

    /// For receiving messages while snapshotter is running
    rx: Receiver<Snapshot>,
}

pub struct Stopper {
    /// For sending instructions to the snapshotter
    tx: Sender<Snapshot>,
}

#[derive(Debug)]
struct SnapshotError {
    /// Detailed error
    reason: String,
}

impl Snapshotter {
    /// Server factory
    pub fn new(interval: u32) -> Self {
        let (tx, rx) = mpsc::channel();
        Self {
            interval,
            db_directory: "/db".into(),
            rx,
            tx,
        }
    }

    /// Start
    pub fn run(&self) {
        info!("Starting Snapshotter");
        loop {
            match self
                .rx
                .recv_timeout(Duration::from_millis(self.interval.into()))
            {
                Err(_) => {
                    info!("Snapshotting");
                    if let Err(e) = self.snapshot() {
                        error!("{:?}", e)
                    }
                }
                Ok(Snapshot::Stop) => info!("Stopped snapshotter"),
            }
        }
    }

    pub fn stopper(&self) -> Stopper {
        Stopper {
            tx: self.tx.clone(),
        }
    }

    fn snapshot(&self) -> Result<(), SnapshotError> {
        let snapshot_result =
            Command::new("mithril-proto/mithril-snapshotter-poc/mithril-snapshot.sh")
                .args(["full", &*self.db_directory])
                .spawn()
                .and_then(|mut child| child.wait());
        snapshot_result.map(|_| ()).map_err(|e| SnapshotError {
            reason: e.to_string(),
        })
    }
}

impl Stopper {
    /// Stop
    pub fn stop(&self) {
        info!("Stopping Snapshotter");
        self.tx.send(Snapshot::Stop).unwrap();
    }
}
