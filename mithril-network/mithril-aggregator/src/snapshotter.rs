use log::info;
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

    /// For sending instructions to the snapshotter
    tx: Sender<Snapshot>,

    /// For receiving messages while snapshotter is running
    rx: Receiver<Snapshot>,
}

pub struct Stopper {
    /// For sending instructions to the snapshotter
    tx: Sender<Snapshot>,
}

impl Snapshotter {
    /// Server factory
    pub fn new(interval: u32) -> Self {
        let (tx, rx) = mpsc::channel();
        Self { interval, rx, tx }
    }

    /// Start
    pub fn run(&self) {
        info!("Starting Snapshotter");
        loop {
            match self
                .rx
                .recv_timeout(Duration::from_millis(self.interval.into()))
            {
                Err(_) => info!("Snapshotting"),
                Ok(Snapshot::Stop) => info!("Stopped snapshotter"),
            }
        }
    }

    pub fn stopper(&self) -> Stopper {
        Stopper {
            tx: self.tx.clone(),
        }
    }
}

impl Stopper {
    /// Stop
    pub fn stop(&self) {
        info!("Stopping Snapshotter");
        self.tx.send(Snapshot::Stop).unwrap();
    }
}
