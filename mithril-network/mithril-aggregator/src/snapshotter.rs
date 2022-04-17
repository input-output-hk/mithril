use hex;
use log::error;
use log::info;
use sha2::{Digest, Sha256};
use std::ffi::OsStr;
use std::fs::File;
use std::io;
use std::path::Path;
use std::sync::mpsc;
use std::sync::mpsc::{Receiver, Sender};
use std::time::Duration;

use walkdir::DirEntry;
use walkdir::WalkDir;

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
    pub fn new(interval: u32, db_directory: String) -> Self {
        let (tx, rx) = mpsc::channel();
        Self {
            interval,
            db_directory,
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
        let files = list_files(&*self.db_directory);
        let immutables: Vec<&DirEntry> = files
            .iter()
            .filter(|entry| is_immutable(entry.path()))
            .collect();

        info!("#files: {}, #immutables: {}", files.len(), immutables.len());

        let hash = compute_hash(&immutables);

        info!("snapshot hash: {}", hex::encode(hash));

        Ok(())
    }
}

fn compute_hash(entries: &Vec<&DirEntry>) -> [u8; 32] {
    let mut hasher = Sha256::new();

    for &entry in entries {
        let mut file = File::open(entry.path()).unwrap();

        io::copy(&mut file, &mut hasher).unwrap();
    }

    hasher.finalize().into()
}

fn is_immutable(path: &Path) -> bool {
    let immutable = OsStr::new("immutable");
    path.iter().any(|component| component == immutable)
}

fn list_files(dir: &str) -> Vec<DirEntry> {
    let mut files: Vec<DirEntry> = vec![];

    for file in WalkDir::new(dir).into_iter().filter_map(|file| file.ok()) {
        if file.metadata().unwrap().is_file() {
            files.push(file);
        }
    }
    files
}

impl Stopper {
    /// Stop
    pub fn stop(&self) {
        info!("Stopping Snapshotter");
        self.tx.send(Snapshot::Stop).unwrap();
    }
}
