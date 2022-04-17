use crate::entities::Snapshot;
use chrono::prelude::*;
use flate2::write::GzEncoder;
use flate2::Compression;
use hex;
use log::error;
use log::info;
use serde_json;
use sha2::{Digest, Sha256};
use std::ffi::OsStr;
use std::fs;
use std::fs::File;
use std::io;
use std::path::Path;
use std::sync::mpsc;
use std::sync::mpsc::{Receiver, Sender};
use std::time::Duration;

use walkdir::DirEntry;
use walkdir::WalkDir;

/// Message sent to Snapshotter
pub enum Messages {
    Stop,
}

/// Snapshotter
pub struct Snapshotter {
    /// Interval between each snapshot, in seconds
    interval: u32,

    /// DB directory to snapshot
    db_directory: String,

    /// For sending instructions to the snapshotter
    tx: Sender<Messages>,

    /// For receiving messages while snapshotter is running
    rx: Receiver<Messages>,
}

pub struct Stopper {
    /// For sending instructions to the snapshotter
    tx: Sender<Messages>,
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
                Ok(Messages::Stop) => info!("Stopped snapshotter"),
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
        let digest = hex::encode(hash);
        info!("snapshot hash: {}", digest);

        let tar_gz = File::create("testnet.tar.gz").unwrap();
        let enc = GzEncoder::new(tar_gz, Compression::default());
        let mut tar = tar::Builder::new(enc);

        info!("compressing {} into testnet.tar.gz", &self.db_directory);

        tar.append_dir_all(".", &self.db_directory).unwrap();
        tar.finish().unwrap();

        // TODO: compute size accurately
        let size = fs::metadata("testnet.tar.gz").unwrap().len();

        let timestamp: DateTime<Utc> = Utc::now();
        let created_at = format!("{:?}", timestamp);

        let snapshot = Snapshot {
            digest,
            certificate_hash: "".to_string(),
            size,
            created_at,
            locations: vec![],
        };

        info!("snapshot: {}", serde_json::to_string(&snapshot).unwrap());
        serde_json::to_writer(&File::create("snapshots.json").unwrap(), &snapshot).unwrap();

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
        self.tx.send(Messages::Stop).unwrap();
    }
}
