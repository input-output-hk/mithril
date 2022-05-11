use std::{error::Error, fmt::Display};

#[derive(Debug)]
pub enum StoreError {
    AdapterError(String),
}

impl Error for StoreError {}

impl Display for StoreError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            StoreError::AdapterError(msg) => write!(f, "Adapter error: {}", msg),
        }
    }
}
