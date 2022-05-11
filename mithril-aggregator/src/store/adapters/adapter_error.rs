use std::{error::Error, fmt::Display};

#[derive(Debug, Clone)]
pub enum AdapterError {
    MediumIsFull(String),
    PermissionDenied(String),
    InputOutputError(String),
}

impl Error for AdapterError {}

impl Display for AdapterError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            AdapterError::MediumIsFull(msg) => write!(
                f,
                "it seems the adapter cannot write more data (out of space): {}",
                msg
            ),
            AdapterError::PermissionDenied(msg) => {
                write!(f, "operation was denied, check permissions: {}", msg)
            }
            AdapterError::InputOutputError(msg) => {
                write!(f, "communication problem encountered: {}", msg)
            }
        }
    }
}
