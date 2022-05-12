use std::{error::Error, fmt::Display};

#[derive(Debug, Clone)]
pub enum AdapterError {
    InputOutputError(String),
}

impl Error for AdapterError {}

impl Display for AdapterError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            AdapterError::InputOutputError(msg) => {
                write!(f, "communication problem encountered: {}", msg)
            }
        }
    }
}
