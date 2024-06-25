use crate::StdError;
use serde::{Deserialize, Serialize};

/// Representation of a Internal Server Error raised by an http server
#[derive(Clone, Debug, PartialEq, Eq, Default, Serialize, Deserialize)]
pub struct InternalServerError {
    /// error message
    pub message: String,
}

impl InternalServerError {
    /// InternalServerError factory
    pub fn new(message: String) -> InternalServerError {
        InternalServerError { message }
    }
}

impl From<String> for InternalServerError {
    fn from(message: String) -> Self {
        InternalServerError::new(message)
    }
}

impl From<&str> for InternalServerError {
    fn from(message: &str) -> Self {
        InternalServerError::new(message.to_string())
    }
}

impl From<StdError> for InternalServerError {
    fn from(error: StdError) -> Self {
        InternalServerError::new(format!("{error:?}"))
    }
}

/// Representation of a Client Error raised by an http server
#[derive(Clone, Debug, PartialEq, Eq, Default, Serialize, Deserialize)]
pub struct ClientError {
    /// error label
    pub label: String,

    /// error message
    pub message: String,
}

impl ClientError {
    /// ClientError factory
    pub fn new<L: Into<String>, M: Into<String>>(label: L, message: M) -> ClientError {
        ClientError {
            label: label.into(),
            message: message.into(),
        }
    }
}
