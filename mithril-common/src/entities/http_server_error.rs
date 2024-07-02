use crate::StdError;
use serde::{Deserialize, Serialize};

/// Representation of a Server Error raised by a http server
#[derive(Clone, Debug, PartialEq, Eq, Default, Serialize, Deserialize)]
pub struct ServerError {
    /// error message
    pub message: String,
}

impl ServerError {
    /// InternalServerError factory
    pub fn new(message: String) -> ServerError {
        ServerError { message }
    }
}

impl From<String> for ServerError {
    fn from(message: String) -> Self {
        ServerError::new(message)
    }
}

impl From<&str> for ServerError {
    fn from(message: &str) -> Self {
        ServerError::new(message.to_string())
    }
}

impl From<StdError> for ServerError {
    fn from(error: StdError) -> Self {
        ServerError::new(format!("{error:?}"))
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
