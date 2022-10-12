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
    pub fn new(label: String, message: String) -> ClientError {
        ClientError { label, message }
    }
}
