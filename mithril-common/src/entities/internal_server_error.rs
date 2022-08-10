use serde::{Deserialize, Serialize};

/// Representation of a Internal Server Error raised by an http server
#[derive(Clone, Debug, PartialEq, Default, Serialize, Deserialize)]
pub struct InternalServerError {
    /// error message
    #[serde(rename = "message")]
    pub message: String,
}

impl InternalServerError {
    /// InternalServerError factory
    pub fn new(message: String) -> InternalServerError {
        InternalServerError { message }
    }
}
