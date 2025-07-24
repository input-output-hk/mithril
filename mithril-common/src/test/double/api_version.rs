use crate::api_version::ApiVersionDiscriminantSource;

/// A dummy implementation of the `ApiVersionDiscriminantSource` trait for testing purposes.
pub struct DummyApiVersionDiscriminantSource {
    discriminant: String,
}

impl DummyApiVersionDiscriminantSource {
    /// Create a new instance of `DummyApiVersionDiscriminantSource` with the given discriminant.
    pub fn new<T: Into<String>>(discrimant: T) -> Self {
        Self {
            discriminant: discrimant.into(),
        }
    }
}

impl Default for DummyApiVersionDiscriminantSource {
    fn default() -> Self {
        Self {
            discriminant: "dummy".to_string(),
        }
    }
}

impl ApiVersionDiscriminantSource for DummyApiVersionDiscriminantSource {
    fn get_discriminant(&self) -> String {
        self.discriminant.clone()
    }
}
