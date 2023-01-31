use std::str::FromStr;

use serde::Deserialize;
use thiserror::Error;

/// The era that the software is running or will run
#[derive(Debug, Clone, Copy, PartialEq, Eq, Deserialize)]
pub enum SupportedEra {
    /// Thales era
    Thales,
}

impl SupportedEra {
    /// Retrieve a dummy era (for test only)
    #[cfg(any(test, feature = "test_only"))]
    pub fn dummy() -> Self {
        Self::Thales
    }
}

/// Error related to [SupportedEra] String parsing implementation.
#[derive(Error, Debug)]
#[error("Unable to transform era '{0}' into a currently supported era ('thales').")]
pub struct UnsupportedEraError(String);

impl FromStr for SupportedEra {
    type Err = UnsupportedEraError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let s = s.trim().to_lowercase();

        if &s == "thales" {
            Ok(Self::Thales)
        } else {
            Err(UnsupportedEraError(s))
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    const ERA_NAME: &str = "thales";

    #[test]
    fn from_str() {
        let supported_era =
            SupportedEra::from_str(ERA_NAME).expect("This era name should be supported.");

        assert_eq!(SupportedEra::dummy(), supported_era);
    }

    #[test]
    fn from_bad_str() {
        let era_name = &format!("  {} ", ERA_NAME.to_ascii_uppercase());
        let supported_era =
            SupportedEra::from_str(era_name).expect("This era name should be supported.");

        assert_eq!(SupportedEra::dummy(), supported_era);
    }
}
