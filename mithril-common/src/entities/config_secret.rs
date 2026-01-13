use std::fmt;
use std::hash::Hash;
use std::str::FromStr;

use serde::{Deserialize, Deserializer, Serialize, Serializer};

/// A wrapper type for sensitive configuration values that should never be printed.
///
/// **IMPORTANT**: This type is only designed for configuration secrets and should not be used for
/// other purposes.
/// It does not provide protection against memory inspection, it does not zeroize the memory on drop,
/// and may be sensible to time-based attacks.
///
/// This type implements `Debug` and `Display` to always show `[REDACTED]` instead
/// of the actual value, preventing accidental exposure in logs or console output.
///
/// **Note on Serialization**: When serialized, this type always outputs `"[REDACTED]"`
/// regardless of the inner value.
/// Deserialization works normally, parsing the actual value.
/// This asymmetry is intentional to prevent accidental secret exposure in serialized output.
pub struct ConfigSecret<T>(T);

impl<T> ConfigSecret<T> {
    /// Creates a new secret configuration value.
    pub fn new(value: T) -> Self {
        Self(value)
    }

    /// Exposes the inner secret value.
    ///
    /// Use this method carefully and only when you actually need the secret value.
    pub fn expose_secret(&self) -> &T {
        &self.0
    }

    /// Consumes the wrapper and returns the inner secret value.
    pub fn into_inner(self) -> T {
        self.0
    }
}

impl<T> fmt::Debug for ConfigSecret<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str("[REDACTED]")
    }
}

impl<T> fmt::Display for ConfigSecret<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str("[REDACTED]")
    }
}

impl<T: Clone> Clone for ConfigSecret<T> {
    fn clone(&self) -> Self {
        Self(self.0.clone())
    }
}

impl<T: Default> Default for ConfigSecret<T> {
    fn default() -> Self {
        Self(T::default())
    }
}

impl<T> From<T> for ConfigSecret<T> {
    fn from(value: T) -> Self {
        Self::new(value)
    }
}

impl<T: FromStr> FromStr for ConfigSecret<T> {
    type Err = T::Err;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        T::from_str(s).map(Self::new)
    }
}

impl<T: PartialEq> PartialEq for ConfigSecret<T> {
    fn eq(&self, other: &Self) -> bool {
        self.0.eq(&other.0)
    }
}

impl<T: Eq> Eq for ConfigSecret<T> {}

impl<T: Hash> Hash for ConfigSecret<T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.0.hash(state);
    }
}

impl<T: Serialize> Serialize for ConfigSecret<T> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        serializer.serialize_str("[REDACTED]")
    }
}

impl<'de, T: Deserialize<'de>> Deserialize<'de> for ConfigSecret<T> {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        T::deserialize(deserializer).map(ConfigSecret::new)
    }
}

#[cfg(test)]
mod tests {
    use crate::test::TestLogger;

    use super::*;

    #[test]
    fn test_debug_redacted_in_display_and_debug() {
        let secret = ConfigSecret::new("secret");
        assert_eq!(format!("{secret}"), "[REDACTED]");
        assert_eq!(format!("{secret:#}"), "[REDACTED]");
        assert_eq!(format!("{secret:?}"), "[REDACTED]");
        assert_eq!(format!("{secret:#?}"), "[REDACTED]");
    }

    #[test]
    fn test_redacted_in_slog() {
        let (logger, inspector) = TestLogger::memory();
        let secret = ConfigSecret::new("0123456789ABCD");

        slog::info!(
            logger,
            "log: {secret}, log debug: {secret:?}, log alternate: {secret:#}, log alternate debug: {secret:#?}"
        );
        slog::info!(logger, "log in keys";
            "secret" => %secret, "debug" => ?secret, "alternate" => #%secret, "alternate_debug" => #?secret
        );

        assert!(!inspector.contains_log(secret.expose_secret()));
    }

    #[test]
    fn test_from_str() {
        let secret: ConfigSecret<String> = "my-secret".parse().unwrap();
        assert_eq!(secret.expose_secret(), "my-secret");
    }

    #[test]
    fn test_serde_serialization() {
        let secret = ConfigSecret::new("secret");
        let serialized = serde_json::to_string(&secret).unwrap();
        assert_eq!(serialized, r#""[REDACTED]""#);
    }

    #[test]
    fn test_serde_deserialization() {
        let secret: ConfigSecret<String> = serde_json::from_str(r#""secret""#).unwrap();
        assert_eq!(secret, ConfigSecret::new("secret".to_string()));

        #[derive(Deserialize, PartialEq, Debug)]
        struct Mixed {
            secret: ConfigSecret<String>,
            non_secret: String,
        }
        let mixed_struct: Mixed =
            serde_json::from_str(r#"{ "secret": "secret", "non_secret": "non-secret" }"#).unwrap();
        assert_eq!(
            mixed_struct,
            Mixed {
                secret: ConfigSecret::new("secret".to_string()),
                non_secret: "non-secret".to_string(),
            }
        );
    }
}
