use std::fmt::{Display, Formatter};
use std::str::FromStr;

use serde::{Deserialize, Deserializer, Serialize};

use mithril_common::StdResult;

/// Name of a CryptoKeyVersion that represents an individual cryptographic key and the associated key material.
///
/// see name property in <https://cloud.google.com/kms/docs/reference/rpc/google.cloud.kms.v1#google.cloud.kms.v1.CryptoKeyVersion>
#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct GcpCryptoKeyVersionResourceName {
    /// The project ID of the Google Cloud project.
    pub project: String,
    /// The location of the key ring.
    pub location: String,
    /// The key ring that contains the key.
    pub key_ring: String,
    /// The name of the key.
    pub key_name: String,
    /// The version of the key.
    pub version: String,
}

const PROJECT_KEY: &str = "projects";
const LOCATION_KEY: &str = "locations";
const KEY_RING_KEY: &str = "keyRings";
const KEY_NAME_KEY: &str = "cryptoKeys";
const VERSION_KEY: &str = "cryptoKeyVersions";

impl Display for GcpCryptoKeyVersionResourceName {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{PROJECT_KEY}/{}/{LOCATION_KEY}/{}/{KEY_RING_KEY}/{}/{KEY_NAME_KEY}/{}/{VERSION_KEY}/{}",
            self.project, self.location, self.key_ring, self.key_name, self.version
        )
    }
}

impl FromStr for GcpCryptoKeyVersionResourceName {
    type Err = anyhow::Error;

    fn from_str(s: &str) -> StdResult<Self> {
        let error = format!(
            "Invalid resource name: '{s}' does not match pattern '{PROJECT_KEY}/../{LOCATION_KEY}/../{KEY_RING_KEY}/../{KEY_NAME_KEY}/../{VERSION_KEY}/..'"
        );
        let parts: Vec<&str> = s.split('/').collect();

        if parts.len() != 10 {
            anyhow::bail!(error);
        }

        if parts[0] != PROJECT_KEY
            || parts[2] != LOCATION_KEY
            || parts[4] != KEY_RING_KEY
            || parts[6] != KEY_NAME_KEY
            || parts[8] != VERSION_KEY
        {
            anyhow::bail!(error);
        }

        if parts.iter().any(|part| part.is_empty()) {
            anyhow::bail!(error);
        }

        Ok(Self {
            project: parts[1].to_string(),
            location: parts[3].to_string(),
            key_ring: parts[5].to_string(),
            key_name: parts[7].to_string(),
            version: parts[9].to_string(),
        })
    }
}

impl<'de: 'a, 'a> Deserialize<'de> for GcpCryptoKeyVersionResourceName {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        use serde::de::Error;
        let str: &'a str = Deserialize::deserialize(deserializer)?;
        Self::from_str(str).map_err(Error::custom)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn convert_crypto_key_resource_name_to_string_use_gcp_format() {
        let gcp_key = GcpCryptoKeyVersionResourceName {
            project: "my_project".to_string(),
            location: "my_location".to_string(),
            key_ring: "my_key_ring".to_string(),
            key_name: "my_key".to_string(),
            version: "1".to_string(),
        };

        assert_eq!(
            gcp_key.to_string(),
            "projects/my_project/locations/my_location/keyRings/my_key_ring/cryptoKeys/my_key/cryptoKeyVersions/1".to_string()
        );
    }

    mod parse_from_str {
        use super::*;

        #[test]
        fn with_correctly_formatted_str_retrieve_all_keys() {
            let resource_name_string =
                format!("{PROJECT_KEY}/my_project/{LOCATION_KEY}/my_location/{KEY_RING_KEY}/my_key_ring/{KEY_NAME_KEY}/my_key/{VERSION_KEY}/1");

            for parsed_resource_name in [
                GcpCryptoKeyVersionResourceName::from_str(&resource_name_string).unwrap(),
                serde_json::from_str(&format!(r#""{resource_name_string}""#)).unwrap(),
            ] {
                assert_eq!(
                    parsed_resource_name,
                    GcpCryptoKeyVersionResourceName {
                        project: "my_project".to_string(),
                        location: "my_location".to_string(),
                        key_ring: "my_key_ring".to_string(),
                        key_name: "my_key".to_string(),
                        version: "1".to_string(),
                    }
                )
            }
        }

        #[test]
        fn with_missing_key_yield_error() {
            GcpCryptoKeyVersionResourceName::from_str(&format!(
                "/proj/{LOCATION_KEY}/loc/{KEY_RING_KEY}/kr/{KEY_NAME_KEY}/key/{VERSION_KEY}/1",
            ))
            .expect_err("Expected an error with missing key");
        }

        #[test]
        fn with_missing_value_yield_error() {
            GcpCryptoKeyVersionResourceName::from_str(
                &format!(
                    "{PROJECT_KEY}//{LOCATION_KEY}/loc/{KEY_RING_KEY}/kr/{KEY_NAME_KEY}/key/{VERSION_KEY}/1",
                )
            ).expect_err("Expected an error with missing value");
        }

        #[test]
        fn with_invalid_key_yield_error() {
            const INVALID_KEY: &str = "invalid_key";
            GcpCryptoKeyVersionResourceName::from_str(
                &format!(
                    "{INVALID_KEY}/proj/{LOCATION_KEY}/loc/{KEY_RING_KEY}/kr/{KEY_NAME_KEY}/key/{VERSION_KEY}/1",
                )
            ).expect_err("Expected an error with invalid key for 'projects");
            GcpCryptoKeyVersionResourceName::from_str(
                &format!(
                    "{PROJECT_KEY}/proj/{INVALID_KEY}/loc/{KEY_RING_KEY}/kr/{KEY_NAME_KEY}/key/{VERSION_KEY}/1",
                )
            )
                .expect_err("Expected an error with invalid key for 'locations");
            GcpCryptoKeyVersionResourceName::from_str(
                &format!(
                    "{PROJECT_KEY}/proj/{LOCATION_KEY}/loc/{INVALID_KEY}/kr/{KEY_NAME_KEY}/key/{VERSION_KEY}/1",
                )
            ).expect_err("Expected an error with invalid key for 'keyRings");
            GcpCryptoKeyVersionResourceName::from_str(
                &format!(
                    "{PROJECT_KEY}/proj/{LOCATION_KEY}/loc/{KEY_RING_KEY}/kr/{INVALID_KEY}/key/{VERSION_KEY}/1",
                )
            ).expect_err("Expected an error with invalid key for 'cryptoKeys");
            GcpCryptoKeyVersionResourceName::from_str(
                &format!(
                    "{PROJECT_KEY}/proj/{LOCATION_KEY}/loc/{KEY_RING_KEY}/kr/{KEY_NAME_KEY}/key/{INVALID_KEY}/1",
                )
            ).expect_err("Expected an error with invalid key for 'cryptoKeyVersions");
        }
    }
}
