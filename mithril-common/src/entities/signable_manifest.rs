use std::collections::BTreeMap;

use serde::{Deserialize, Serialize};

use crate::crypto_helper::ManifestSignature;

/// Stores a map of files and their hashes, with an optional signature to verify the integrity of the
/// signed data.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct SignableManifest<TKey: Ord, TValue> {
    /// The data stored in the manifest
    pub data: BTreeMap<TKey, TValue>,
    /// The signature of the manifest
    #[serde(skip_serializing_if = "Option::is_none")]
    pub signature: Option<ManifestSignature>,
}
