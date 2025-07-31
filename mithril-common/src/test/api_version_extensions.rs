//! A set of extension traits to add test utilities to this crate `APIVersionProvider`

use semver::Version;
use std::collections::HashMap;

use crate::api_version::{APIVersionProvider, OpenAPIFileName};

/// Extension trait adding test utilities to [APIVersionProvider]
pub trait ApiVersionProviderTestExtension {
    /// `TEST ONLY` - Replace the open api versions
    fn update_open_api_versions(&mut self, open_api_versions: HashMap<OpenAPIFileName, Version>);

    /// `TEST ONLY` - Set up an ` APIVersionProvider ` with the given version for the `openapi.yaml` file
    fn new_with_default_version(version: Version) -> APIVersionProvider;

    /// `TEST ONLY` - Set up an ` APIVersionProvider ` that fails to compute api versions
    fn new_failing() -> APIVersionProvider;
}
