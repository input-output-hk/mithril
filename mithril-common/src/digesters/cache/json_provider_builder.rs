use crate::{
    digesters::cache::{ImmutableFileDigestCacheProvider, JsonImmutableFileDigestCacheProvider},
    StdResult,
};
use anyhow::Context;
use slog::{info, Logger};
use std::path::Path;
#[cfg(feature = "fs")]
use tokio::fs;

/// A [JsonImmutableFileDigestCacheProvider] builder.
pub struct JsonImmutableFileDigestCacheProviderBuilder<'a> {
    cache_dir: &'a Path,
    filename: &'a str,
    ensure_dir_exist: bool,
    reset_digests_cache: bool,
    logger: Logger,
}

impl<'a> JsonImmutableFileDigestCacheProviderBuilder<'a> {
    /// [JsonImmutableFileDigestCacheProviderBuilder] factory.
    pub fn new(cache_dir: &'a Path, filename: &'a str) -> Self {
        Self {
            cache_dir,
            filename,
            ensure_dir_exist: false,
            reset_digests_cache: false,
            logger: Logger::root(slog::Discard, slog::o!()),
        }
    }

    /// If set will create the cache directory if it doesn't already exist.
    pub fn ensure_dir_exist(&mut self) -> &mut Self {
        self.ensure_dir_exist = true;
        self
    }

    /// Set if existing cached values in the provider must be reset.
    pub fn should_reset_digests_cache(&mut self, should_reset: bool) -> &mut Self {
        self.reset_digests_cache = should_reset;
        self
    }

    /// Set the [Logger] to use.
    pub fn with_logger(&mut self, logger: Logger) -> &mut Self {
        self.logger = logger.new(slog::o!("src" => "JsonImmutableFileDigestCacheProviderBuilder"));
        self
    }

    /// Build a [JsonImmutableFileDigestCacheProvider] based on the parameters previously set.
    pub async fn build(&self) -> StdResult<JsonImmutableFileDigestCacheProvider> {
        let cache_file = self.cache_dir.join(self.filename);
        let cache_provider = JsonImmutableFileDigestCacheProvider::new(&cache_file);

        if self.ensure_dir_exist {
            fs::create_dir_all(&self.cache_dir).await.with_context(|| {
                format!(
                    "Failure when creating cache directory `{}`",
                    self.cache_dir.display(),
                )
            })?;
        }

        if self.reset_digests_cache {
            cache_provider.reset().await.with_context(|| {
                format!(
                    "Failure when resetting digests cache file `{}`",
                    cache_file.display(),
                )
            })?;
        }

        info!(
            self.logger,
            "Storing/Getting immutables digests cache from: {}",
            cache_file.display()
        );

        Ok(cache_provider)
    }
}

#[cfg(test)]
mod tests {
    use crate::digesters::cache::JsonImmutableFileDigestCacheProviderBuilder;
    use crate::test_utils::TempDir;
    use std::path::PathBuf;

    fn get_test_dir(subdir_name: &str) -> PathBuf {
        TempDir::new("json_provider_builder", subdir_name).build_path()
    }

    #[tokio::test]
    async fn create_dir_if_ensure_dir_exist_is_set() {
        let dir = get_test_dir("create_dir_if_ensure_dir_exist_is_set");

        JsonImmutableFileDigestCacheProviderBuilder::new(&dir, "test.json")
            .ensure_dir_exist()
            .build()
            .await
            .expect("Build should not fail");

        assert!(dir.exists());
    }

    #[tokio::test]
    async fn dont_create_dir_if_ensure_dir_exist_is_not_set() {
        let dir = get_test_dir("dont_create_dir_if_ensure_dir_exist_is_not_set");

        JsonImmutableFileDigestCacheProviderBuilder::new(&dir, "test.json")
            .build()
            .await
            .expect("Build should not fail");

        assert!(!dir.exists());
    }
}
