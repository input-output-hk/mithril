use anyhow::{anyhow, Context};
use async_trait::async_trait;
use chrono::{DateTime, TimeDelta, Utc};
use std::ops::Add;
use web_sys::{window, Storage};

use mithril_client::certificate_client::CertificateVerifierCache;
use mithril_client::MithrilResult;

pub type CertificateHash = str;
pub type PreviousCertificateHash = str;

/// Browser local-storage based cache for the certificate verifier.
///
/// Note : as this cache is based on the browser local storage, it can only be used in a browser
/// (it is not compatible with nodejs or other non-browser environment).
pub struct LocalStorageCertificateVerifierCache {
    cache_key_prefix: String,
    expiration_delay: TimeDelta,
}

#[derive(Debug, PartialEq, Eq, Clone, serde::Serialize, serde::Deserialize)]
struct CachedCertificate {
    previous_hash: String,
    expire_at: DateTime<Utc>,
}

impl CachedCertificate {
    fn new<TPreviousHash: Into<String>>(
        previous_hash: TPreviousHash,
        expire_at: DateTime<Utc>,
    ) -> Self {
        CachedCertificate {
            previous_hash: previous_hash.into(),
            expire_at,
        }
    }
}

impl LocalStorageCertificateVerifierCache {
    /// `LocalStorageCertificateVerifierCache` factory
    pub fn new(cache_key_prefix_seed: &str, expiration_delay: TimeDelta) -> Self {
        const CACHE_KEY_BASE_PREFIX: &'static str = "certificate_cache";

        LocalStorageCertificateVerifierCache {
            cache_key_prefix: format!("{CACHE_KEY_BASE_PREFIX}_{cache_key_prefix_seed}_"),
            expiration_delay,
        }
    }

    fn push(
        &self,
        certificate_hash: &CertificateHash,
        previous_certificate_hash: &PreviousCertificateHash,
        expire_at: DateTime<Utc>,
    ) -> MithrilResult<()> {
        if let Some(storage) = open_local_storage()? {
            let key = self.cache_key(certificate_hash);
            storage
                .set_item(
                    &key,
                    &serde_json::to_string(&CachedCertificate::new(
                        previous_certificate_hash,
                        expire_at,
                    ))
                    .map_err(|err| anyhow!("Error serializing cache: {err:?}"))?,
                )
                .map_err(|err| anyhow!("Error storing key `{key}` in local storage: {err:?}"))?;
        }
        Ok(())
    }

    fn parse_cached_certificate(value: String) -> MithrilResult<CachedCertificate> {
        serde_json::from_str(&value)
            .map_err(|err| anyhow!("Error deserializing cached certificate: {err:?}"))
    }

    fn cache_key(&self, certificate_hash: &CertificateHash) -> String {
        format!("{}{}", self.cache_key_prefix, certificate_hash)
    }
}

fn open_local_storage() -> MithrilResult<Option<Storage>> {
    let window = window()
        .with_context(|| "No window object")?
        .local_storage()
        .map_err(|err| anyhow!("Error accessing local storage: {err:?}"))?;
    Ok(window)
}

#[cfg_attr(target_family = "wasm", async_trait(?Send))]
#[cfg_attr(not(target_family = "wasm"), async_trait)]
impl CertificateVerifierCache for LocalStorageCertificateVerifierCache {
    async fn store_validated_certificate(
        &self,
        certificate_hash: &CertificateHash,
        previous_certificate_hash: &PreviousCertificateHash,
    ) -> MithrilResult<()> {
        self.push(
            certificate_hash,
            previous_certificate_hash,
            Utc::now().add(self.expiration_delay),
        )?;
        Ok(())
    }

    async fn get_previous_hash(
        &self,
        certificate_hash: &CertificateHash,
    ) -> MithrilResult<Option<String>> {
        match open_local_storage()? {
            None => return Ok(None),
            Some(storage) => {
                let key = self.cache_key(certificate_hash);
                match storage.get_item(&key).map_err(|err| {
                    anyhow!("Error accessing key `{key}` from local storage: {err:?}")
                })? {
                    Some(value) => {
                        let cached = Self::parse_cached_certificate(value)?;
                        if Utc::now() >= cached.expire_at {
                            Ok(None)
                        } else {
                            Ok(Some(cached.previous_hash))
                        }
                    }
                    None => Ok(None),
                }
            }
        }
    }

    async fn reset(&self) -> MithrilResult<()> {
        if let Some(storage) = open_local_storage()? {
            let len = storage
                .length()
                .map_err(|err| anyhow!("Error accessing local storage length: {err:?}"))?;
            let mut key_to_remove = vec![];

            for i in 0..len {
                match storage.key(i).map_err(|err| {
                    anyhow!("Error accessing key index `{i}` from local storage: {err:?}")
                })? {
                    Some(key) if key.starts_with(&self.cache_key_prefix) => key_to_remove.push(key),
                    _ => continue,
                }
            }

            for key in key_to_remove {
                storage.remove_item(&key).map_err(|err| {
                    anyhow!("Error removing key `{key}` from local storage: {err:?}")
                })?;
            }
        }

        Ok(())
    }
}

#[cfg(test)]
pub(crate) mod test_tools {
    use std::collections::HashMap;

    use super::*;

    /// `Test only` Return the raw content of the local storage
    pub(crate) fn local_storage_content() -> HashMap<String, String> {
        let storage = open_local_storage().unwrap().unwrap();
        let len = storage.length().unwrap();
        let mut content = HashMap::new();

        for i in 0..len {
            let key = storage.key(i).unwrap().unwrap();
            let value = storage.get_item(&key).unwrap().unwrap();
            content.insert(key, value);
        }

        content
    }

    impl LocalStorageCertificateVerifierCache {
        /// `Test only` Return the number of items in the cache
        pub(crate) fn len(&self) -> usize {
            local_storage_content()
                .into_iter()
                .filter(|(k, _v)| k.starts_with(&self.cache_key_prefix))
                .count()
        }

        /// `Test only` Populate the cache with the given hash and previous hash
        pub(crate) fn with_items<'a, T>(self, key_values: T) -> Self
        where
            T: IntoIterator<Item = (&'a CertificateHash, &'a PreviousCertificateHash)>,
        {
            let expire_at = Utc::now() + self.expiration_delay;
            for (k, v) in key_values {
                self.push(k, v, expire_at).unwrap();
            }
            self
        }

        /// `Test only` Return the content of the cache (without the expiration date)
        pub(crate) fn content(&self) -> HashMap<String, String> {
            local_storage_content()
                .into_iter()
                .filter(|(k, _v)| k.starts_with(&self.cache_key_prefix))
                .map(|(k, v)| {
                    (
                        k.trim_start_matches(&self.cache_key_prefix).to_string(),
                        Self::parse_cached_certificate(v).unwrap().previous_hash,
                    )
                })
                .collect()
        }

        /// `Test only` Overwrite the expiration date of an entry the given certificate hash.
        ///
        /// panic if the key is not found
        pub(crate) fn overwrite_expiration_date(
            &self,
            certificate_hash: &CertificateHash,
            expire_at: DateTime<Utc>,
        ) {
            let storage = open_local_storage().unwrap().unwrap();
            let key = self.cache_key(certificate_hash);
            let existing_value = Self::parse_cached_certificate(
                storage.get_item(&key).unwrap().expect("Key not found"),
            )
            .unwrap();
            storage
                .set_item(
                    &key,
                    &serde_json::to_string(&CachedCertificate::new(
                        &existing_value.previous_hash,
                        expire_at,
                    ))
                    .unwrap(),
                )
                .unwrap();
        }

        /// `Test only` Get the cached value for the given certificate hash
        pub(super) fn get_cached_value(
            &self,
            certificate_hash: &CertificateHash,
        ) -> Option<CachedCertificate> {
            let storage = open_local_storage().unwrap().unwrap();
            storage
                .get_item(&self.cache_key(certificate_hash))
                .unwrap()
                .map(Self::parse_cached_certificate)
                .transpose()
                .unwrap()
        }
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;
    use wasm_bindgen_test::*;

    use super::{test_tools::*, *};

    // Note: as those tests are using local storage, they MUST be run in a browser as node doesn't
    // have support for local storage.
    wasm_bindgen_test_configure!(run_in_browser);

    #[wasm_bindgen_test]
    async fn from_str_iterator() {
        let cache =
            LocalStorageCertificateVerifierCache::new("from_str_iterator", TimeDelta::hours(1))
                .with_items([("first", "one"), ("second", "two")]);

        assert_eq!(
            HashMap::from_iter([
                ("first".to_string(), "one".to_string()),
                ("second".to_string(), "two".to_string())
            ]),
            cache.content()
        );
    }

    mod store_validated_certificate {
        use super::*;

        #[wasm_bindgen_test]
        async fn store_in_empty_cache_add_new_item_that_expire_after_parametrized_delay() {
            let expiration_delay = TimeDelta::hours(1);
            let now = Utc::now();
            let cache = LocalStorageCertificateVerifierCache::new(
                "store_in_empty_cache_add_new_item_that_expire_after_parametrized_delay",
                expiration_delay,
            );
            cache
                .store_validated_certificate("hash", "parent")
                .await
                .unwrap();

            let cached = cache
                .get_cached_value("hash")
                .expect("Cache should have been populated");

            assert_eq!(1, cache.len());
            assert_eq!("parent", cached.previous_hash);
            assert!(cached.expire_at - now >= expiration_delay);
        }

        #[wasm_bindgen_test]
        async fn store_new_hash_push_new_key_at_end_and_dont_alter_existing_values() {
            let cache = LocalStorageCertificateVerifierCache::new(
                "store_new_hash_push_new_key_at_end_and_dont_alter_existing_values",
                TimeDelta::hours(1),
            )
            .with_items([
                ("existing_hash", "existing_parent"),
                ("another_hash", "another_parent"),
            ]);
            cache
                .store_validated_certificate("new_hash", "new_parent")
                .await
                .unwrap();

            assert_eq!(
                HashMap::from_iter([
                    ("existing_hash".to_string(), "existing_parent".to_string()),
                    ("another_hash".to_string(), "another_parent".to_string()),
                    ("new_hash".to_string(), "new_parent".to_string()),
                ]),
                cache.content()
            );
        }

        #[wasm_bindgen_test]
        async fn storing_same_hash_update_parent_hash_and_expiration_time() {
            let expiration_delay = TimeDelta::days(2);
            let now = Utc::now();
            let cache = LocalStorageCertificateVerifierCache::new(
                "storing_same_hash_update_parent_hash_and_expiration_time",
                expiration_delay,
            )
            .with_items([("hash", "first_parent"), ("another_hash", "another_parent")]);

            cache
                .store_validated_certificate("hash", "updated_parent")
                .await
                .unwrap();

            let updated_value = cache
                .get_cached_value("hash")
                .expect("Cache should have been populated");

            assert_eq!(2, cache.len());
            assert_eq!(
                Some("another_parent".to_string()),
                cache.get_previous_hash("another_hash").await.unwrap(),
                "Existing but not updated value should not have been altered, content: {:#?}, now: {:?}",
                local_storage_content(), now,
            );
            assert_eq!("updated_parent", updated_value.previous_hash);
            assert!(updated_value.expire_at - now >= expiration_delay);
        }
    }

    mod get_previous_hash {
        use super::*;

        #[wasm_bindgen_test]
        async fn get_previous_hash_when_key_exists() {
            let cache = LocalStorageCertificateVerifierCache::new(
                "get_previous_hash_when_key_exists",
                TimeDelta::hours(1),
            )
            .with_items([("hash", "parent"), ("another_hash", "another_parent")]);

            assert_eq!(
                Some("parent".to_string()),
                cache.get_previous_hash("hash").await.unwrap()
            );
        }

        #[wasm_bindgen_test]
        async fn get_previous_hash_return_none_if_not_found() {
            let cache = LocalStorageCertificateVerifierCache::new(
                "get_previous_hash_return_none_if_not_found",
                TimeDelta::hours(1),
            )
            .with_items([("hash", "parent"), ("another_hash", "another_parent")]);

            assert_eq!(None, cache.get_previous_hash("not_found").await.unwrap());
        }

        #[wasm_bindgen_test]
        async fn get_expired_previous_hash_return_none() {
            let cache = LocalStorageCertificateVerifierCache::new(
                "get_expired_previous_hash_return_none",
                TimeDelta::hours(1),
            )
            .with_items([("hash", "parent")]);
            cache.overwrite_expiration_date("hash", Utc::now() - TimeDelta::days(5));

            assert_eq!(None, cache.get_previous_hash("hash").await.unwrap());
        }
    }

    mod reset {
        use super::*;

        #[wasm_bindgen_test]
        async fn reset_empty_cache_dont_raise_error() {
            let cache = LocalStorageCertificateVerifierCache::new(
                "reset_empty_cache_dont_raise_error",
                TimeDelta::hours(1),
            );

            cache.reset().await.unwrap();

            assert_eq!(HashMap::new(), cache.content());
        }

        #[wasm_bindgen_test]
        async fn reset_not_empty_cache() {
            let cache = LocalStorageCertificateVerifierCache::new(
                "reset_not_empty_cache",
                TimeDelta::hours(1),
            )
            .with_items([("hash", "parent"), ("another_hash", "another_parent")]);
            let storage = open_local_storage().unwrap().unwrap();
            storage
                .set_item("key_from_another_component", "another_value")
                .unwrap();

            cache.reset().await.unwrap();

            assert_eq!(HashMap::new(), cache.content());
            assert_eq!(
                Some(&"another_value".to_string()),
                local_storage_content().get("key_from_another_component")
            );
        }
    }
}
