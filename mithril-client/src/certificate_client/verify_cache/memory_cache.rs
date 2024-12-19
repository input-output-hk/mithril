use async_trait::async_trait;
use chrono::{DateTime, TimeDelta, Utc};
use std::collections::HashMap;
use std::ops::Add;
use tokio::sync::RwLock;

use crate::certificate_client::CertificateVerifierCache;
use crate::MithrilResult;

pub type CertificateHash = str;
pub type PreviousCertificateHash = str;

/// A in-memory cache for the certificate verifier.
pub struct MemoryCertificateVerifierCache {
    expiration_delay: TimeDelta,
    cache: RwLock<HashMap<String, CachedCertificate>>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
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

impl MemoryCertificateVerifierCache {
    /// `MemoryCertificateVerifierCache` factory
    pub fn new(expiration_delay: TimeDelta) -> Self {
        MemoryCertificateVerifierCache {
            expiration_delay,
            cache: RwLock::new(HashMap::new()),
        }
    }

    /// Get the number of elements in the cache
    pub async fn len(&self) -> usize {
        self.cache.read().await.len()
    }

    /// Return true if the cache is empty
    pub async fn is_empty(&self) -> bool {
        self.cache.read().await.is_empty()
    }
}

#[cfg_attr(target_family = "wasm", async_trait(?Send))]
#[cfg_attr(not(target_family = "wasm"), async_trait)]
impl CertificateVerifierCache for MemoryCertificateVerifierCache {
    async fn store_validated_certificate(
        &self,
        certificate_hash: &CertificateHash,
        previous_certificate_hash: &PreviousCertificateHash,
    ) -> MithrilResult<()> {
        // todo: should we raise an error if an empty string is given for previous_certificate_hash ? (or any other kind of validation)
        let mut cache = self.cache.write().await;
        cache.insert(
            certificate_hash.to_string(),
            CachedCertificate::new(
                previous_certificate_hash,
                Utc::now().add(self.expiration_delay),
            ),
        );
        Ok(())
    }

    async fn get_previous_hash(
        &self,
        certificate_hash: &CertificateHash,
    ) -> MithrilResult<Option<String>> {
        let cache = self.cache.read().await;
        Ok(cache
            .get(certificate_hash)
            .filter(|cached| cached.expire_at >= Utc::now())
            .map(|cached| cached.previous_hash.clone()))
    }

    async fn reset(&self) -> MithrilResult<()> {
        let mut cache = self.cache.write().await;
        cache.clear();
        Ok(())
    }
}

#[cfg(test)]
pub(crate) mod test_tools {
    use mithril_common::entities::Certificate;

    use super::*;

    impl MemoryCertificateVerifierCache {
        /// `Test only` Populate the cache with the given hash and previous hash
        pub(crate) fn with_items<'a, T>(mut self, key_values: T) -> Self
        where
            T: IntoIterator<Item = (&'a CertificateHash, &'a PreviousCertificateHash)>,
        {
            let expire_at = Utc::now() + self.expiration_delay;
            self.cache = RwLock::new(
                key_values
                    .into_iter()
                    .map(|(k, v)| (k.to_string(), CachedCertificate::new(v, expire_at)))
                    .collect(),
            );
            self
        }

        /// `Test only` Populate the cache with the given hash and previous hash from given certificates
        pub(crate) fn with_items_from_chain<'a, T>(self, chain: T) -> Self
        where
            T: IntoIterator<Item = &'a Certificate>,
        {
            self.with_items(
                chain
                    .into_iter()
                    .map(|cert| (cert.hash.as_str(), cert.previous_hash.as_str())),
            )
        }

        /// `Test only` Return the content of the cache (without the expiration date)
        pub(crate) async fn content(&self) -> HashMap<String, String> {
            self.cache
                .read()
                .await
                .iter()
                .map(|(hash, cached)| (hash.clone(), cached.previous_hash.clone()))
                .collect()
        }

        /// `Test only` Overwrite the expiration date of an entry the given certificate hash.
        ///
        /// panic if the key is not found
        pub(crate) async fn overwrite_expiration_date(
            &self,
            certificate_hash: &CertificateHash,
            expire_at: DateTime<Utc>,
        ) {
            let mut cache = self.cache.write().await;
            cache
                .get_mut(certificate_hash)
                .expect("Key not found")
                .expire_at = expire_at;
        }

        /// `Test only` Get the cached value for the given certificate hash
        pub(super) async fn get_cached_value(
            &self,
            certificate_hash: &CertificateHash,
        ) -> Option<CachedCertificate> {
            self.cache.read().await.get(certificate_hash).cloned()
        }
    }
}

#[cfg(test)]
mod tests {
    use mithril_common::entities::Certificate;
    use mithril_common::test_utils::fake_data;

    use super::*;

    #[tokio::test]
    async fn from_str_iterator() {
        let cache = MemoryCertificateVerifierCache::new(TimeDelta::hours(1))
            .with_items([("first", "one"), ("second", "two")]);

        assert_eq!(
            HashMap::from_iter([
                ("first".to_string(), "one".to_string()),
                ("second".to_string(), "two".to_string())
            ]),
            cache.content().await
        );
    }

    #[tokio::test]
    async fn from_certificate_iterator() {
        let chain = vec![
            Certificate {
                previous_hash: "first_parent".to_string(),
                ..fake_data::certificate("first")
            },
            Certificate {
                previous_hash: "second_parent".to_string(),
                ..fake_data::certificate("second")
            },
        ];
        let cache =
            MemoryCertificateVerifierCache::new(TimeDelta::hours(1)).with_items_from_chain(&chain);

        assert_eq!(
            HashMap::from_iter([
                ("first".to_string(), "first_parent".to_string()),
                ("second".to_string(), "second_parent".to_string())
            ]),
            cache.content().await
        );
    }

    mod store_validated_certificate {
        use super::*;

        #[tokio::test]
        async fn store_in_empty_cache_add_new_item_that_expire_after_parametrized_delay() {
            let expiration_delay = TimeDelta::hours(1);
            let start_time = Utc::now();
            let cache = MemoryCertificateVerifierCache::new(expiration_delay);
            cache
                .store_validated_certificate("hash", "parent")
                .await
                .unwrap();

            let cached = cache
                .get_cached_value("hash")
                .await
                .expect("Cache should have been populated");

            assert_eq!(1, cache.len().await);
            assert_eq!("parent", cached.previous_hash);
            assert!(cached.expire_at - start_time >= expiration_delay);
        }

        #[tokio::test]
        async fn store_new_hash_push_new_key_at_end_and_dont_alter_existing_values() {
            let cache = MemoryCertificateVerifierCache::new(TimeDelta::hours(1)).with_items([
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
                cache.content().await
            );
        }

        #[tokio::test]
        async fn storing_same_hash_update_parent_hash_and_expiration_time() {
            let expiration_delay = TimeDelta::days(2);
            let start_time = Utc::now();
            let cache = MemoryCertificateVerifierCache::new(expiration_delay)
                .with_items([("hash", "first_parent"), ("another_hash", "another_parent")]);

            let initial_value = cache.get_cached_value("hash").await.unwrap();

            cache
                .store_validated_certificate("hash", "updated_parent")
                .await
                .unwrap();

            let updated_value = cache.get_cached_value("hash").await.unwrap();

            assert_eq!(2, cache.len().await);
            assert_eq!(
                Some("another_parent".to_string()),
                cache.get_previous_hash("another_hash").await.unwrap(),
                "Existing but not updated value should not have been altered"
            );
            assert_ne!(initial_value, updated_value);
            assert_eq!("updated_parent", updated_value.previous_hash);
            assert!(updated_value.expire_at - start_time >= expiration_delay);
        }
    }

    mod get_previous_hash {
        use super::*;

        #[tokio::test]
        async fn get_previous_hash_when_key_exists() {
            let cache = MemoryCertificateVerifierCache::new(TimeDelta::hours(1))
                .with_items([("hash", "parent"), ("another_hash", "another_parent")]);

            assert_eq!(
                Some("parent".to_string()),
                cache.get_previous_hash("hash").await.unwrap()
            );
        }

        #[tokio::test]
        async fn get_previous_hash_return_none_if_not_found() {
            let cache = MemoryCertificateVerifierCache::new(TimeDelta::hours(1))
                .with_items([("hash", "parent"), ("another_hash", "another_parent")]);

            assert_eq!(None, cache.get_previous_hash("not_found").await.unwrap());
        }

        #[tokio::test]
        async fn get_expired_previous_hash_return_none() {
            let cache = MemoryCertificateVerifierCache::new(TimeDelta::hours(1))
                .with_items([("hash", "parent")]);
            cache
                .overwrite_expiration_date("hash", Utc::now() - TimeDelta::days(5))
                .await;

            assert_eq!(None, cache.get_previous_hash("hash").await.unwrap());
        }
    }

    mod reset {
        use super::*;

        #[tokio::test]
        async fn reset_empty_cache_dont_raise_error() {
            let cache = MemoryCertificateVerifierCache::new(TimeDelta::hours(1));

            cache.reset().await.unwrap();

            assert_eq!(HashMap::new(), cache.content().await);
        }

        #[tokio::test]
        async fn reset_not_empty_cache() {
            let cache = MemoryCertificateVerifierCache::new(TimeDelta::hours(1))
                .with_items([("hash", "parent"), ("another_hash", "another_parent")]);

            cache.reset().await.unwrap();

            assert_eq!(HashMap::new(), cache.content().await);
        }
    }
}
