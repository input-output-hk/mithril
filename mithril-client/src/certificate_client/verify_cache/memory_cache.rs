use async_trait::async_trait;
use std::collections::HashMap;
use tokio::sync::RwLock;

use mithril_common::entities::Certificate;

use crate::certificate_client::CertificateVerifierCache;
use crate::MithrilResult;

pub type CertificateHash = str;
pub type PreviousCertificateHash = str;

/// A in-memory cache for the certificate verifier.
#[derive(Default)]
pub struct MemoryCertificateVerifierCache {
    /// Hashmap of certificate hash and their parent hash.
    cache: RwLock<HashMap<String, String>>,
}

impl MemoryCertificateVerifierCache {
    #[cfg(test)]
    async fn content(&self) -> HashMap<String, String> {
        self.cache.read().await.clone()
    }
}

impl From<&[(&CertificateHash, &PreviousCertificateHash)]> for MemoryCertificateVerifierCache {
    fn from(slice: &[(&CertificateHash, &PreviousCertificateHash)]) -> Self {
        MemoryCertificateVerifierCache {
            cache: RwLock::new(
                slice
                    .iter()
                    .map(|(k, v)| (k.to_string(), v.to_string()))
                    .collect(),
            ),
        }
    }
}

impl<'a> FromIterator<(&'a CertificateHash, &'a PreviousCertificateHash)>
    for MemoryCertificateVerifierCache
{
    fn from_iter<T: IntoIterator<Item = (&'a CertificateHash, &'a PreviousCertificateHash)>>(
        iter: T,
    ) -> Self {
        MemoryCertificateVerifierCache {
            cache: RwLock::new(
                iter.into_iter()
                    .map(|(k, v)| (k.to_string(), v.to_string()))
                    .collect(),
            ),
        }
    }
}

impl<'a> FromIterator<&'a Certificate> for MemoryCertificateVerifierCache {
    fn from_iter<T: IntoIterator<Item = &'a Certificate>>(iter: T) -> Self {
        MemoryCertificateVerifierCache {
            cache: RwLock::new(
                iter.into_iter()
                    .map(|cert| (cert.hash.clone(), cert.previous_hash.clone()))
                    .collect(),
            ),
        }
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
            previous_certificate_hash.to_string(),
        );
        Ok(())
    }

    async fn get_previous_hash(
        &self,
        certificate_hash: &CertificateHash,
    ) -> MithrilResult<Option<String>> {
        let cache = self.cache.read().await;
        Ok(cache.get(certificate_hash).cloned())
    }

    async fn reset(&self) -> MithrilResult<()> {
        let mut cache = self.cache.write().await;
        cache.clear();
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use mithril_common::test_utils::fake_data;

    use super::*;

    #[tokio::test]
    async fn from_str_iterator() {
        let cache =
            MemoryCertificateVerifierCache::from_iter([("first", "one"), ("second", "two")]);

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
        let cache = MemoryCertificateVerifierCache::from_iter(&chain);

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
        async fn store_in_empty_cache() {
            let cache = MemoryCertificateVerifierCache::default();
            cache
                .store_validated_certificate("hash", "parent")
                .await
                .unwrap();

            assert_eq!(
                HashMap::from_iter([("hash".to_string(), "parent".to_string()),]),
                cache.content().await
            );
        }

        #[tokio::test]
        async fn store_new_hash_push_new_key_at_end_and_dont_alter_existing_values() {
            let cache = MemoryCertificateVerifierCache::from_iter([
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
        async fn storing_same_hash_update_parent_hash() {
            let cache = MemoryCertificateVerifierCache::from_iter([
                ("hash", "first_parent"),
                ("another_hash", "another_parent"),
            ]);
            cache
                .store_validated_certificate("hash", "updated_parent")
                .await
                .unwrap();

            assert_eq!(
                HashMap::from_iter([
                    ("hash".to_string(), "updated_parent".to_string()),
                    ("another_hash".to_string(), "another_parent".to_string())
                ]),
                cache.content().await
            );
        }
    }

    mod get_previous_hash {
        use super::*;

        #[tokio::test]
        async fn get_previous_hash_when_key_exists() {
            let cache = MemoryCertificateVerifierCache::from_iter([
                ("hash", "parent"),
                ("another_hash", "another_parent"),
            ]);

            assert_eq!(
                Some("parent".to_string()),
                cache.get_previous_hash("hash").await.unwrap()
            );
        }

        #[tokio::test]
        async fn get_previous_hash_return_none_if_not_found() {
            let cache = MemoryCertificateVerifierCache::from_iter([
                ("hash", "parent"),
                ("another_hash", "another_parent"),
            ]);

            assert_eq!(None, cache.get_previous_hash("not_found").await.unwrap());
        }
    }

    mod reset {
        use super::*;

        #[tokio::test]
        async fn reset_empty_cache_dont_raise_error() {
            let cache = MemoryCertificateVerifierCache::default();

            cache.reset().await.unwrap();

            assert_eq!(HashMap::new(), cache.content().await);
        }

        #[tokio::test]
        async fn reset_not_empty_cache() {
            let cache = MemoryCertificateVerifierCache::from_iter([
                ("hash", "parent"),
                ("another_hash", "another_parent"),
            ]);

            cache.reset().await.unwrap();

            assert_eq!(HashMap::new(), cache.content().await);
        }
    }
}
