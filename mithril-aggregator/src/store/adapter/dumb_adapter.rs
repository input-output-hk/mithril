use super::{AdapterError, StoreAdapter};
use async_trait::async_trait;

pub struct DumbStoreAdapter<K, R> {
    last_key: Option<K>,
    last_certificate: Option<R>,
}

impl<K, R> DumbStoreAdapter<K, R> {
    pub fn new() -> Self {
        Self {
            last_key: None,
            last_certificate: None,
        }
    }
}

#[async_trait]
impl<K, R> StoreAdapter for DumbStoreAdapter<K, R>
where
    R: Clone + Send + Sync,
    K: PartialEq + Clone + Send + Sync,
{
    type Key = K;
    type Record = R;

    async fn store_record(
        &mut self,
        key: &Self::Key,
        record: &Self::Record,
    ) -> Result<(), AdapterError> {
        let key = key.clone();
        let record = record.clone();

        self.last_key = Some(key);
        self.last_certificate = Some(record);

        Ok(())
    }

    async fn get_record(&self, key: &Self::Key) -> Result<Option<Self::Record>, AdapterError> {
        if self.record_exists(key).await? {
            Ok(self.last_certificate.as_ref().cloned())
        } else {
            Ok(None)
        }
    }

    async fn record_exists(&self, key: &Self::Key) -> Result<bool, AdapterError> {
        Ok(self.last_key.is_some() && self.last_key.as_ref().unwrap() == key)
    }

    async fn get_last_n_records(
        &self,
        how_many: usize,
    ) -> Result<Vec<(Self::Key, Self::Record)>, AdapterError> {
        if how_many > 0 {
            match &self.last_key {
                Some(_key) => Ok(vec![(
                    self.last_key.as_ref().cloned().unwrap(),
                    self.last_certificate.as_ref().cloned().unwrap(),
                )]),
                None => Ok(Vec::new()),
            }
        } else {
            Ok(Vec::new())
        }
    }

    async fn remove(&mut self, key: &Self::Key) -> Result<Option<Self::Record>, AdapterError> {
        if let Some(record) = self.get_record(key).await? {
            self.last_key = None;
            self.last_certificate = None;

            Ok(Some(record))
        } else {
            Ok(None)
        }
    }
}

#[cfg(test)]
mod tests {

    use super::*;

    #[tokio::test]
    async fn test_with_no_record_exists() {
        let adapter: DumbStoreAdapter<u64, String> = DumbStoreAdapter::new();

        assert!(!adapter.record_exists(&1).await.unwrap());
    }

    #[tokio::test]
    async fn test_with_no_record_get() {
        let adapter: DumbStoreAdapter<u64, String> = DumbStoreAdapter::new();

        assert!(adapter.get_record(&1).await.unwrap().is_none());
    }

    #[tokio::test]
    async fn test_write_record() {
        let mut adapter: DumbStoreAdapter<u64, String> = DumbStoreAdapter::new();

        assert!(adapter
            .store_record(&1, &"record".to_string())
            .await
            .is_ok());
        assert_eq!(
            "record".to_owned(),
            adapter.get_record(&1).await.unwrap().unwrap()
        );
    }

    #[tokio::test]
    async fn test_list_with_no_record() {
        let adapter: DumbStoreAdapter<u64, String> = DumbStoreAdapter::new();

        assert_eq!(0, adapter.get_last_n_records(10).await.unwrap().len());
    }

    #[tokio::test]
    async fn test_list_with_records() {
        let mut adapter: DumbStoreAdapter<u64, String> = DumbStoreAdapter::new();
        let _res = adapter
            .store_record(&1, &"record".to_string())
            .await
            .unwrap();
        let list = adapter.get_last_n_records(10).await.unwrap();

        assert_eq!(1, list.len());

        let (key, record) = &list[0];

        assert_eq!(&1, key);
        assert_eq!(&("record".to_owned()), record);
    }

    #[tokio::test]
    async fn test_list_with_last_zero() {
        let mut adapter: DumbStoreAdapter<u64, String> = DumbStoreAdapter::new();
        let _res = adapter
            .store_record(&1, &"record".to_string())
            .await
            .unwrap();
        let list = adapter.get_last_n_records(0).await.unwrap();

        assert_eq!(0, list.len());
    }

    #[tokio::test]
    async fn test_remove_existing_record() {
        let mut adapter: DumbStoreAdapter<u64, String> = DumbStoreAdapter::new();
        let _res = adapter
            .store_record(&1, &"record".to_string())
            .await
            .unwrap();
        let value = adapter.remove(&1).await.unwrap().unwrap();

        assert_eq!("record".to_string(), value);
        assert!(!adapter.record_exists(&1).await.unwrap());
    }

    #[tokio::test]
    async fn test_remove_non_existing_record() {
        let mut adapter: DumbStoreAdapter<u64, String> = DumbStoreAdapter::new();
        let _res = adapter
            .store_record(&1, &"record".to_string())
            .await
            .unwrap();
        let maybe_record = adapter.remove(&0).await.unwrap();

        assert!(maybe_record.is_none());
    }
}
