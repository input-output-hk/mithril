//! # Signed Entity Type Lock
//!
//! This module provides a non-blocking lock mechanism for signed entity types to prevent multiple
//! modification on a same entity type at the same time.

use std::collections::BTreeSet;

use tokio::sync::RwLock;

use crate::entities::SignedEntityTypeDiscriminants;

/// Non-blocking lock mechanism for signed entity types to prevent multiple
/// modification on a same entity type at the same time.
pub struct SignedEntityTypeLock {
    locked_entities: RwLock<BTreeSet<SignedEntityTypeDiscriminants>>,
}

impl SignedEntityTypeLock {
    /// Create a new instance of `SignedEntityLock` without any signed entity locked.
    pub fn new() -> Self {
        Self {
            locked_entities: RwLock::new(BTreeSet::new()),
        }
    }

    /// Check if a signed entity is locked.
    pub async fn is_locked<T: Into<SignedEntityTypeDiscriminants>>(&self, entity_type: T) -> bool {
        let locked_entities = self.locked_entities.read().await;
        locked_entities.contains(&entity_type.into())
    }

    /// Lock a signed entity.
    ///
    /// If the entity is already locked, this function does nothing.
    pub async fn lock<T: Into<SignedEntityTypeDiscriminants>>(&self, entity_type: T) {
        let mut locked_entities = self.locked_entities.write().await;
        locked_entities.insert(entity_type.into());
    }

    /// Release a locked signed entity.
    ///
    /// If the entity is not locked, this function does nothing.
    pub async fn release<T: Into<SignedEntityTypeDiscriminants>>(&self, entity_type: T) {
        let mut locked_entities = self.locked_entities.write().await;
        locked_entities.remove(&entity_type.into());
    }

    /// List only the unlocked signed entities in the given list.
    pub async fn filter_unlocked_entries<T: Into<SignedEntityTypeDiscriminants> + Clone>(
        &self,
        entries: Vec<T>,
    ) -> Vec<T> {
        let locked_entities = self.locked_entities.read().await;
        entries
            .into_iter()
            .filter(|entry| !locked_entities.contains(&entry.clone().into()))
            .collect()
    }
}

impl Default for SignedEntityTypeLock {
    /// Create a new instance of `SignedEntityLock` without any signed entity locked.
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn by_default_entity_is_not_locked() {
        let signed_entity_type_lock = SignedEntityTypeLock::new();
        for entity_type in SignedEntityTypeDiscriminants::all() {
            assert!(!signed_entity_type_lock.is_locked(entity_type).await);
        }
    }

    #[tokio::test]
    async fn lock_a_signed_entity() {
        for entity_type in SignedEntityTypeDiscriminants::all() {
            let signed_entity_type_lock = SignedEntityTypeLock::new();
            signed_entity_type_lock.lock(entity_type).await;
            assert!(signed_entity_type_lock.is_locked(entity_type).await);
        }
    }

    #[tokio::test]
    async fn locking_an_entity_more_than_once_in_a_row_dont_crash() {
        let entity = SignedEntityTypeDiscriminants::MithrilStakeDistribution;
        let signed_entity_type_lock = SignedEntityTypeLock::new();

        signed_entity_type_lock.lock(entity).await;
        signed_entity_type_lock.lock(entity).await;
        signed_entity_type_lock.lock(entity).await;

        assert!(signed_entity_type_lock.is_locked(entity).await);
    }

    #[tokio::test]
    async fn locking_a_signed_entity_does_not_lock_other_entity() {
        let signed_entity_type_lock = SignedEntityTypeLock::new();
        signed_entity_type_lock
            .lock(SignedEntityTypeDiscriminants::CardanoImmutableFilesFull)
            .await;
        assert!(
            signed_entity_type_lock
                .is_locked(SignedEntityTypeDiscriminants::CardanoImmutableFilesFull)
                .await
        );
        assert!(
            !signed_entity_type_lock
                .is_locked(SignedEntityTypeDiscriminants::MithrilStakeDistribution)
                .await
        );
    }

    #[tokio::test]
    async fn releasing_an_locked_entity() {
        let entity = SignedEntityTypeDiscriminants::MithrilStakeDistribution;
        let signed_entity_type_lock = SignedEntityTypeLock::new();

        signed_entity_type_lock.lock(entity).await;
        signed_entity_type_lock.release(entity).await;

        assert!(!signed_entity_type_lock.is_locked(entity).await);
    }

    #[tokio::test]
    async fn releasing_an_already_released_entity() {
        let entity = SignedEntityTypeDiscriminants::MithrilStakeDistribution;
        let signed_entity_type_lock = SignedEntityTypeLock::new();

        assert!(!signed_entity_type_lock.is_locked(entity).await);

        signed_entity_type_lock.release(entity).await;

        assert!(!signed_entity_type_lock.is_locked(entity).await);
    }

    #[tokio::test]
    async fn releasing_a_signed_entity_does_not_release_other_entity() {
        let released_entity = SignedEntityTypeDiscriminants::MithrilStakeDistribution;
        let signed_entity_type_lock = SignedEntityTypeLock::new();

        for entity in SignedEntityTypeDiscriminants::all() {
            signed_entity_type_lock.lock(entity).await;
        }

        signed_entity_type_lock.release(released_entity).await;

        assert!(!signed_entity_type_lock.is_locked(released_entity).await);
        assert!(
            signed_entity_type_lock
                .is_locked(SignedEntityTypeDiscriminants::CardanoImmutableFilesFull)
                .await
        );
        assert!(
            signed_entity_type_lock
                .is_locked(SignedEntityTypeDiscriminants::CardanoTransactions)
                .await
        );
    }

    #[tokio::test]
    async fn filters_out_locked_entities() {
        let signed_entity_type_lock = SignedEntityTypeLock::new();
        let signed_entities: Vec<_> = vec![
            SignedEntityTypeDiscriminants::MithrilStakeDistribution,
            SignedEntityTypeDiscriminants::CardanoTransactions,
        ];

        assert_eq!(
            signed_entity_type_lock
                .filter_unlocked_entries(signed_entities.clone())
                .await,
            signed_entities.clone()
        );

        signed_entity_type_lock
            .lock(SignedEntityTypeDiscriminants::MithrilStakeDistribution)
            .await;

        assert_eq!(
            signed_entity_type_lock
                .filter_unlocked_entries(signed_entities.clone())
                .await,
            vec![SignedEntityTypeDiscriminants::CardanoTransactions]
        );
    }
}
