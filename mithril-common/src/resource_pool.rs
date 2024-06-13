//! Resource pool implementation

use anyhow::Context;
use std::{
    collections::VecDeque,
    ops::{Deref, DerefMut},
    sync::{Condvar, Mutex},
    time::Duration,
};
use thiserror::Error;

use crate::StdResult;

/// [ResourcePool] related errors.
#[derive(Error, Debug)]
pub enum ResourcePoolError {
    /// Internal Mutex is poisoned
    #[error("Poisoned mutex caused error during acquire lock on resource pool")]
    PoisonedLock,

    /// Acquire resource has timed out
    #[error("Acquire resource has timed out")]
    AcquireTimeout,
}

/// Resource pool implementation (FIFO)
pub struct ResourcePool<T: Reset + Send + Sync> {
    /// The size of the pool
    size: usize,

    /// Discriminant for the resource pool to check if a returned resource is stale
    discriminant: Mutex<u64>,

    /// Resources in the pool
    resources: Mutex<VecDeque<T>>,

    /// Condition variable to notify when a resource is available
    not_empty: Condvar,
}

impl<T: Reset + Send + Sync> ResourcePool<T> {
    /// Create a new resource pool
    pub fn new(pool_size: usize, resources: Vec<T>) -> Self {
        Self {
            size: pool_size,
            discriminant: Mutex::new(0),
            resources: Mutex::new(resources.into()),
            not_empty: Condvar::new(),
        }
    }

    /// Acquire a resource from the pool with a timeout
    pub fn acquire_resource(&self, timeout: Duration) -> StdResult<ResourcePoolItem<'_, T>> {
        let mut resources = self
            .resources
            .lock()
            .map_err(|_| ResourcePoolError::PoisonedLock)
            .with_context(|| "Resource pool 'acquire_resource' failed locking Mutex")?;
        while resources.is_empty() {
            let (resources_locked, wait_result) = self
                .not_empty
                .wait_timeout(resources, timeout)
                .map_err(|_| ResourcePoolError::PoisonedLock)
                .with_context(|| "Resource pool 'acquire_resource' failed waiting for resource")?;
            if wait_result.timed_out() {
                return Err(ResourcePoolError::AcquireTimeout)
                    .with_context(|| "Resource pool 'acquire_resource' has timed out");
            }
            resources = resources_locked;
        }

        Ok(ResourcePoolItem::new(self, resources.pop_front().unwrap()))
    }

    /// Give back a resource to the pool
    /// A resource is given back to the pool only if the discriminant matches
    /// and if the pool is not already full
    pub fn give_back_resource(&self, resource: T, discriminant: u64) -> StdResult<()> {
        if self.count()? == self.size {
            // Pool is full
            return Ok(());
        }
        let mut resources = self
            .resources
            .lock()
            .map_err(|_| ResourcePoolError::PoisonedLock)
            .with_context(|| "Resource pool 'give_back_resource' failed locking Mutex")?;
        if self.discriminant()? != discriminant {
            // Stale resource
            return Ok(());
        }
        resources.push_back(resource);
        self.not_empty.notify_one();

        Ok(())
    }

    /// Give back a resource pool item to the pool
    /// If the resource pool item has not been taken yet, the resource will be given back
    /// If the resource pool item has been taken, nothing will happen
    pub fn give_back_resource_pool_item(
        &self,
        resource_pool_item: ResourcePoolItem<'_, T>,
    ) -> StdResult<()> {
        let mut resource_pool_item = resource_pool_item;
        resource_pool_item.take().map(|resource_item| {
            let mut resource_item = resource_item;
            resource_item.reset()?;

            self.give_back_resource(resource_item, self.discriminant()?)
        });

        Ok(())
    }

    /// Clear the pool
    pub fn clear(&self) {
        let mut resources = self.resources.lock().unwrap();
        resources.clear();
    }

    /// Get the discriminant of the resource pool item
    pub fn discriminant(&self) -> StdResult<u64> {
        Ok(*self
            .discriminant
            .lock()
            .map_err(|_| ResourcePoolError::PoisonedLock)
            .with_context(|| "Resource pool 'discriminant' failed locking Mutex")?)
    }

    /// Set the discriminant of the resource pool item
    pub fn set_discriminant(&self, discriminant: u64) -> StdResult<()> {
        let mut discriminant_guard = self
            .discriminant
            .lock()
            .map_err(|_| ResourcePoolError::PoisonedLock)
            .with_context(|| "Resource pool 'set_discriminant' failed locking Mutex")?;
        *discriminant_guard = discriminant;

        Ok(())
    }

    /// Count the resources in the pool
    pub fn count(&self) -> StdResult<usize> {
        Ok(self
            .resources
            .lock()
            .map_err(|_| ResourcePoolError::PoisonedLock)
            .with_context(|| "Resource pool 'count' failed locking Mutex")?
            .len())
    }

    /// Size of the resource pool
    pub fn size(&self) -> usize {
        self.size
    }
}

/// Resource pool item which will return the resource to the pool when dropped
pub struct ResourcePoolItem<'a, T: Reset + Send + Sync> {
    resource_pool: &'a ResourcePool<T>,
    discriminant: u64,
    resource: Option<T>,
}

impl<'a, T: Reset + Send + Sync> ResourcePoolItem<'a, T> {
    /// Create a new resource pool item
    pub fn new(resource_pool: &'a ResourcePool<T>, resource: T) -> Self {
        let discriminant = *resource_pool.discriminant.lock().unwrap();
        Self {
            resource_pool,
            discriminant,
            resource: Some(resource),
        }
    }

    /// Get the discriminant of the resource pool item
    pub fn discriminant(&self) -> u64 {
        self.discriminant
    }

    /// Take the inner resource if exists
    fn take(&mut self) -> Option<T> {
        self.resource.take()
    }
}

impl<T: Reset + Send + Sync> Deref for ResourcePoolItem<'_, T> {
    type Target = T;

    fn deref(&self) -> &T {
        self.resource.as_ref().unwrap()
    }
}

impl<T: Reset + Send + Sync> DerefMut for ResourcePoolItem<'_, T> {
    fn deref_mut(&mut self) -> &mut T {
        self.resource.as_mut().unwrap()
    }
}

impl<T: Reset + Send + Sync> Drop for ResourcePoolItem<'_, T> {
    fn drop(&mut self) {
        self.take().map(|resource| {
            self.resource_pool
                .give_back_resource(resource, self.discriminant)
        });
    }
}

/// Reset trait which is implemented by pooled resource items.
/// As pool resource items are mutable, this will guarantee that the pool stays  consistent
/// and that acquired resource items do not depend from previous computations.
pub trait Reset {
    /// Reset the resource
    fn reset(&mut self) -> StdResult<()> {
        Ok(())
    }
}

cfg_test_tools! {
    impl Reset for String {}
}

#[cfg(test)]
mod tests {
    use std::time::Duration;

    use super::*;

    #[test]
    fn test_resource_pool_acquire_returns_resource_when_available() {
        let pool_size = 10;
        let resources_expected: Vec<String> = (0..pool_size).map(|i| i.to_string()).collect();
        let pool = ResourcePool::<String>::new(pool_size, resources_expected.clone());

        let mut resources_items = vec![];
        for _ in 0..pool_size {
            let resource_item = pool.acquire_resource(Duration::from_millis(10)).unwrap();
            resources_items.push(resource_item);
        }
        let resources_result = resources_items
            .iter_mut()
            .map(|resource_item| resource_item.take().unwrap())
            .collect::<Vec<_>>();

        assert_eq!(resources_expected, resources_result);
        assert_eq!(pool.count().unwrap(), 0);
    }

    #[tokio::test]
    async fn test_resource_pool_acquire_locks_until_timeout_when_no_resource_available() {
        let pool_size = 10;
        let resources_expected: Vec<String> = (0..pool_size).map(|i| i.to_string()).collect();
        let pool = ResourcePool::<String>::new(pool_size, resources_expected.clone());

        let mut resources_items = vec![];
        for _ in 0..pool_size {
            let resource_item = pool.acquire_resource(Duration::from_millis(10)).unwrap();
            // The resource is stored in a list to avoid it being dropped when exiting this block scope
            resources_items.push(resource_item);
        }

        assert!(pool.acquire_resource(Duration::from_millis(1000)).is_err());
    }

    #[tokio::test]
    async fn test_resource_pool_clears_successfully() {
        let pool_size = 10;
        let resources_expected: Vec<String> = (0..pool_size).map(|i| i.to_string()).collect();
        let pool = ResourcePool::<String>::new(pool_size, resources_expected.clone());
        assert_eq!(pool.count().unwrap(), pool_size);

        pool.clear();

        assert_eq!(pool.count().unwrap(), 0);
    }

    #[tokio::test]
    async fn test_resource_pool_gives_back_fresh_resource() {
        let pool_size = 10;
        let resources_expected: Vec<String> = (0..pool_size).map(|i| i.to_string()).collect();
        let pool = ResourcePool::<String>::new(pool_size, resources_expected.clone());
        assert_eq!(pool.count().unwrap(), pool_size);

        let mut resource_item = pool.acquire_resource(Duration::from_millis(10)).unwrap();
        assert_eq!(pool.count().unwrap(), pool_size - 1);
        pool.give_back_resource(resource_item.take().unwrap(), pool.discriminant().unwrap())
            .unwrap();

        assert_eq!(pool.count().unwrap(), pool_size);
    }

    #[tokio::test]
    async fn test_resource_pool_gives_back_resource_automatically() {
        let pool_size = 10;
        let resources_expected: Vec<String> = (0..pool_size).map(|i| i.to_string()).collect();
        let pool = ResourcePool::<String>::new(pool_size, resources_expected.clone());
        assert_eq!(pool.count().unwrap(), pool_size);

        {
            // Resource will be returned when resource item is dropped (will occur when exiting this block scope)
            let _resource_item = pool.acquire_resource(Duration::from_millis(10)).unwrap();
            assert_eq!(pool.count().unwrap(), pool_size - 1);
        }

        assert_eq!(pool.count().unwrap(), pool_size);
    }

    #[tokio::test]
    async fn test_resource_pool_does_not_give_back_resource_when_pool_is_full() {
        let pool_size = 10;
        let resources_expected: Vec<String> = (0..pool_size).map(|i| i.to_string()).collect();
        let pool = ResourcePool::<String>::new(pool_size, resources_expected.clone());
        assert_eq!(pool.count().unwrap(), pool_size);

        pool.give_back_resource("resource".to_string(), pool.discriminant().unwrap())
            .unwrap();

        assert_eq!(pool.count().unwrap(), pool_size);
    }

    #[tokio::test]
    async fn test_resource_pool_does_not_give_back_stale_resource() {
        let pool_size = 10;
        let resources_expected: Vec<String> = (0..pool_size).map(|i| i.to_string()).collect();
        let pool = ResourcePool::<String>::new(pool_size, resources_expected.clone());
        assert_eq!(pool.count().unwrap(), pool_size);

        let mut resource_item = pool.acquire_resource(Duration::from_millis(10)).unwrap();
        assert_eq!(pool.count().unwrap(), pool_size - 1);
        let discriminant_stale = pool.discriminant().unwrap();
        pool.set_discriminant(pool.discriminant().unwrap() + 1)
            .unwrap();
        pool.give_back_resource(resource_item.take().unwrap(), discriminant_stale)
            .unwrap();

        assert_eq!(pool.count().unwrap(), pool_size - 1);
    }

    #[tokio::test]
    async fn test_resource_pool_gives_back_fresh_resource_pool_item_if_not_taken_yet() {
        let pool_size = 10;
        let resources_expected: Vec<String> = (0..pool_size).map(|i| i.to_string()).collect();
        let pool = ResourcePool::<String>::new(pool_size, resources_expected.clone());
        assert_eq!(pool.count().unwrap(), pool_size);

        let resource_item = pool.acquire_resource(Duration::from_millis(10)).unwrap();
        assert_eq!(pool.count().unwrap(), pool_size - 1);
        pool.give_back_resource_pool_item(resource_item).unwrap();

        assert_eq!(pool.count().unwrap(), pool_size);
    }

    #[tokio::test]
    async fn test_resource_pool_does_not_give_back_fresh_resource_pool_item_if_already_taken() {
        let pool_size = 10;
        let resources_expected: Vec<String> = (0..pool_size).map(|i| i.to_string()).collect();
        let pool = ResourcePool::<String>::new(pool_size, resources_expected.clone());
        assert_eq!(pool.count().unwrap(), pool_size);

        {
            // Resource taken will be dropped when exiting this block scope
            let mut resource_item = pool.acquire_resource(Duration::from_millis(10)).unwrap();
            assert_eq!(pool.count().unwrap(), pool_size - 1);
            let _resource = resource_item.take().unwrap(); // This can only happen in this module as 'take' is private
            pool.give_back_resource_pool_item(resource_item).unwrap();
        }

        assert_eq!(pool.count().unwrap(), pool_size - 1);
    }
}
