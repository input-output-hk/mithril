use std::{collections::HashMap, rc::Rc};
use thiserror::Error;

use mithril_common::{
    chain_observer::ChainObserver, digesters::Digester, store::stake_store::StakeStore,
};

use crate::{
    certificate_handler::{self, CertificateHandler},
    single_signer::SingleSigner,
};

pub enum Service {
    CertificateHandler(Rc<Box<dyn CertificateHandler>>),
    ChainObserver(Rc<Box<dyn ChainObserver>>),
    Digester(Rc<Box<dyn Digester>>),
    StakeStore(Rc<StakeStore>),
    SingleSigner(Rc<Box<dyn SingleSigner>>),
}

#[derive(Error, Debug)]
pub enum ServiceError {
    #[error("Service not found: {0}")]
    ServiceNotRegistered(String),
}
pub struct SignerServices {
    services: HashMap<String, Service>,
}

impl SignerServices {
    pub fn new() -> Self {
        Self {
            services: HashMap::new(),
        }
    }

    /// register or override a service
    pub fn register(&mut self, service_name: &str, service: Service) -> &mut Self {
        let _ = self.services.insert(service_name.to_string(), service);

        self
    }

    pub fn is_registered(&self, service_name: &str) -> bool {
        self.services.contains_key(service_name)
    }

    pub fn get(&self, service_name: &str) -> Result<&Service, ServiceError> {
        self.services
            .get(service_name)
            .ok_or_else(|| ServiceError::ServiceNotRegistered(service_name.to_string()))
    }
}

#[cfg(test)]
mod tests {
    use mithril_common::store::adapter::DumbStoreAdapter;

    use super::*;

    #[test]
    fn getting_unexisting_service_raise_error() {
        let services = SignerServices::new();
        assert!(services.get("whatever").is_err());
    }

    #[test]
    fn is_a_service_registered() {
        let mut services = SignerServices::new();
        assert!(!services.is_registered("service_name"));
        services.register(
            "service_name",
            Service::StakeStore(Rc::new(StakeStore::new(Box::new(DumbStoreAdapter::new())))),
        );
        assert!(services.is_registered("service_name"));
    }

    #[test]
    fn get_existing_service() {
        let mut services = SignerServices::new();
        services.register(
            "service_name",
            Service::StakeStore(Rc::new(StakeStore::new(Box::new(DumbStoreAdapter::new())))),
        );
        let _service = services
            .get("service_name")
            .expect("getting registered service should not raise an error");
    }
}
