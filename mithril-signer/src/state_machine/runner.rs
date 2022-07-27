use crate::Config;

use super::signer_services::SignerServices;

pub trait Runner {}

pub struct SignerRunner {
    config: Config,
    services: SignerServices,
}

impl SignerRunner {
    pub fn new(config: Config, services: SignerServices) -> Self {
        Self { services, config }
    }
}

impl Runner for SignerRunner {}
