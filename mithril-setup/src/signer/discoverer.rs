//! Signer discoverer implementation

use anyhow::{Context, anyhow};

use crate::orchestrator::{
    interfaces::Discoverer,
    store::{Field, Store},
};

/// A discoverer for the signer
pub struct SignerDiscoverer {}

impl Discoverer for SignerDiscoverer {
    fn run(store: Store) -> anyhow::Result<Store> {
        let mut discovered_fields: Vec<Field> = Vec::new();

        if let Ok(field) = SignerDiscoverer::discover_network(&store) {
            discovered_fields.push(field);
        }

        Ok(Store::new(discovered_fields))
    }
}

impl SignerDiscoverer {
    fn discover_network(store: &Store) -> anyhow::Result<Field> {
        let mut field = store
            .get_field("network")
            .cloned()
            .ok_or_else(|| anyhow!("Field 'network' not found in the store"))?;

        let network =
            std::env::var("NETWORK").context("Environment variable 'NETWORK' not found")?;

        field.set_discovered_value(network);

        Ok(field)
    }
}

#[cfg(test)]
mod test {
    use crate::orchestrator::store::{Field, FieldType};

    use super::*;

    #[test]
    fn test_signer_discoverer_must_discover_network_throught_env_variable() {
        let store = Store::new(vec![Field::init(
            "network".to_string(),
            "The Cardano network to target".to_string(),
            FieldType::String,
            Some("preview".to_string()),
        )]);

        //set env variable
        unsafe { std::env::set_var("NETWORK", "mainnet") };

        let new_store = SignerDiscoverer::run(store);
        let actual_value = new_store
            .unwrap()
            .get_field("network")
            .unwrap()
            .value
            .clone()
            .unwrap();

        assert_eq!("mainnet".to_string(), actual_value);
    }
}
