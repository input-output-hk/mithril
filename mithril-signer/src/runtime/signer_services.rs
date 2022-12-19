use async_trait::async_trait;
use std::{
    error::{Error as StdError, Error},
    fs,
    sync::Arc,
};

use mithril_common::digesters::cache::{
    ImmutableFileDigestCacheProvider, JsonImmutableFileDigestCacheProviderBuilder,
};
use mithril_common::{
    chain_observer::{CardanoCliChainObserver, CardanoCliRunner, ChainObserver},
    crypto_helper::{OpCert, ProtocolPartyId, SerDeShelleyFileFormat},
    digesters::{CardanoImmutableDigester, ImmutableDigester, ImmutableFileSystemObserver},
    store::{adapter::SQLiteAdapter, StakeStore},
    BeaconProvider, BeaconProviderImpl,
};

use crate::{
    certificate_handler::CertificateHandler, single_signer::SingleSigner,
    CertificateHandlerHTTPClient, Config, MithrilSingleSigner, ProtocolInitializerStore,
    ProtocolInitializerStorer,
};

type StakeStoreService = Arc<StakeStore>;
type CertificateHandlerService = Arc<dyn CertificateHandler>;
type ChainObserverService = Arc<dyn ChainObserver>;
type DigesterService = Arc<dyn ImmutableDigester>;
type SingleSignerService = Arc<dyn SingleSigner>;
type BeaconProviderService = Arc<dyn BeaconProvider>;
type ProtocolInitializerStoreService = Arc<dyn ProtocolInitializerStorer>;

/// The ServiceBuilder is intended to manage Services instance creation.
/// The goal of this is to put all this code out of the way of business code.
#[async_trait]
pub trait ServiceBuilder {
    /// Create a SignerService instance.
    async fn build(&self) -> Result<SignerServices, Box<dyn StdError>>;
}

/// Create a SignerService instance for Production environment.
pub struct ProductionServiceBuilder<'a> {
    config: &'a Config,
}

impl<'a> ProductionServiceBuilder<'a> {
    /// Create a new production service builder.
    pub fn new(config: &'a Config) -> Self {
        Self { config }
    }

    /// Compute protocol party id
    fn compute_protocol_party_id(&self) -> Result<ProtocolPartyId, Box<dyn StdError>> {
        match &self.config.operational_certificate_path {
            Some(operational_certificate_path) => {
                let opcert: OpCert = OpCert::from_file(operational_certificate_path)
                    .map_err(|e| format!("Could not decode operational certificate: {:?}", e))?;
                Ok(opcert.compute_protocol_party_id().map_err(|e| {
                    format!(
                        "Could not compute party_id from operational certificate: {:?}",
                        e
                    )
                })?)
            }
            _ => Ok(self
                .config
                .party_id
                .to_owned()
                .ok_or("A party_id should at least be provided")?),
        }
    }

    async fn build_digester_cache_provider(
        &self,
    ) -> Result<Option<Arc<dyn ImmutableFileDigestCacheProvider>>, Box<dyn Error>> {
        if self.config.disable_digests_cache {
            return Ok(None);
        }

        let cache_provider = JsonImmutableFileDigestCacheProviderBuilder::new(
            &self.config.data_stores_directory,
            &format!("immutables_digests_{}.json", self.config.network),
        )
        .should_reset_digests_cache(self.config.reset_digests_cache)
        .build()
        .await?;

        Ok(Some(Arc::new(cache_provider)))
    }
}

#[async_trait]
impl<'a> ServiceBuilder for ProductionServiceBuilder<'a> {
    /// Build a Services for the Production environment.
    async fn build(&self) -> Result<SignerServices, Box<dyn StdError>> {
        if !self.config.data_stores_directory.exists() {
            fs::create_dir_all(self.config.data_stores_directory.clone())
                .map_err(|e| format!("Could not create data stores directory: {:?}", e))?;
        }

        let sqlite_db_path = Some(self.config.get_sqlite_file());
        let protocol_initializer_store = Arc::new(ProtocolInitializerStore::new(
            Box::new(SQLiteAdapter::new(
                "protocol_initializer",
                sqlite_db_path.clone(),
            )?),
            self.config.store_retention_limit,
        ));
        let single_signer = Arc::new(MithrilSingleSigner::new(self.compute_protocol_party_id()?));
        let certificate_handler = Arc::new(CertificateHandlerHTTPClient::new(
            self.config.aggregator_endpoint.clone(),
        ));
        let digester = Arc::new(CardanoImmutableDigester::new(
            self.config.db_directory.clone(),
            self.build_digester_cache_provider().await?,
            slog_scope::logger(),
        ));
        let stake_store = Arc::new(StakeStore::new(
            Box::new(SQLiteAdapter::new("stake", sqlite_db_path)?),
            self.config.store_retention_limit,
        ));
        let chain_observer = Arc::new(CardanoCliChainObserver::new(Box::new(
            CardanoCliRunner::new(
                self.config.cardano_cli_path.clone(),
                self.config.cardano_node_socket_path.clone(),
                self.config.get_network()?,
            ),
        )));
        let beacon_provider = Arc::new(BeaconProviderImpl::new(
            chain_observer.clone(),
            Arc::new(ImmutableFileSystemObserver::new(&self.config.db_directory)),
            self.config.get_network()?.to_owned(),
        ));

        let services = SignerServices {
            beacon_provider,
            certificate_handler,
            chain_observer,
            digester,
            single_signer,
            stake_store,
            protocol_initializer_store,
        };

        Ok(services)
    }
}

/// This structure groups all the services required by the state machine.
pub struct SignerServices {
    /// Beacon provider service
    pub beacon_provider: BeaconProviderService,

    /// Stake store service
    pub stake_store: StakeStoreService,

    /// Certificate handler service
    pub certificate_handler: CertificateHandlerService,

    /// Chain Observer service
    pub chain_observer: ChainObserverService,

    /// Digester service
    pub digester: DigesterService,

    /// SingleSigner service
    pub single_signer: SingleSignerService,

    /// ProtocolInitializer store
    pub protocol_initializer_store: ProtocolInitializerStoreService,
}

#[cfg(test)]
mod tests {
    use super::*;

    use std::path::PathBuf;

    fn get_test_dir() -> PathBuf {
        let test_dir = std::env::temp_dir().join("mithril_test");

        if test_dir.exists() {
            fs::remove_dir_all(&test_dir)
                .unwrap_or_else(|_| panic!("Could not remove dir {:?}", test_dir));
        }
        fs::create_dir_all(&test_dir)
            .unwrap_or_else(|_| panic!("Could not create dir {:?}", test_dir));

        test_dir
    }

    #[tokio::test]
    async fn test_auto_create_stores_directory() {
        let stores_dir = get_test_dir().join("stores");
        let config = Config {
            cardano_cli_path: PathBuf::new(),
            cardano_node_socket_path: PathBuf::new(),
            network_magic: None,
            network: "preview".to_string(),
            aggregator_endpoint: "".to_string(),
            party_id: Some("party-123456".to_string()),
            run_interval: 1000,
            db_directory: PathBuf::new(),
            data_stores_directory: stores_dir.clone(),
            store_retention_limit: None,
            kes_secret_key_path: None,
            operational_certificate_path: None,
            disable_digests_cache: false,
            reset_digests_cache: false,
        };

        assert!(!stores_dir.exists());
        let service_builder = ProductionServiceBuilder::new(&config);
        service_builder
            .build()
            .await
            .expect("service builder build should not fail");
        assert!(stores_dir.exists());
    }
}
