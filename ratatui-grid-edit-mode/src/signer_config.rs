use std::{fmt, path::PathBuf};

const DEFAULT_DB_DIRECTORY: &str = "./mithril-signer/db";
const DEFAULT_CARDANO_CLI_PATH: &str = "cardano-cli";
const DEFAULT_RUN_INTERVAL: u64 = 5000;
const DEFAULT_STORE_RETENTION_LIMIT: u64 = 10;

/// Cardano network the signer runs on.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Network {
    Mainnet,
    Preprod,
    Preview,
}

impl Network {
    /// Every network accepted by the signer.
    pub const ALL: [Network; 3] = [Network::Mainnet, Network::Preprod, Network::Preview];
}

impl fmt::Display for Network {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Network::Mainnet => write!(f, "mainnet"),
            Network::Preprod => write!(f, "preprod"),
            Network::Preview => write!(f, "preview"),
        }
    }
}

/// Configuration of a Mithril signer
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SignerConfig {
    pub network: Network,
    pub kes_secret_key_path: PathBuf,
    pub operational_certificate_path: PathBuf,
    pub db_directory: PathBuf,
    pub cardano_cli_path: PathBuf,
    pub run_interval: u64,
    pub store_retention_limit: u64,
}

/// [`SignerConfig`] being filled with grid edit mode.
#[derive(Debug, Default, Clone)]
pub struct SignerConfigDraft {
    pub network: Option<Network>,
    pub kes_secret_key_path: Option<PathBuf>,
    pub operational_certificate_path: Option<PathBuf>,
    pub db_directory: Option<PathBuf>,
    pub cardano_cli_path: Option<PathBuf>,
    pub run_interval: Option<u64>,
    pub store_retention_limit: Option<u64>,
}

impl SignerConfigDraft {
    /// Create a draft with example defaults for the signer runtime settings.
    pub fn with_defaults() -> Self {
        Self {
            db_directory: Some(DEFAULT_DB_DIRECTORY.into()),
            cardano_cli_path: Some(DEFAULT_CARDANO_CLI_PATH.into()),
            run_interval: Some(DEFAULT_RUN_INTERVAL),
            store_retention_limit: Some(DEFAULT_STORE_RETENTION_LIMIT),
            ..Self::default()
        }
    }
}

impl TryFrom<&SignerConfigDraft> for SignerConfig {
    type Error = &'static str;

    fn try_from(draft: &SignerConfigDraft) -> Result<Self, Self::Error> {
        Ok(Self {
            network: draft.network.ok_or("network is required")?,
            kes_secret_key_path: draft
                .kes_secret_key_path
                .clone()
                .ok_or("kes_secret_key_path is required")?,
            operational_certificate_path: draft
                .operational_certificate_path
                .clone()
                .ok_or("operational_certificate_path is required")?,
            db_directory: draft.db_directory.clone().ok_or("db_directory is required")?,
            cardano_cli_path: draft
                .cardano_cli_path
                .clone()
                .ok_or("cardano_cli_path is required")?,
            run_interval: draft.run_interval.ok_or("run_interval is required")?,
            store_retention_limit: draft
                .store_retention_limit
                .ok_or("store_retention_limit is required")?,
        })
    }
}
