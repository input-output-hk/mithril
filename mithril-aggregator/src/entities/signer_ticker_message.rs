use mithril_common::entities::PartyId;
use serde::{Deserialize, Serialize};

/// Message structure of a known signer
#[derive(Clone, Debug, PartialEq, Eq, Default, Serialize, Deserialize)]
pub struct SignerTickerMessage {
    /// The signer party id
    pub party_id: PartyId,

    /// The signer pool ticker
    #[serde(skip_serializing_if = "Option::is_none")]
    pub pool_ticker: Option<String>,

    /// True if the signer have registered at least once
    pub has_registered: bool,
}
