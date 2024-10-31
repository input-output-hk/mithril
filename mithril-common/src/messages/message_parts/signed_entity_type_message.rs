use serde::{Deserialize, Serialize};
use strum::Display;

use crate::entities::{BlockNumber, CardanoDbBeacon, Epoch, ImmutableFileNumber, SignedEntityType};

/// A point in the Cardano chain at which a Mithril certificate of the Cardano Database should be
/// produced.
///
/// Note: This struct is a temporary solution to allow removal of the network field from
/// its [equivalent entity][CardanoDbBeacon] while keeping compatibility with not yet updated
/// nodes.
/// For new messages, use the [CardanoDbBeacon] directly.
// TODO: Remove this enum when all nodes are updated, and use the [CardanoDbBeacon] directly as before.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct CardanoDbBeaconMessagePart {
    /// Cardano network
    #[serde(skip_serializing_if = "Option::is_none")]
    pub network: Option<String>,

    /// Cardano chain epoch number
    pub epoch: Epoch,

    /// Number of the last included immutable files for the digest computation
    pub immutable_file_number: ImmutableFileNumber,
}

/// The signed entity type that represents a type of data signed by the Mithril
///
/// Note: This enum is a temporary solution to allow removal of the network field from
/// the [CardanoDbBeacon] while keeping compatibility with not yet updated nodes.
///
/// For new messages, use the [SignedEntityType] directly.
// TODO: Remove this enum when all nodes are updated, and use the [SignedEntityType] directly as before.
#[derive(Display, Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[strum(serialize_all = "PascalCase")]
pub enum SignedEntityTypeMessagePart {
    /// Mithril stake distribution
    MithrilStakeDistribution(Epoch),

    /// Cardano Stake Distribution
    CardanoStakeDistribution(Epoch),

    /// Full Cardano Immutable Files
    CardanoImmutableFilesFull(CardanoDbBeaconMessagePart),

    /// Cardano Transactions
    CardanoTransactions(Epoch, BlockNumber),
}

impl CardanoDbBeaconMessagePart {
    /// `CardanoDbBeaconMessagePart` factory
    pub fn new<T: Into<String>>(
        network: T,
        epoch: Epoch,
        immutable_file_number: ImmutableFileNumber,
    ) -> Self {
        Self {
            network: Some(network.into()),
            epoch,
            immutable_file_number,
        }
    }

    /// Create a new `CardanoDbBeaconMessagePart` without network
    pub fn new_without_network(epoch: Epoch, immutable_file_number: ImmutableFileNumber) -> Self {
        Self {
            network: None,
            epoch,
            immutable_file_number,
        }
    }
}

impl From<CardanoDbBeaconMessagePart> for CardanoDbBeacon {
    fn from(message: CardanoDbBeaconMessagePart) -> Self {
        Self::new(
            message.network.unwrap_or_default(),
            *message.epoch,
            message.immutable_file_number,
        )
    }
}

impl<N: Into<String>> From<(CardanoDbBeacon, N)> for CardanoDbBeaconMessagePart {
    fn from((beacon, network): (CardanoDbBeacon, N)) -> Self {
        CardanoDbBeaconMessagePart::new(network.into(), beacon.epoch, beacon.immutable_file_number)
    }
}

impl PartialEq<CardanoDbBeaconMessagePart> for CardanoDbBeacon {
    fn eq(&self, other: &CardanoDbBeaconMessagePart) -> bool {
        self.epoch == other.epoch && self.immutable_file_number == other.immutable_file_number
    }
}

impl PartialEq<CardanoDbBeacon> for CardanoDbBeaconMessagePart {
    fn eq(&self, other: &CardanoDbBeacon) -> bool {
        other.eq(self)
    }
}

impl From<SignedEntityTypeMessagePart> for SignedEntityType {
    fn from(message: SignedEntityTypeMessagePart) -> Self {
        match message {
            SignedEntityTypeMessagePart::MithrilStakeDistribution(epoch) => {
                Self::MithrilStakeDistribution(epoch)
            }
            SignedEntityTypeMessagePart::CardanoStakeDistribution(epoch) => {
                Self::CardanoStakeDistribution(epoch)
            }
            SignedEntityTypeMessagePart::CardanoImmutableFilesFull(beacon) => {
                Self::CardanoImmutableFilesFull(beacon.into())
            }
            SignedEntityTypeMessagePart::CardanoTransactions(epoch, block_number) => {
                Self::CardanoTransactions(epoch, block_number)
            }
        }
    }
}

impl PartialEq<SignedEntityTypeMessagePart> for SignedEntityType {
    fn eq(&self, other: &SignedEntityTypeMessagePart) -> bool {
        match (&self, &other) {
            (
                &SignedEntityType::MithrilStakeDistribution(left_epoch),
                &SignedEntityTypeMessagePart::MithrilStakeDistribution(right_epoch),
            ) => left_epoch == right_epoch,
            (
                &SignedEntityType::CardanoStakeDistribution(left_epoch),
                &SignedEntityTypeMessagePart::CardanoStakeDistribution(right_epoch),
            ) => left_epoch == right_epoch,
            (
                &SignedEntityType::CardanoImmutableFilesFull(left_beacon),
                &SignedEntityTypeMessagePart::CardanoImmutableFilesFull(right_beacon),
            ) => left_beacon == right_beacon,
            (
                &SignedEntityType::CardanoTransactions(left_epoch, left_block_number),
                &SignedEntityTypeMessagePart::CardanoTransactions(right_epoch, right_block_number),
            ) => left_epoch == right_epoch && left_block_number == right_block_number,
            _ => false,
        }
    }
}

impl PartialEq<SignedEntityType> for SignedEntityTypeMessagePart {
    fn eq(&self, other: &SignedEntityType) -> bool {
        other.eq(self)
    }
}

impl<N: Into<String>> From<(SignedEntityType, N)> for SignedEntityTypeMessagePart {
    fn from((signed_entity_type, network): (SignedEntityType, N)) -> Self {
        match signed_entity_type {
            SignedEntityType::MithrilStakeDistribution(epoch) => {
                Self::MithrilStakeDistribution(epoch)
            }
            SignedEntityType::CardanoStakeDistribution(epoch) => {
                Self::CardanoStakeDistribution(epoch)
            }
            SignedEntityType::CardanoImmutableFilesFull(beacon) => {
                Self::CardanoImmutableFilesFull(CardanoDbBeaconMessagePart::new(
                    network.into(),
                    beacon.epoch,
                    beacon.immutable_file_number,
                ))
            }
            SignedEntityType::CardanoTransactions(epoch, block_number) => {
                Self::CardanoTransactions(epoch, block_number)
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn convert_message_to_cardano_db_beacon_entity() {
        assert_eq!(
            CardanoDbBeacon::new("whatever", 12, 48),
            CardanoDbBeacon::from(CardanoDbBeaconMessagePart::new("whatever", Epoch(12), 48))
        );
    }

    #[test]
    fn convert_cardano_db_beacon_entity_to_message() {
        assert_eq!(
            CardanoDbBeaconMessagePart::new("whatever", Epoch(12), 48),
            CardanoDbBeaconMessagePart::from((CardanoDbBeacon::new("unused", 12, 48), "whatever"))
        );
    }

    #[test]
    fn convert_message_to_signed_entity_type_entity() {
        assert_eq!(
            SignedEntityType::CardanoImmutableFilesFull(CardanoDbBeacon::new("whatever", 12, 48)),
            SignedEntityType::from(SignedEntityTypeMessagePart::CardanoImmutableFilesFull(
                CardanoDbBeaconMessagePart::new("whatever", Epoch(12), 48)
            ))
        );
        assert_eq!(
            SignedEntityType::MithrilStakeDistribution(Epoch(123)),
            SignedEntityType::from(SignedEntityTypeMessagePart::MithrilStakeDistribution(
                Epoch(123)
            ))
        );
        assert_eq!(
            SignedEntityType::CardanoStakeDistribution(Epoch(123)),
            SignedEntityType::from(SignedEntityTypeMessagePart::CardanoStakeDistribution(
                Epoch(123)
            ))
        );
        assert_eq!(
            SignedEntityType::CardanoTransactions(Epoch(123), BlockNumber(4678)),
            SignedEntityType::from(SignedEntityTypeMessagePart::CardanoTransactions(
                Epoch(123),
                BlockNumber(4678)
            ))
        );
    }

    #[test]
    fn convert_signed_entity_type_entity_to_message() {
        assert_eq!(
            SignedEntityTypeMessagePart::CardanoImmutableFilesFull(
                CardanoDbBeaconMessagePart::new("whatever", Epoch(12), 48)
            ),
            SignedEntityTypeMessagePart::from((
                SignedEntityType::CardanoImmutableFilesFull(CardanoDbBeacon::new("unused", 12, 48)),
                "whatever"
            ))
        );
        assert_eq!(
            SignedEntityTypeMessagePart::MithrilStakeDistribution(Epoch(123)),
            SignedEntityTypeMessagePart::from((
                SignedEntityType::MithrilStakeDistribution(Epoch(123)),
                "unused"
            ))
        );
        assert_eq!(
            SignedEntityTypeMessagePart::CardanoStakeDistribution(Epoch(123)),
            SignedEntityTypeMessagePart::from((
                SignedEntityType::CardanoStakeDistribution(Epoch(123)),
                "unused"
            ))
        );
        assert_eq!(
            SignedEntityTypeMessagePart::CardanoTransactions(Epoch(123), BlockNumber(4678)),
            SignedEntityTypeMessagePart::from((
                SignedEntityType::CardanoTransactions(Epoch(123), BlockNumber(4678)),
                "unused"
            ))
        );
    }

    #[test]
    fn comparing_message_to_cardano_db_beacon_entity() {
        let (epoch, immutable, network) = (Epoch(10), 50, "whatever".to_string());
        assert_eq!(
            CardanoDbBeacon::new(&network, *epoch, immutable),
            CardanoDbBeaconMessagePart::new(&network, epoch, immutable),
        );
        assert_eq!(
            CardanoDbBeaconMessagePart::new(&network, epoch, immutable),
            CardanoDbBeacon::new(&network, *epoch, immutable),
        );

        // Changing epoch should make them not equal
        assert_ne!(
            CardanoDbBeacon::new(&network, *epoch + 1, immutable),
            CardanoDbBeaconMessagePart::new(&network, epoch, immutable),
        );
        assert_ne!(
            CardanoDbBeaconMessagePart::new(&network, epoch + 1, immutable),
            CardanoDbBeacon::new(&network, *epoch, immutable),
        );

        // Changing immutable file number should make them not equal
        assert_ne!(
            CardanoDbBeacon::new(&network, *epoch, immutable + 1),
            CardanoDbBeaconMessagePart::new(&network, epoch, immutable),
        );
        assert_ne!(
            CardanoDbBeaconMessagePart::new(&network, epoch, immutable + 1),
            CardanoDbBeacon::new(&network, *epoch, immutable),
        );

        // Changing network should not matter
        assert_eq!(
            CardanoDbBeacon::new("another network", *epoch, immutable),
            CardanoDbBeaconMessagePart::new(&network, epoch, immutable),
        );
        assert_eq!(
            CardanoDbBeaconMessagePart::new("another network", epoch, immutable),
            CardanoDbBeacon::new(&network, *epoch, immutable),
        );

        // Missing network should not matter too
        assert_eq!(
            CardanoDbBeacon::new(&network, *epoch, immutable),
            CardanoDbBeaconMessagePart {
                network: None,
                ..CardanoDbBeaconMessagePart::new(&network, epoch, immutable)
            },
        );
        assert_eq!(
            CardanoDbBeaconMessagePart {
                network: None,
                ..CardanoDbBeaconMessagePart::new(&network, epoch, immutable)
            },
            CardanoDbBeacon::new(&network, *epoch, immutable),
        );
    }

    #[test]
    fn comparing_message_to_signed_entity_type_entity() {
        let (epoch, immutable) = (Epoch(10), 50);
        let (block_number, network) = (BlockNumber(4678), "whatever".to_string());

        // CardanoImmutableFilesFull
        assert_eq!(
            SignedEntityType::CardanoImmutableFilesFull(CardanoDbBeacon::new(
                &network, *epoch, immutable
            )),
            SignedEntityTypeMessagePart::CardanoImmutableFilesFull(
                CardanoDbBeaconMessagePart::new("whatever", epoch, immutable)
            ),
        );
        assert_eq!(
            SignedEntityTypeMessagePart::CardanoImmutableFilesFull(
                CardanoDbBeaconMessagePart::new(&network, epoch, immutable)
            ),
            SignedEntityType::CardanoImmutableFilesFull(CardanoDbBeacon::new(
                &network, *epoch, immutable
            )),
        );
        assert_ne!(
            SignedEntityType::CardanoImmutableFilesFull(CardanoDbBeacon::new(
                &network,
                *epoch + 10,
                immutable
            )),
            SignedEntityTypeMessagePart::CardanoImmutableFilesFull(
                CardanoDbBeaconMessagePart::new("whatever", epoch, immutable)
            ),
        );
        assert_ne!(
            SignedEntityTypeMessagePart::CardanoImmutableFilesFull(
                CardanoDbBeaconMessagePart::new(&network, epoch + 10, immutable)
            ),
            SignedEntityType::CardanoImmutableFilesFull(CardanoDbBeacon::new(
                &network, *epoch, immutable
            )),
        );

        // MithrilStakeDistribution
        assert_eq!(
            SignedEntityType::MithrilStakeDistribution(epoch),
            SignedEntityTypeMessagePart::MithrilStakeDistribution(epoch),
        );
        assert_eq!(
            SignedEntityTypeMessagePart::MithrilStakeDistribution(epoch),
            SignedEntityType::MithrilStakeDistribution(epoch),
        );
        assert_ne!(
            SignedEntityType::MithrilStakeDistribution(epoch + 1),
            SignedEntityTypeMessagePart::MithrilStakeDistribution(epoch),
        );
        assert_ne!(
            SignedEntityTypeMessagePart::MithrilStakeDistribution(epoch + 1),
            SignedEntityType::MithrilStakeDistribution(epoch),
        );

        // CardanoStakeDistribution
        assert_eq!(
            SignedEntityType::CardanoStakeDistribution(epoch),
            SignedEntityTypeMessagePart::CardanoStakeDistribution(epoch),
        );
        assert_eq!(
            SignedEntityTypeMessagePart::CardanoStakeDistribution(epoch),
            SignedEntityType::CardanoStakeDistribution(epoch),
        );
        assert_ne!(
            SignedEntityType::CardanoStakeDistribution(epoch + 5),
            SignedEntityTypeMessagePart::CardanoStakeDistribution(epoch),
        );
        assert_ne!(
            SignedEntityTypeMessagePart::CardanoStakeDistribution(epoch + 5),
            SignedEntityType::CardanoStakeDistribution(epoch),
        );

        // CardanoTransactions
        assert_eq!(
            SignedEntityType::CardanoTransactions(epoch, block_number),
            SignedEntityTypeMessagePart::CardanoTransactions(epoch, block_number),
        );
        assert_eq!(
            SignedEntityTypeMessagePart::CardanoTransactions(epoch, block_number),
            SignedEntityType::CardanoTransactions(epoch, block_number),
        );
        assert_ne!(
            SignedEntityType::CardanoTransactions(epoch + 1, block_number),
            SignedEntityTypeMessagePart::CardanoTransactions(epoch, block_number),
        );
        assert_ne!(
            SignedEntityTypeMessagePart::CardanoTransactions(epoch + 1, block_number),
            SignedEntityType::CardanoTransactions(epoch, block_number),
        );
        assert_ne!(
            SignedEntityType::CardanoTransactions(epoch, block_number + 3),
            SignedEntityTypeMessagePart::CardanoTransactions(epoch, block_number),
        );
        assert_ne!(
            SignedEntityTypeMessagePart::CardanoTransactions(epoch, block_number + 3),
            SignedEntityType::CardanoTransactions(epoch, block_number),
        );
    }
}
