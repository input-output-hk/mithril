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
        Self::new(*message.epoch, message.immutable_file_number)
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
    use std::fmt::Debug;

    use super::*;

    fn assert_commutative_eq<TL: PartialEq<TR> + Debug, TR: PartialEq<TL> + Debug>(
        left: TL,
        right: TR,
    ) {
        assert_eq!(left, right);
        assert_eq!(right, left);
    }

    fn assert_commutative_ne<TL: PartialEq<TR> + Debug, TR: PartialEq<TL> + Debug>(
        left: TL,
        right: TR,
    ) {
        assert_ne!(left, right);
        assert_ne!(right, left);
    }

    #[test]
    fn convert_message_to_cardano_db_beacon_entity() {
        assert_eq!(
            CardanoDbBeacon::new(12, 48),
            CardanoDbBeacon::from(CardanoDbBeaconMessagePart::new("devnet", Epoch(12), 48))
        );
    }

    #[test]
    fn convert_cardano_db_beacon_entity_to_message() {
        assert_eq!(
            CardanoDbBeaconMessagePart::new("devnet", Epoch(12), 48),
            CardanoDbBeaconMessagePart::from((CardanoDbBeacon::new(12, 48), "devnet"))
        );
    }

    #[test]
    fn convert_message_to_signed_entity_type_entity() {
        assert_eq!(
            SignedEntityType::CardanoImmutableFilesFull(CardanoDbBeacon::new(12, 48)),
            SignedEntityType::from(SignedEntityTypeMessagePart::CardanoImmutableFilesFull(
                CardanoDbBeaconMessagePart::new("devnet", Epoch(12), 48)
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
                CardanoDbBeaconMessagePart::new("devnet", Epoch(12), 48)
            ),
            SignedEntityTypeMessagePart::from((
                SignedEntityType::CardanoImmutableFilesFull(CardanoDbBeacon::new(12, 48)),
                "devnet"
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
        let (epoch, immutable, network) = (Epoch(10), 50, "devnet".to_string());
        assert_commutative_eq(
            CardanoDbBeacon::new(*epoch, immutable),
            CardanoDbBeaconMessagePart::new(&network, epoch, immutable),
        );

        // Changing epoch should make them not equal
        assert_commutative_ne(
            CardanoDbBeacon::new(*epoch + 1, immutable),
            CardanoDbBeaconMessagePart::new(&network, epoch, immutable),
        );

        // Changing immutable file number should make them not equal
        assert_commutative_ne(
            CardanoDbBeacon::new(*epoch, immutable + 1),
            CardanoDbBeaconMessagePart::new(&network, epoch, immutable),
        );

        // Changing network should not matter
        assert_commutative_eq(
            CardanoDbBeacon::new(*epoch, immutable),
            CardanoDbBeaconMessagePart::new("another network", epoch, immutable),
        );

        // Missing network should not matter too
        assert_commutative_eq(
            CardanoDbBeacon::new(*epoch, immutable),
            CardanoDbBeaconMessagePart {
                network: None,
                ..CardanoDbBeaconMessagePart::new(&network, epoch, immutable)
            },
        );
    }

    #[test]
    fn comparing_message_to_signed_entity_type_entity() {
        let (epoch, immutable) = (Epoch(10), 50);
        let (block_number, network) = (BlockNumber(4678), "devnet".to_string());

        // CardanoImmutableFilesFull
        assert_commutative_eq(
            SignedEntityType::CardanoImmutableFilesFull(CardanoDbBeacon::new(*epoch, immutable)),
            SignedEntityTypeMessagePart::CardanoImmutableFilesFull(
                CardanoDbBeaconMessagePart::new(&network, epoch, immutable),
            ),
        );
        // Changing network should not matter
        assert_commutative_eq(
            SignedEntityType::CardanoImmutableFilesFull(CardanoDbBeacon::new(*epoch, immutable)),
            SignedEntityTypeMessagePart::CardanoImmutableFilesFull(
                CardanoDbBeaconMessagePart::new("another network", epoch, immutable),
            ),
        );
        assert_commutative_ne(
            SignedEntityType::CardanoImmutableFilesFull(CardanoDbBeacon::new(
                *epoch + 10,
                immutable,
            )),
            SignedEntityTypeMessagePart::CardanoImmutableFilesFull(
                CardanoDbBeaconMessagePart::new(&network, epoch, immutable),
            ),
        );
        assert_commutative_ne(
            SignedEntityType::CardanoImmutableFilesFull(CardanoDbBeacon::new(
                *epoch,
                immutable + 30,
            )),
            SignedEntityTypeMessagePart::CardanoImmutableFilesFull(
                CardanoDbBeaconMessagePart::new(&network, epoch, immutable),
            ),
        );

        // MithrilStakeDistribution
        assert_commutative_eq(
            SignedEntityType::MithrilStakeDistribution(epoch),
            SignedEntityTypeMessagePart::MithrilStakeDistribution(epoch),
        );
        assert_commutative_ne(
            SignedEntityType::MithrilStakeDistribution(epoch + 1),
            SignedEntityTypeMessagePart::MithrilStakeDistribution(epoch),
        );

        // CardanoStakeDistribution
        assert_commutative_eq(
            SignedEntityType::CardanoStakeDistribution(epoch),
            SignedEntityTypeMessagePart::CardanoStakeDistribution(epoch),
        );
        assert_commutative_ne(
            SignedEntityType::CardanoStakeDistribution(epoch + 5),
            SignedEntityTypeMessagePart::CardanoStakeDistribution(epoch),
        );

        // CardanoTransactions
        assert_commutative_eq(
            SignedEntityType::CardanoTransactions(epoch, block_number),
            SignedEntityTypeMessagePart::CardanoTransactions(epoch, block_number),
        );
        assert_commutative_ne(
            SignedEntityType::CardanoTransactions(epoch + 1, block_number),
            SignedEntityTypeMessagePart::CardanoTransactions(epoch, block_number),
        );
        assert_commutative_ne(
            SignedEntityType::CardanoTransactions(epoch, block_number + 3),
            SignedEntityTypeMessagePart::CardanoTransactions(epoch, block_number),
        );
    }
}
