use std::fmt::{Display, Formatter};
use std::num::TryFromIntError;
use std::ops::{Deref, DerefMut};

use serde::{Deserialize, Serialize};

use crate::entities::arithmetic_operation_wrapper::{
    impl_add_to_wrapper, impl_partial_eq_to_wrapper, impl_sub_to_wrapper,
};

/// [Cardano Slot number](https://docs.cardano.org/learn/cardano-node/#slotsandepochs)
#[derive(
    Debug, Copy, Clone, Default, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize, Hash,
)]
pub struct SlotNumber(pub u64);

impl Display for SlotNumber {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl Deref for SlotNumber {
    type Target = u64;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for SlotNumber {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

// Useful for conversion to sqlite number (that use i64)
impl TryFrom<SlotNumber> for i64 {
    type Error = TryFromIntError;

    fn try_from(value: SlotNumber) -> Result<Self, Self::Error> {
        value.0.try_into()
    }
}

impl_add_to_wrapper!(SlotNumber, u64);
impl_sub_to_wrapper!(SlotNumber, u64);
impl_partial_eq_to_wrapper!(SlotNumber, u64);

#[cfg(test)]
mod tests {
    use crate::entities::arithmetic_operation_wrapper::tests::test_op_assign;

    use super::*;

    #[test]
    fn test_display() {
        assert_eq!(format!("{}", SlotNumber(72)), "72");
        assert_eq!(format!("{}", &SlotNumber(13224)), "13224");
    }

    #[test]
    fn test_serialize() {
        assert_eq!(serde_json::to_string(&SlotNumber(72)).unwrap(), "72");
    }

    #[test]
    fn test_deserialize() {
        let slot_number: SlotNumber = serde_json::from_str("13224").unwrap();
        assert_eq!(slot_number, SlotNumber(13224));
    }

    #[test]
    #[allow(clippy::op_ref)]
    fn test_add() {
        assert_eq!(SlotNumber(4), SlotNumber(1) + SlotNumber(3));
        assert_eq!(SlotNumber(4), SlotNumber(1) + 3_u64);
        assert_eq!(SlotNumber(4), SlotNumber(1) + &3_u64);

        assert_eq!(SlotNumber(4), 3_u64 + SlotNumber(1));
        assert_eq!(SlotNumber(4), 3_u64 + &SlotNumber(1));
        assert_eq!(SlotNumber(4), &3_u64 + SlotNumber(1));
        assert_eq!(SlotNumber(4), &3_u64 + &SlotNumber(1));

        test_op_assign!(SlotNumber(1), +=, SlotNumber(3) => SlotNumber(4));
        test_op_assign!(SlotNumber(1), +=, 3_u64 => SlotNumber(4));
        test_op_assign!(SlotNumber(1), +=, &3_u64 => SlotNumber(4));

        test_op_assign!(1_u64, +=, SlotNumber(3) => 4_u64);
        test_op_assign!(1_u64, +=, &SlotNumber(3) => 4_u64);
    }

    #[test]
    #[allow(clippy::op_ref)]
    fn test_sub() {
        assert_eq!(SlotNumber(8), SlotNumber(14) - SlotNumber(6));
        assert_eq!(SlotNumber(8), SlotNumber(14) - 6_u64);
        assert_eq!(SlotNumber(8), SlotNumber(14) - &6_u64);

        assert_eq!(SlotNumber(8), 6_u64 - SlotNumber(14));
        assert_eq!(SlotNumber(8), 6_u64 - &SlotNumber(14));
        assert_eq!(SlotNumber(8), &6_u64 - SlotNumber(14));
        assert_eq!(SlotNumber(8), &6_u64 - &SlotNumber(14));

        test_op_assign!(SlotNumber(14), -=, SlotNumber(6) => SlotNumber(8));
        test_op_assign!(SlotNumber(14), -=, 6_u64 => SlotNumber(8));
        test_op_assign!(SlotNumber(14), -=, &6_u64 => SlotNumber(8));

        test_op_assign!(14_u64, -=, SlotNumber(6) => 8_u64);
        test_op_assign!(14_u64, -=, &SlotNumber(6) => 8_u64);
    }

    #[test]
    fn saturating_sub() {
        assert_eq!(SlotNumber(0), SlotNumber(1) - SlotNumber(5));
        assert_eq!(SlotNumber(0), SlotNumber(1) - 5_u64);
    }

    #[test]
    fn test_eq() {
        assert_eq!(SlotNumber(1), SlotNumber(1));
        assert_eq!(SlotNumber(2), &SlotNumber(2));
        assert_eq!(&SlotNumber(3), SlotNumber(3));
        assert_eq!(&SlotNumber(4), &SlotNumber(4));

        assert_eq!(SlotNumber(5), 5);
        assert_eq!(SlotNumber(6), &6);
        assert_eq!(&SlotNumber(7), 7);
        assert_eq!(&SlotNumber(8), &8);

        assert_eq!(9, SlotNumber(9));
        assert_eq!(10, &SlotNumber(10));
        assert_eq!(&11, SlotNumber(11));
        assert_eq!(&12, &SlotNumber(12));
    }

    #[test]
    fn test_try_into_i64() {
        let slot_number = SlotNumber(42);
        let number: i64 = slot_number.try_into().unwrap();
        assert_eq!(42, number);
    }
}
