use std::fmt::{Display, Formatter};
use std::ops::{Deref, DerefMut};

use serde::{Deserialize, Serialize};

use crate::entities::wrapper_helpers::{
    impl_add_to_wrapper, impl_div_to_wrapper, impl_mul_to_wrapper, impl_partial_eq_to_wrapper,
    impl_rem_to_wrapper, impl_sub_to_wrapper,
};
use crate::signable_builder::Beacon;

/// BlockNumber is the block number of a Cardano transaction.
#[derive(
    Debug, Copy, Clone, Default, PartialEq, Serialize, Deserialize, Hash, Eq, PartialOrd, Ord,
)]
pub struct BlockNumber(pub u64);

impl Beacon for BlockNumber {}

impl Display for BlockNumber {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl Deref for BlockNumber {
    type Target = u64;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for BlockNumber {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl_add_to_wrapper!(BlockNumber, u64);
impl_sub_to_wrapper!(BlockNumber, u64);
impl_mul_to_wrapper!(BlockNumber, u64);
impl_div_to_wrapper!(BlockNumber, u64);
impl_rem_to_wrapper!(BlockNumber, u64);
impl_partial_eq_to_wrapper!(BlockNumber, u64);

#[cfg(test)]
mod tests {
    use crate::entities::wrapper_helpers::tests::test_op_assign;

    use super::*;

    #[test]
    fn test_display() {
        assert_eq!(format!("{}", BlockNumber(72)), "72");
        assert_eq!(format!("{}", &BlockNumber(13224)), "13224");
    }

    #[test]
    fn test_serialize() {
        assert_eq!(serde_json::to_string(&BlockNumber(72)).unwrap(), "72");
    }

    #[test]
    fn test_deserialize() {
        let block_number: BlockNumber = serde_json::from_str("13224").unwrap();
        assert_eq!(block_number, BlockNumber(13224));
    }

    #[test]
    #[allow(clippy::op_ref)]
    fn test_add() {
        assert_eq!(BlockNumber(4), BlockNumber(1) + BlockNumber(3));
        assert_eq!(BlockNumber(4), BlockNumber(1) + 3_u64);
        assert_eq!(BlockNumber(4), BlockNumber(1) + &3_u64);

        assert_eq!(BlockNumber(4), 3_u64 + BlockNumber(1));
        assert_eq!(BlockNumber(4), 3_u64 + &BlockNumber(1));
        assert_eq!(BlockNumber(4), &3_u64 + BlockNumber(1));
        assert_eq!(BlockNumber(4), &3_u64 + &BlockNumber(1));

        test_op_assign!(BlockNumber(1), +=, BlockNumber(3) => BlockNumber(4));
        test_op_assign!(BlockNumber(1), +=, 3_u64 => BlockNumber(4));
        test_op_assign!(BlockNumber(1), +=, &3_u64 => BlockNumber(4));

        test_op_assign!(1_u64, +=, BlockNumber(3) => 4_u64);
        test_op_assign!(1_u64, +=, &BlockNumber(3) => 4_u64);
    }

    #[test]
    #[allow(clippy::op_ref)]
    fn test_sub() {
        assert_eq!(BlockNumber(8), BlockNumber(14) - BlockNumber(6));
        assert_eq!(BlockNumber(8), BlockNumber(14) - 6_u64);
        assert_eq!(BlockNumber(8), BlockNumber(14) - &6_u64);

        assert_eq!(BlockNumber(8), 6_u64 - BlockNumber(14));
        assert_eq!(BlockNumber(8), 6_u64 - &BlockNumber(14));
        assert_eq!(BlockNumber(8), &6_u64 - BlockNumber(14));
        assert_eq!(BlockNumber(8), &6_u64 - &BlockNumber(14));

        test_op_assign!(BlockNumber(14), -=, BlockNumber(6) => BlockNumber(8));
        test_op_assign!(BlockNumber(14), -=, 6_u64 => BlockNumber(8));
        test_op_assign!(BlockNumber(14), -=, &6_u64 => BlockNumber(8));

        test_op_assign!(14_u64, -=, BlockNumber(6) => 8_u64);
        test_op_assign!(14_u64, -=, &BlockNumber(6) => 8_u64);
    }

    #[test]
    fn saturating_sub() {
        assert_eq!(BlockNumber(0), BlockNumber(1) - BlockNumber(5));
        assert_eq!(BlockNumber(0), BlockNumber(1) - 5_u64);
    }

    #[test]
    #[allow(clippy::op_ref)]
    fn test_mul() {
        assert_eq!(BlockNumber(6), BlockNumber(2) * BlockNumber(3));
        assert_eq!(BlockNumber(6), BlockNumber(2) * 3_u64);
        assert_eq!(BlockNumber(6), BlockNumber(2) * &3_u64);

        assert_eq!(BlockNumber(6), 3_u64 * BlockNumber(2));
        assert_eq!(BlockNumber(6), 3_u64 * &BlockNumber(2));
        assert_eq!(BlockNumber(6), &3_u64 * BlockNumber(2));
        assert_eq!(BlockNumber(6), &3_u64 * &BlockNumber(2));

        test_op_assign!(BlockNumber(2), *=, BlockNumber(3) => BlockNumber(6));
        test_op_assign!(BlockNumber(2), *=, 3_u64 => BlockNumber(6));
        test_op_assign!(BlockNumber(2), *=, &3_u64 => BlockNumber(6));

        test_op_assign!(2_u64, *=, BlockNumber(3) => 6_u64);
        test_op_assign!(2_u64, *=, &BlockNumber(3) => 6_u64);
    }

    #[test]
    #[allow(clippy::op_ref)]
    fn test_div() {
        assert_eq!(BlockNumber(6), BlockNumber(18) / BlockNumber(3));
        assert_eq!(BlockNumber(6), BlockNumber(18) / 3_u64);
        assert_eq!(BlockNumber(6), BlockNumber(18) / &3_u64);

        assert_eq!(BlockNumber(6), 12_u64 / BlockNumber(2));
        assert_eq!(BlockNumber(6), 12_u64 / &BlockNumber(2));
        assert_eq!(BlockNumber(6), &12_u64 / BlockNumber(2));
        assert_eq!(BlockNumber(6), &12_u64 / &BlockNumber(2));

        test_op_assign!(BlockNumber(18), /=, BlockNumber(3) => BlockNumber(6));
        test_op_assign!(BlockNumber(18), /=, 3_u64 => BlockNumber(6));
        test_op_assign!(BlockNumber(18), /=, &3_u64 => BlockNumber(6));

        test_op_assign!(18_u64, /=, BlockNumber(3) => 6_u64);
        test_op_assign!(18_u64, /=, &BlockNumber(3) => 6_u64);
    }

    #[test]
    #[allow(clippy::op_ref)]
    fn test_rem() {
        assert_eq!(BlockNumber(3), BlockNumber(18) % BlockNumber(5));
        assert_eq!(BlockNumber(3), BlockNumber(18) % 5_u64);
        assert_eq!(BlockNumber(3), BlockNumber(18) % &5_u64);

        assert_eq!(BlockNumber(6), 20_u64 % BlockNumber(7));
        assert_eq!(BlockNumber(6), 20_u64 % &BlockNumber(7));
        assert_eq!(BlockNumber(6), &20_u64 % BlockNumber(7));
        assert_eq!(BlockNumber(6), &20_u64 % &BlockNumber(7));

        test_op_assign!(BlockNumber(18), %=, BlockNumber(5) => BlockNumber(3));
        test_op_assign!(BlockNumber(18), %=, 5_u64 => BlockNumber(3));
        test_op_assign!(BlockNumber(18), %=, &5_u64 => BlockNumber(3));

        test_op_assign!(18_u64, %=, BlockNumber(5) => 3_u64);
        test_op_assign!(18_u64, %=, &BlockNumber(5) => 3_u64);
    }

    #[test]
    fn test_eq() {
        assert_eq!(BlockNumber(1), BlockNumber(1));
        assert_eq!(BlockNumber(2), &BlockNumber(2));
        assert_eq!(&BlockNumber(3), BlockNumber(3));
        assert_eq!(&BlockNumber(4), &BlockNumber(4));

        assert_eq!(BlockNumber(5), 5);
        assert_eq!(BlockNumber(6), &6);
        assert_eq!(&BlockNumber(7), 7);
        assert_eq!(&BlockNumber(8), &8);

        assert_eq!(9, BlockNumber(9));
        assert_eq!(10, &BlockNumber(10));
        assert_eq!(&11, BlockNumber(11));
        assert_eq!(&12, &BlockNumber(12));
    }
}
