use std::{
    fmt::{Display, Formatter},
    num::TryFromIntError,
    ops::{Deref, DerefMut},
};

use serde::{Deserialize, Serialize};

use crate::entities::{
    BlockNumber,
    arithmetic_operation_wrapper::{
        impl_add_to_wrapper, impl_partial_eq_to_wrapper, impl_sub_to_wrapper,
    },
};

#[cfg(target_family = "wasm")]
use wasm_bindgen::prelude::*;

/// BlockNumberOffset represents the offset of a block number
#[derive(
    Debug, Copy, Clone, Default, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize, Hash,
)]
#[cfg_attr(target_family = "wasm", wasm_bindgen)]
pub struct BlockNumberOffset(pub u64);

impl Display for BlockNumberOffset {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl Deref for BlockNumberOffset {
    type Target = u64;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for BlockNumberOffset {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl TryFrom<i64> for BlockNumberOffset {
    type Error = TryFromIntError;

    fn try_from(value: i64) -> Result<Self, Self::Error> {
        Ok(BlockNumberOffset(u64::try_from(value)?))
    }
}

impl TryFrom<BlockNumberOffset> for i64 {
    type Error = TryFromIntError;

    fn try_from(value: BlockNumberOffset) -> Result<Self, Self::Error> {
        value.0.try_into()
    }
}

//TODO: clem, check if we still need those converter avec switching to BlockNumberOffset
impl From<BlockNumber> for BlockNumberOffset {
    fn from(value: BlockNumber) -> Self {
        BlockNumberOffset(value.0)
    }
}

//TODO: clem, check if we still need those converter avec switching to BlockNumberOffset
impl From<BlockNumberOffset> for BlockNumber {
    fn from(value: BlockNumberOffset) -> Self {
        BlockNumber(value.0)
    }
}

impl_add_to_wrapper!(BlockNumberOffset, u64);
impl_sub_to_wrapper!(BlockNumberOffset, u64);
impl_partial_eq_to_wrapper!(BlockNumberOffset, u64);

impl Add<BlockNumber> for BlockNumberOffset {
    type Output = Self;

    fn add(self, rhs: BlockNumber) -> Self::Output {
        self + *rhs
    }
}

impl Sub<BlockNumber> for BlockNumberOffset {
    type Output = Self;

    fn sub(self, rhs: BlockNumber) -> Self::Output {
        self - *rhs
    }
}

impl AddAssign<BlockNumber> for BlockNumberOffset {
    fn add_assign(&mut self, rhs: BlockNumber) {
        *self = *self + rhs;
    }
}

#[cfg(test)]
mod tests {
    use crate::entities::arithmetic_operation_wrapper::tests::test_op_assign;

    use super::*;

    #[test]
    fn test_display() {
        assert_eq!(format!("{}", BlockNumberOffset(72)), "72");
        assert_eq!(format!("{}", &BlockNumberOffset(13224)), "13224");
    }

    #[test]
    fn test_serialize() {
        assert_eq!(serde_json::to_string(&BlockNumberOffset(72)).unwrap(), "72");
    }

    #[test]
    fn test_deserialize() {
        let kes_period: BlockNumberOffset = serde_json::from_str("13224").unwrap();
        assert_eq!(kes_period, BlockNumberOffset(13224));
    }

    #[test]
    #[allow(clippy::op_ref)]
    fn test_add() {
        assert_eq!(
            BlockNumberOffset(4),
            BlockNumberOffset(1) + BlockNumberOffset(3)
        );
        assert_eq!(BlockNumberOffset(4), BlockNumberOffset(1) + 3_u64);
        assert_eq!(BlockNumberOffset(4), BlockNumberOffset(1) + &3_u64);

        assert_eq!(BlockNumberOffset(4), 3_u64 + BlockNumberOffset(1));
        assert_eq!(BlockNumberOffset(4), 3_u64 + &BlockNumberOffset(1));
        assert_eq!(BlockNumberOffset(4), &3_u64 + BlockNumberOffset(1));
        assert_eq!(BlockNumberOffset(4), &3_u64 + &BlockNumberOffset(1));

        test_op_assign!(BlockNumberOffset(1), +=, BlockNumberOffset(3) => BlockNumberOffset(4));
        test_op_assign!(BlockNumberOffset(1), +=, 3_u64 => BlockNumberOffset(4));
        test_op_assign!(BlockNumberOffset(1), +=, &3_u64 => BlockNumberOffset(4));

        test_op_assign!(1_u64, +=, BlockNumberOffset(3) => 4_u64);
        test_op_assign!(1_u64, +=, &BlockNumberOffset(3) => 4_u64);
    }

    #[test]
    #[allow(clippy::op_ref)]
    fn test_sub() {
        assert_eq!(
            BlockNumberOffset(8),
            BlockNumberOffset(14) - BlockNumberOffset(6)
        );
        assert_eq!(BlockNumberOffset(8), BlockNumberOffset(14) - 6_u64);
        assert_eq!(BlockNumberOffset(8), BlockNumberOffset(14) - &6_u64);

        assert_eq!(BlockNumberOffset(8), 6_u64 - BlockNumberOffset(14));
        assert_eq!(BlockNumberOffset(8), 6_u64 - &BlockNumberOffset(14));
        assert_eq!(BlockNumberOffset(8), &6_u64 - BlockNumberOffset(14));
        assert_eq!(BlockNumberOffset(8), &6_u64 - &BlockNumberOffset(14));

        test_op_assign!(BlockNumberOffset(14), -=, BlockNumberOffset(6) => BlockNumberOffset(8));
        test_op_assign!(BlockNumberOffset(14), -=, 6_u64 => BlockNumberOffset(8));
        test_op_assign!(BlockNumberOffset(14), -=, &6_u64 => BlockNumberOffset(8));

        test_op_assign!(14_u64, -=, BlockNumberOffset(6) => 8_u64);
        test_op_assign!(14_u64, -=, &BlockNumberOffset(6) => 8_u64);
    }

    #[test]
    fn saturating_sub() {
        assert_eq!(
            BlockNumberOffset(0),
            BlockNumberOffset(1) - BlockNumberOffset(5)
        );
        assert_eq!(BlockNumberOffset(0), BlockNumberOffset(1) - 5_u64);
    }

    #[test]
    fn test_eq() {
        assert_eq!(BlockNumberOffset(1), BlockNumberOffset(1));
        assert_eq!(BlockNumberOffset(2), &BlockNumberOffset(2));
        assert_eq!(&BlockNumberOffset(3), BlockNumberOffset(3));
        assert_eq!(&BlockNumberOffset(4), &BlockNumberOffset(4));

        assert_eq!(BlockNumberOffset(5), 5);
        assert_eq!(BlockNumberOffset(6), &6);
        assert_eq!(&BlockNumberOffset(7), 7);
        assert_eq!(&BlockNumberOffset(8), &8);

        assert_eq!(9, BlockNumberOffset(9));
        assert_eq!(10, &BlockNumberOffset(10));
        assert_eq!(&11, BlockNumberOffset(11));
        assert_eq!(&12, &BlockNumberOffset(12));
    }

    #[test]
    fn test_add_block_number() {
        assert_eq!(BlockNumberOffset(4), BlockNumberOffset(1) + BlockNumber(3));
    }

    #[test]
    fn test_sub_block_number() {
        assert_eq!(BlockNumberOffset(3), BlockNumberOffset(4) - BlockNumber(1));
    }

    #[test]
    fn test_add_asign_block_number() {
        test_op_assign!(BlockNumberOffset(1), +=, BlockNumber(3) => BlockNumberOffset(4));
        test_op_assign!(BlockNumberOffset(1), +=, BlockNumber(3) => &BlockNumberOffset(4));
        test_op_assign!(BlockNumberOffset(1), +=, 3 => BlockNumberOffset(4));
        test_op_assign!(BlockNumberOffset(1), +=, &3 => BlockNumberOffset(4));
    }

    #[test]
    fn test_from_block_number() {
        let block_number = BlockNumber(42);
        let block_number_offset: BlockNumberOffset = block_number.into();
        assert_eq!(block_number_offset, BlockNumberOffset(42));
    }

    #[test]
    fn test_from_block_number_offset() {
        let block_number_offset = BlockNumberOffset(42);
        let block_number: BlockNumber = block_number_offset.into();
        assert_eq!(block_number, BlockNumber(42));
    }
}
