use std::fmt::{Display, Formatter};
use std::ops::{Add, AddAssign, Deref, DerefMut, Sub, SubAssign};

use serde::{Deserialize, Serialize};

/// BlockNumber is the block number of a Cardano transaction.
#[derive(
    Debug, Copy, Clone, Default, PartialEq, Serialize, Deserialize, Hash, Eq, PartialOrd, Ord,
)]
pub struct BlockNumber(u64);

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

impl Add for BlockNumber {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        self + *rhs
    }
}

impl Add<u64> for BlockNumber {
    type Output = Self;

    fn add(self, rhs: u64) -> Self::Output {
        BlockNumber(*self + rhs)
    }
}

impl Add<&u64> for BlockNumber {
    type Output = Self;

    fn add(self, rhs: &u64) -> Self::Output {
        self.add(*rhs)
    }
}

impl AddAssign for BlockNumber {
    fn add_assign(&mut self, rhs: Self) {
        *self = self.add(rhs);
    }
}

impl AddAssign<u64> for BlockNumber {
    fn add_assign(&mut self, rhs: u64) {
        *self = self.add(rhs);
    }
}

impl AddAssign<&u64> for BlockNumber {
    fn add_assign(&mut self, rhs: &u64) {
        *self = self.add(rhs);
    }
}

impl Sub for BlockNumber {
    type Output = Self;

    fn sub(self, rhs: Self) -> Self::Output {
        self - *rhs
    }
}

impl Sub<u64> for BlockNumber {
    type Output = Self;

    fn sub(self, rhs: u64) -> Self::Output {
        BlockNumber(self.saturating_sub(rhs))
    }
}

impl Sub<&u64> for BlockNumber {
    type Output = Self;

    fn sub(self, rhs: &u64) -> Self::Output {
        self.sub(*rhs)
    }
}

impl SubAssign for BlockNumber {
    fn sub_assign(&mut self, rhs: Self) {
        *self = self.sub(rhs);
    }
}

impl SubAssign<u64> for BlockNumber {
    fn sub_assign(&mut self, rhs: u64) {
        *self = self.sub(rhs);
    }
}

impl SubAssign<&u64> for BlockNumber {
    fn sub_assign(&mut self, rhs: &u64) {
        *self = self.sub(rhs);
    }
}

impl PartialEq<u64> for BlockNumber {
    fn eq(&self, other: &u64) -> bool {
        self.0.eq(other)
    }
}

impl PartialEq<&u64> for BlockNumber {
    fn eq(&self, other: &&u64) -> bool {
        self.0.eq(*other)
    }
}

impl PartialEq<&BlockNumber> for BlockNumber {
    fn eq(&self, other: &&BlockNumber) -> bool {
        other.0.eq(self)
    }
}

impl PartialEq<u64> for &BlockNumber {
    fn eq(&self, other: &u64) -> bool {
        self.0.eq(other)
    }
}

impl PartialEq<BlockNumber> for &BlockNumber {
    fn eq(&self, other: &BlockNumber) -> bool {
        other.0.eq(self)
    }
}

impl PartialEq<BlockNumber> for u64 {
    fn eq(&self, other: &BlockNumber) -> bool {
        other.0.eq(self)
    }
}

impl PartialEq<&BlockNumber> for u64 {
    fn eq(&self, other: &&BlockNumber) -> bool {
        other.0.eq(self)
    }
}

impl PartialEq<BlockNumber> for &u64 {
    fn eq(&self, other: &BlockNumber) -> bool {
        other.0.eq(*self)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_display() {
        assert_eq!(format!("{}", BlockNumber(72)), "72");
        assert_eq!(format!("{}", &BlockNumber(13224)), "13224");
    }

    #[test]
    #[allow(clippy::op_ref)]
    fn test_add() {
        assert_eq!(BlockNumber(4), BlockNumber(1) + BlockNumber(3));
        assert_eq!(BlockNumber(4), BlockNumber(1) + 3_u64);
        assert_eq!(BlockNumber(4), BlockNumber(1) + &3_u64);

        let mut block_number = BlockNumber(1);
        block_number += BlockNumber(3);
        assert_eq!(BlockNumber(4), block_number);

        let mut block_number = BlockNumber(1);
        block_number += 3_u64;
        assert_eq!(BlockNumber(4), block_number);

        let mut block_number = BlockNumber(1);
        block_number += &3_u64;
        assert_eq!(BlockNumber(4), block_number);
    }

    #[test]
    #[allow(clippy::op_ref)]
    fn test_sub() {
        assert_eq!(BlockNumber(8), BlockNumber(14) - BlockNumber(6));
        assert_eq!(BlockNumber(8), BlockNumber(14) - 6_u64);
        assert_eq!(BlockNumber(8), BlockNumber(14) - &6_u64);

        let mut block_number = BlockNumber(14);
        block_number -= BlockNumber(6);
        assert_eq!(BlockNumber(8), block_number);

        let mut block_number = BlockNumber(14);
        block_number -= 6_u64;
        assert_eq!(BlockNumber(8), block_number);

        let mut block_number = BlockNumber(14);
        block_number -= &6_u64;
        assert_eq!(BlockNumber(8), block_number);
    }

    #[test]
    fn saturating_sub() {
        assert_eq!(BlockNumber(0), BlockNumber(1) - BlockNumber(5));
        assert_eq!(BlockNumber(0), BlockNumber(1) - 5_u64);
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
