use std::fmt::{Display, Formatter};
use std::num::TryFromIntError;
use std::ops::{Deref, DerefMut};

use serde::{Deserialize, Serialize};

use crate::entities::arithmetic_operation_wrapper::{
    impl_add_to_wrapper, impl_partial_eq_to_wrapper, impl_sub_to_wrapper,
};

/// KesEvolutions represents the KES evolutions used to evolve the KES secret key
///
/// Even if KES evolutions are expressed as KES periods, we create a distinct type to avoid confusion
/// with 'KesPeriod' that represents absolute KES periods in the blockchain.
#[derive(
    Debug, Copy, Clone, Default, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize, Hash,
)]
pub struct KesEvolutions(pub u64);

impl Display for KesEvolutions {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl Deref for KesEvolutions {
    type Target = u64;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for KesEvolutions {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

// Useful for conversion to sqlite number (that uses i64)
impl TryFrom<KesEvolutions> for i64 {
    type Error = TryFromIntError;

    fn try_from(value: KesEvolutions) -> Result<Self, Self::Error> {
        i64::try_from(value.0)
    }
}

impl TryFrom<i64> for KesEvolutions {
    type Error = TryFromIntError;

    fn try_from(value: i64) -> Result<Self, Self::Error> {
        Ok(KesEvolutions(u64::try_from(value)?))
    }
}

impl From<KesEvolutions> for u64 {
    fn from(value: KesEvolutions) -> Self {
        value.0
    }
}

impl_add_to_wrapper!(KesEvolutions, u64);
impl_sub_to_wrapper!(KesEvolutions, u64);
impl_partial_eq_to_wrapper!(KesEvolutions, u64);

#[cfg(test)]
mod tests {
    use crate::entities::arithmetic_operation_wrapper::tests::test_op_assign;

    use super::*;

    #[test]
    fn test_display() {
        assert_eq!(format!("{}", KesEvolutions(72)), "72");
        assert_eq!(format!("{}", &KesEvolutions(13224)), "13224");
    }

    #[test]
    fn test_serialize() {
        assert_eq!(serde_json::to_string(&KesEvolutions(72)).unwrap(), "72");
    }

    #[test]
    fn test_deserialize() {
        let kes_period: KesEvolutions = serde_json::from_str("13224").unwrap();
        assert_eq!(kes_period, KesEvolutions(13224));
    }

    #[test]
    #[allow(clippy::op_ref)]
    fn test_add() {
        assert_eq!(KesEvolutions(4), KesEvolutions(1) + KesEvolutions(3));
        assert_eq!(KesEvolutions(4), KesEvolutions(1) + 3_u64);
        assert_eq!(KesEvolutions(4), KesEvolutions(1) + &3_u64);

        assert_eq!(KesEvolutions(4), 3_u64 + KesEvolutions(1));
        assert_eq!(KesEvolutions(4), 3_u64 + &KesEvolutions(1));
        assert_eq!(KesEvolutions(4), &3_u64 + KesEvolutions(1));
        assert_eq!(KesEvolutions(4), &3_u64 + &KesEvolutions(1));

        test_op_assign!(KesEvolutions(1), +=, KesEvolutions(3) => KesEvolutions(4));
        test_op_assign!(KesEvolutions(1), +=, 3_u64 => KesEvolutions(4));
        test_op_assign!(KesEvolutions(1), +=, &3_u64 => KesEvolutions(4));

        test_op_assign!(1_u64, +=, KesEvolutions(3) => 4_u64);
        test_op_assign!(1_u64, +=, &KesEvolutions(3) => 4_u64);
    }

    #[test]
    #[allow(clippy::op_ref)]
    fn test_sub() {
        assert_eq!(KesEvolutions(8), KesEvolutions(14) - KesEvolutions(6));
        assert_eq!(KesEvolutions(8), KesEvolutions(14) - 6_u64);
        assert_eq!(KesEvolutions(8), KesEvolutions(14) - &6_u64);

        assert_eq!(KesEvolutions(8), 6_u64 - KesEvolutions(14));
        assert_eq!(KesEvolutions(8), 6_u64 - &KesEvolutions(14));
        assert_eq!(KesEvolutions(8), &6_u64 - KesEvolutions(14));
        assert_eq!(KesEvolutions(8), &6_u64 - &KesEvolutions(14));

        test_op_assign!(KesEvolutions(14), -=, KesEvolutions(6) => KesEvolutions(8));
        test_op_assign!(KesEvolutions(14), -=, 6_u64 => KesEvolutions(8));
        test_op_assign!(KesEvolutions(14), -=, &6_u64 => KesEvolutions(8));

        test_op_assign!(14_u64, -=, KesEvolutions(6) => 8_u64);
        test_op_assign!(14_u64, -=, &KesEvolutions(6) => 8_u64);
    }

    #[test]
    fn saturating_sub() {
        assert_eq!(KesEvolutions(0), KesEvolutions(1) - KesEvolutions(5));
        assert_eq!(KesEvolutions(0), KesEvolutions(1) - 5_u64);
    }

    #[test]
    fn test_eq() {
        assert_eq!(KesEvolutions(1), KesEvolutions(1));
        assert_eq!(KesEvolutions(2), &KesEvolutions(2));
        assert_eq!(&KesEvolutions(3), KesEvolutions(3));
        assert_eq!(&KesEvolutions(4), &KesEvolutions(4));

        assert_eq!(KesEvolutions(5), 5);
        assert_eq!(KesEvolutions(6), &6);
        assert_eq!(&KesEvolutions(7), 7);
        assert_eq!(&KesEvolutions(8), &8);

        assert_eq!(9, KesEvolutions(9));
        assert_eq!(10, &KesEvolutions(10));
        assert_eq!(&11, KesEvolutions(11));
        assert_eq!(&12, &KesEvolutions(12));
    }
}
