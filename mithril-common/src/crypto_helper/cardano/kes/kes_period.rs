use std::fmt::{Display, Formatter};
use std::num::TryFromIntError;
use std::ops::{Deref, DerefMut, Sub};

use serde::{Deserialize, Serialize};

use crate::crypto_helper::KesEvolutions;
use crate::entities::arithmetic_operation_wrapper::{
    impl_add_to_wrapper, impl_partial_eq_to_wrapper,
};

/// KesPeriod represents the KES period used to check if the KES keys are expired
#[derive(
    Debug, Copy, Clone, Default, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize, Hash,
)]
pub struct KesPeriod(pub u64);

impl Display for KesPeriod {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl Deref for KesPeriod {
    type Target = u64;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for KesPeriod {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

// Useful for conversion to sqlite number (that uses i64)
impl TryFrom<KesPeriod> for i64 {
    type Error = TryFromIntError;

    fn try_from(value: KesPeriod) -> Result<Self, Self::Error> {
        i64::try_from(value.0)
    }
}

impl TryFrom<i64> for KesPeriod {
    type Error = TryFromIntError;

    fn try_from(value: i64) -> Result<Self, Self::Error> {
        Ok(KesPeriod(u64::try_from(value)?))
    }
}

impl From<KesPeriod> for u64 {
    fn from(value: KesPeriod) -> Self {
        value.0
    }
}

impl From<u32> for KesPeriod {
    fn from(value: u32) -> Self {
        KesPeriod(value.into())
    }
}

impl From<u64> for KesPeriod {
    fn from(value: u64) -> Self {
        KesPeriod(value)
    }
}

impl Add<KesEvolutions> for KesPeriod {
    type Output = Self;

    fn add(self, rhs: KesEvolutions) -> Self::Output {
        self + *rhs
    }
}

impl Sub<KesPeriod> for KesPeriod {
    type Output = KesEvolutions;

    fn sub(self, rhs: KesPeriod) -> Self::Output {
        KesEvolutions((*self).saturating_sub(*rhs))
    }
}

impl_add_to_wrapper!(KesPeriod, u64);
impl_partial_eq_to_wrapper!(KesPeriod, u64);

#[cfg(test)]
mod tests {
    use crate::entities::arithmetic_operation_wrapper::tests::test_op_assign;

    use super::*;

    #[test]
    fn test_display() {
        assert_eq!(format!("{}", KesPeriod(72)), "72");
        assert_eq!(format!("{}", &KesPeriod(13224)), "13224");
    }

    #[test]
    fn test_serialize() {
        assert_eq!(serde_json::to_string(&KesPeriod(72)).unwrap(), "72");
    }

    #[test]
    fn test_deserialize() {
        let kes_period: KesPeriod = serde_json::from_str("13224").unwrap();
        assert_eq!(kes_period, KesPeriod(13224));
    }

    #[test]
    #[allow(clippy::op_ref)]
    fn test_add() {
        assert_eq!(KesPeriod(4), KesPeriod(1) + KesPeriod(3));
        assert_eq!(KesPeriod(4), KesPeriod(1) + 3_u64);
        assert_eq!(KesPeriod(4), KesPeriod(1) + &3_u64);

        assert_eq!(KesPeriod(4), 3_u64 + KesPeriod(1));
        assert_eq!(KesPeriod(4), 3_u64 + &KesPeriod(1));
        assert_eq!(KesPeriod(4), &3_u64 + KesPeriod(1));
        assert_eq!(KesPeriod(4), &3_u64 + &KesPeriod(1));

        test_op_assign!(KesPeriod(1), +=, KesPeriod(3) => KesPeriod(4));
        test_op_assign!(KesPeriod(1), +=, 3_u64 => KesPeriod(4));
        test_op_assign!(KesPeriod(1), +=, &3_u64 => KesPeriod(4));

        test_op_assign!(1_u64, +=, KesPeriod(3) => 4_u64);
        test_op_assign!(1_u64, +=, &KesPeriod(3) => 4_u64);

        assert_eq!(KesPeriod(4), KesPeriod(1) + KesEvolutions(3));
    }

    #[test]
    #[allow(clippy::op_ref)]
    fn test_sub() {
        assert_eq!(KesEvolutions(8), KesPeriod(14) - KesPeriod(6));
    }

    #[test]
    fn saturating_sub() {
        assert_eq!(KesEvolutions(0), KesPeriod(1) - KesPeriod(5));
    }

    #[test]
    fn test_eq() {
        assert_eq!(KesPeriod(1), KesPeriod(1));
        assert_eq!(KesPeriod(2), &KesPeriod(2));
        assert_eq!(&KesPeriod(3), KesPeriod(3));
        assert_eq!(&KesPeriod(4), &KesPeriod(4));

        assert_eq!(KesPeriod(5), 5);
        assert_eq!(KesPeriod(6), &6);
        assert_eq!(&KesPeriod(7), 7);
        assert_eq!(&KesPeriod(8), &8);

        assert_eq!(9, KesPeriod(9));
        assert_eq!(10, &KesPeriod(10));
        assert_eq!(&11, KesPeriod(11));
        assert_eq!(&12, &KesPeriod(12));
    }
}
