macro_rules! impl_add_to_wrapper {
    ( $wrapper:ident, $inner:ty ) => {
        use std::ops::{Add, AddAssign};

        impl Add for $wrapper {
            type Output = Self;

            fn add(self, rhs: Self) -> Self::Output {
                self + *rhs
            }
        }

        impl Add<$inner> for $wrapper {
            type Output = Self;

            fn add(self, rhs: $inner) -> Self::Output {
                $wrapper(*self + rhs)
            }
        }

        impl Add<&$inner> for $wrapper {
            type Output = Self;

            fn add(self, rhs: &$inner) -> Self::Output {
                self.add(*rhs)
            }
        }

        impl Add<$wrapper> for $inner {
            type Output = $wrapper;

            fn add(self, rhs: $wrapper) -> Self::Output {
                rhs.add(self)
            }
        }

        impl Add<&$wrapper> for $inner {
            type Output = $wrapper;

            fn add(self, rhs: &$wrapper) -> Self::Output {
                rhs.add(self)
            }
        }

        impl Add<$wrapper> for &$inner {
            type Output = $wrapper;

            fn add(self, rhs: $wrapper) -> Self::Output {
                rhs.add(self)
            }
        }

        impl Add<&$wrapper> for &$inner {
            type Output = $wrapper;

            fn add(self, rhs: &$wrapper) -> Self::Output {
                rhs.add(self)
            }
        }

        impl AddAssign for $wrapper {
            fn add_assign(&mut self, rhs: Self) {
                *self = self.add(rhs);
            }
        }

        impl AddAssign<$inner> for $wrapper {
            fn add_assign(&mut self, rhs: $inner) {
                *self = self.add(rhs);
            }
        }

        impl AddAssign<&$inner> for $wrapper {
            fn add_assign(&mut self, rhs: &$inner) {
                *self = self.add(rhs);
            }
        }

        impl AddAssign<$wrapper> for $inner {
            fn add_assign(&mut self, rhs: $wrapper) {
                *self = self.add(rhs.0);
            }
        }

        impl AddAssign<&$wrapper> for $inner {
            fn add_assign(&mut self, rhs: &$wrapper) {
                *self = self.add(rhs.0);
            }
        }
    };
}
pub(crate) use impl_add_to_wrapper;

macro_rules! impl_sub_to_wrapper {
    ( $wrapper:ident, $inner:ty ) => {
        use std::ops::{Sub, SubAssign};

        impl Sub for $wrapper {
            type Output = Self;

            fn sub(self, rhs: Self) -> Self::Output {
                self - *rhs
            }
        }

        impl Sub<$inner> for $wrapper {
            type Output = Self;

            fn sub(self, rhs: $inner) -> Self::Output {
                $wrapper(self.saturating_sub(rhs))
            }
        }

        impl Sub<&$inner> for $wrapper {
            type Output = Self;

            fn sub(self, rhs: &$inner) -> Self::Output {
                self.sub(*rhs)
            }
        }

        impl Sub<$wrapper> for $inner {
            type Output = $wrapper;

            fn sub(self, rhs: $wrapper) -> Self::Output {
                rhs.sub(self)
            }
        }

        impl Sub<&$wrapper> for $inner {
            type Output = $wrapper;

            fn sub(self, rhs: &$wrapper) -> Self::Output {
                rhs.sub(self)
            }
        }

        impl Sub<$wrapper> for &$inner {
            type Output = $wrapper;

            fn sub(self, rhs: $wrapper) -> Self::Output {
                rhs.sub(self)
            }
        }

        impl Sub<&$wrapper> for &$inner {
            type Output = $wrapper;

            fn sub(self, rhs: &$wrapper) -> Self::Output {
                rhs.sub(self)
            }
        }

        impl SubAssign for $wrapper {
            fn sub_assign(&mut self, rhs: Self) {
                *self = self.sub(rhs);
            }
        }

        impl SubAssign<$inner> for $wrapper {
            fn sub_assign(&mut self, rhs: $inner) {
                *self = self.sub(rhs);
            }
        }

        impl SubAssign<&$inner> for $wrapper {
            fn sub_assign(&mut self, rhs: &$inner) {
                *self = self.sub(rhs);
            }
        }

        impl SubAssign<$wrapper> for $inner {
            fn sub_assign(&mut self, rhs: $wrapper) {
                *self = self.sub(rhs.0);
            }
        }

        impl SubAssign<&$wrapper> for $inner {
            fn sub_assign(&mut self, rhs: &$wrapper) {
                *self = self.sub(rhs.0);
            }
        }
    };
}
pub(crate) use impl_sub_to_wrapper;

macro_rules! impl_mul_to_wrapper {
    ( $wrapper:ident, $inner:ty ) => {
        use std::ops::{Mul, MulAssign};

        impl Mul for $wrapper {
            type Output = Self;

            fn mul(self, rhs: Self) -> Self::Output {
                self * *rhs
            }
        }

        impl Mul<$inner> for $wrapper {
            type Output = Self;

            fn mul(self, rhs: $inner) -> Self::Output {
                $wrapper(*self * rhs)
            }
        }

        impl Mul<&$inner> for $wrapper {
            type Output = Self;

            fn mul(self, rhs: &$inner) -> Self::Output {
                self.mul(*rhs)
            }
        }

        impl Mul<$wrapper> for $inner {
            type Output = $wrapper;

            fn mul(self, rhs: $wrapper) -> Self::Output {
                rhs.mul(self)
            }
        }

        impl Mul<&$wrapper> for $inner {
            type Output = $wrapper;

            fn mul(self, rhs: &$wrapper) -> Self::Output {
                rhs.mul(self)
            }
        }

        impl Mul<$wrapper> for &$inner {
            type Output = $wrapper;

            fn mul(self, rhs: $wrapper) -> Self::Output {
                rhs.mul(self)
            }
        }

        impl Mul<&$wrapper> for &$inner {
            type Output = $wrapper;

            fn mul(self, rhs: &$wrapper) -> Self::Output {
                rhs.mul(self)
            }
        }

        impl MulAssign for $wrapper {
            fn mul_assign(&mut self, rhs: Self) {
                *self = self.mul(rhs);
            }
        }

        impl MulAssign<$inner> for $wrapper {
            fn mul_assign(&mut self, rhs: $inner) {
                *self = self.mul(rhs);
            }
        }

        impl MulAssign<&$inner> for $wrapper {
            fn mul_assign(&mut self, rhs: &$inner) {
                *self = self.mul(rhs);
            }
        }

        impl MulAssign<$wrapper> for $inner {
            fn mul_assign(&mut self, rhs: $wrapper) {
                *self = self.mul(rhs.0);
            }
        }

        impl MulAssign<&$wrapper> for $inner {
            fn mul_assign(&mut self, rhs: &$wrapper) {
                *self = self.mul(rhs.0);
            }
        }
    };
}
pub(crate) use impl_mul_to_wrapper;

macro_rules! impl_partial_eq_to_wrapper {
    ( $wrapper:ty, $inner:ty ) => {
        impl PartialEq<$inner> for $wrapper {
            fn eq(&self, other: &$inner) -> bool {
                self.0.eq(other)
            }
        }

        impl PartialEq<&$inner> for $wrapper {
            fn eq(&self, other: &&$inner) -> bool {
                self.0.eq(*other)
            }
        }

        impl PartialEq<&$wrapper> for $wrapper {
            fn eq(&self, other: &&$wrapper) -> bool {
                other.0.eq(self)
            }
        }

        impl PartialEq<$inner> for &$wrapper {
            fn eq(&self, other: &$inner) -> bool {
                self.0.eq(other)
            }
        }

        impl PartialEq<$wrapper> for &$wrapper {
            fn eq(&self, other: &$wrapper) -> bool {
                other.0.eq(self)
            }
        }

        impl PartialEq<$wrapper> for $inner {
            fn eq(&self, other: &$wrapper) -> bool {
                other.0.eq(self)
            }
        }

        impl PartialEq<&$wrapper> for $inner {
            fn eq(&self, other: &&$wrapper) -> bool {
                other.0.eq(self)
            }
        }

        impl PartialEq<$wrapper> for &$inner {
            fn eq(&self, other: &$wrapper) -> bool {
                other.0.eq(*self)
            }
        }
    };
}
pub(crate) use impl_partial_eq_to_wrapper;

#[cfg(test)]
pub(crate) mod tests {
    macro_rules! test_op_assign {
        ( $right:expr, +=, $left:expr => $expected:expr ) => {{
            let mut number = $right;
            number += $left;
            assert_eq!($expected, number);
        }};
        ( $right:expr, -=, $left:expr => $expected:expr ) => {{
            let mut number = $right;
            number -= $left;
            assert_eq!($expected, number);
        }};
        ( $right:expr, *=, $left:expr => $expected:expr ) => {{
            let mut number = $right;
            number *= $left;
            assert_eq!($expected, number);
        }};
    }
    pub(crate) use test_op_assign;
}
