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
