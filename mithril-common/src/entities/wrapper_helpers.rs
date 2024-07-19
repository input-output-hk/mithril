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

macro_rules! impl_div_to_wrapper {
    ( $wrapper:ident, $inner:ty ) => {
        use std::ops::{Div, DivAssign};

        impl Div for $wrapper {
            type Output = Self;

            fn div(self, rhs: Self) -> Self::Output {
                self / *rhs
            }
        }

        impl Div<$inner> for $wrapper {
            type Output = Self;

            fn div(self, rhs: $inner) -> Self::Output {
                $wrapper(*self / rhs)
            }
        }

        impl Div<&$inner> for $wrapper {
            type Output = Self;

            fn div(self, rhs: &$inner) -> Self::Output {
                self.div(*rhs)
            }
        }

        impl Div<$wrapper> for $inner {
            type Output = $wrapper;

            fn div(self, rhs: $wrapper) -> Self::Output {
                $wrapper(self.div(rhs.0))
            }
        }

        impl Div<&$wrapper> for $inner {
            type Output = $wrapper;

            fn div(self, rhs: &$wrapper) -> Self::Output {
                self.div(*rhs)
            }
        }

        impl Div<$wrapper> for &$inner {
            type Output = $wrapper;

            fn div(self, rhs: $wrapper) -> Self::Output {
                (*self).div(rhs)
            }
        }

        impl Div<&$wrapper> for &$inner {
            type Output = $wrapper;

            fn div(self, rhs: &$wrapper) -> Self::Output {
                (*self).div(*rhs)
            }
        }

        impl DivAssign for $wrapper {
            fn div_assign(&mut self, rhs: Self) {
                *self = self.div(rhs);
            }
        }

        impl DivAssign<$inner> for $wrapper {
            fn div_assign(&mut self, rhs: $inner) {
                *self = self.div(rhs);
            }
        }

        impl DivAssign<&$inner> for $wrapper {
            fn div_assign(&mut self, rhs: &$inner) {
                *self = self.div(rhs);
            }
        }

        impl DivAssign<$wrapper> for $inner {
            fn div_assign(&mut self, rhs: $wrapper) {
                *self = self.div(rhs.0);
            }
        }

        impl DivAssign<&$wrapper> for $inner {
            fn div_assign(&mut self, rhs: &$wrapper) {
                *self = self.div(rhs.0);
            }
        }
    };
}
pub(crate) use impl_div_to_wrapper;

macro_rules! impl_rem_to_wrapper {
    ( $wrapper:ident, $inner:ty ) => {
        use std::ops::{Rem, RemAssign};

        impl Rem for $wrapper {
            type Output = Self;

            fn rem(self, rhs: Self) -> Self::Output {
                self % *rhs
            }
        }

        impl Rem<$inner> for $wrapper {
            type Output = Self;

            fn rem(self, rhs: $inner) -> Self::Output {
                $wrapper(*self % rhs)
            }
        }

        impl Rem<&$inner> for $wrapper {
            type Output = Self;

            fn rem(self, rhs: &$inner) -> Self::Output {
                self.rem(*rhs)
            }
        }

        impl Rem<$wrapper> for $inner {
            type Output = $wrapper;

            fn rem(self, rhs: $wrapper) -> Self::Output {
                $wrapper(self.rem(rhs.0))
            }
        }

        impl Rem<&$wrapper> for $inner {
            type Output = $wrapper;

            fn rem(self, rhs: &$wrapper) -> Self::Output {
                self.rem(*rhs)
            }
        }

        impl Rem<$wrapper> for &$inner {
            type Output = $wrapper;

            fn rem(self, rhs: $wrapper) -> Self::Output {
                (*self).rem(rhs)
            }
        }

        impl Rem<&$wrapper> for &$inner {
            type Output = $wrapper;

            fn rem(self, rhs: &$wrapper) -> Self::Output {
                (*self).rem(*rhs)
            }
        }

        impl RemAssign for $wrapper {
            fn rem_assign(&mut self, rhs: Self) {
                *self = self.rem(rhs);
            }
        }

        impl RemAssign<$inner> for $wrapper {
            fn rem_assign(&mut self, rhs: $inner) {
                *self = self.rem(rhs);
            }
        }

        impl RemAssign<&$inner> for $wrapper {
            fn rem_assign(&mut self, rhs: &$inner) {
                *self = self.rem(rhs);
            }
        }

        impl RemAssign<$wrapper> for $inner {
            fn rem_assign(&mut self, rhs: $wrapper) {
                *self = self.rem(rhs.0);
            }
        }

        impl RemAssign<&$wrapper> for $inner {
            fn rem_assign(&mut self, rhs: &$wrapper) {
                *self = self.rem(rhs.0);
            }
        }
    };
}
pub(crate) use impl_rem_to_wrapper;

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
        ( $right:expr, /=, $left:expr => $expected:expr ) => {{
            let mut number = $right;
            number /= $left;
            assert_eq!($expected, number);
        }};
        ( $right:expr, %=, $left:expr => $expected:expr ) => {{
            let mut number = $right;
            number %= $left;
            assert_eq!($expected, number);
        }};
    }
    pub(crate) use test_op_assign;
}
