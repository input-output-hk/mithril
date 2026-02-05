use crate::Stake;

cfg_num_integer! {
    use num_bigint::{BigInt, Sign};
    use num_rational::Ratio;
    use num_traits::{One, Signed};
    use std::ops::Neg;


    /// Checks that ev is successful in the lottery. In particular, it compares the output of `phi`
    /// (a real) to the output of `ev` (a hash).  It uses the same technique used in the
    /// [Cardano ledger](https://github.com/input-output-hk/cardano-ledger/). In particular,
    /// `ev` is a natural in `[0,2^512]`, while `phi` is a floating point in `[0, 1]`, and so what
    /// this check does is verify whether `p < 1 - (1 - phi_f)^w`, with `p = ev / 2^512`.
    ///
    /// The calculation is done using the following optimization:
    ///
    /// let `q = 1 / (1 - p)` and `c = ln(1 - phi_f)`
    ///
    /// then          `p < 1 - (1 - phi_f)^w`
    /// `<=> 1 / (1 - p) < exp(-w * c)`
    /// `<=> q           < exp(-w * c)`
    ///
    /// This can be computed using the taylor expansion. Using error estimation, we can do
    /// an early stop, once we know that the result is either above or below.  We iterate 1000
    /// times. If no conclusive result has been reached, we return false.
    ///
    /// Note that         1             1               evMax
    ///             q = ----- = ------------------ = -------------
    ///                 1 - p    1 - (ev / evMax)    (evMax - ev)
    ///
    /// Used to determine winning lottery tickets.
    pub(crate) fn is_lottery_won(phi_f: f64, ev: [u8; 64], stake: Stake, total_stake: Stake) -> bool {
        // If phi_f = 1, then we automatically break with true
        if (phi_f - 1.0).abs() < f64::EPSILON {
            return true;
        }

        let ev_max = BigInt::from(2u8).pow(512);
        let ev = BigInt::from_bytes_le(Sign::Plus, &ev);
        let q = Ratio::new_raw(ev_max.clone(), ev_max - ev);

        let c =
            Ratio::from_float((1.0 - phi_f).ln()).expect("Only fails if the float is infinite or NaN.");
        let w = Ratio::new_raw(BigInt::from(stake), BigInt::from(total_stake));
        let x = (w * c).neg();

        // Now we compute a taylor function that breaks when the result is known.
        taylor_comparison(1000, q, x)
    }

    /// Checks if cmp < exp(x). Uses error approximation for an early stop. Whenever the value being
    /// compared, `cmp`, is smaller (or greater) than the current approximation minus an `error_term`
    /// (plus an `error_term` respectively), then we stop approximating. The choice of the `error_term`
    /// is specific to our use case, and this function should not be used in other contexts without
    /// reconsidering the `error_term`. As a conservative value of the `error_term` we choose
    /// `new_x * M`, where `new_x` is the next term of the taylor expansion, and `M` is the largest
    /// value of `x` in a reasonable range. Note that `x >= 0`, given that `x = - w * c`, with
    /// `0 <= w <= 1` and `c < 0`, as `c` is defined as `c = ln(1.0 - phi_f)` with `phi_f \in (0,1)`.
    /// Therefore, a good integral bound is the maximum value that `|ln(1.0 - phi_f)|` can take with
    /// `phi_f \in [0, 0.95]` (if we expect to have `phi_f > 0.95` this bound should be extended),
    /// which is `3`. Hence, we set `M = 3`.
    #[allow(clippy::redundant_clone)]
    fn taylor_comparison(bound: usize, cmp: Ratio<BigInt>, x: Ratio<BigInt>) -> bool {
        let mut new_x = x.clone();
        let mut phi: Ratio<BigInt> = One::one();
        let mut divisor: BigInt = One::one();
        for _ in 0..bound {
            phi += new_x.clone();

            divisor += 1;
            new_x = (new_x.clone() * x.clone()) / divisor.clone();
            let error_term = new_x.clone().abs() * BigInt::from(3); // new_x * M

            if cmp > (phi.clone() + error_term.clone()) {
                return false;
            } else if cmp < phi.clone() - error_term.clone() {
                return true;
            }
        }
        false
    }
}

cfg_num_integer! {
    // use num_bigint::BigInt;
    // use num_rational::Ratio;

    // Description of the math behind the computation we want to do
    //
    // poseidon_hash / p < 1 - (1 - phi_f)^w
    // where p is the modulus of the field, phi_f a parameter constant comprised in (0,1) and w a variable in (0,1)
    // poseidon_hash < p * (1 - (1 - phi_f)^w)
    // poseidon_hash < p * (1 - exp( w * ln(1 - phi_f) ) )
    // let C = ln(1 - phi_f)
    // poseidon_hash < p * (1 - exp( w * C ) )
    // We want to use the taylor series to approximate exp( w * C )
    // exp(C*x) = 1 + C*x + (C*x)^2/2! + (C*x)^3/3! + ... + (C*x)^{N-1}/(N-1}! + O(x^(N))
    // We want to stop when the next term is less than our precision target, that is epsilon = 2^{-128}
    // Hence we stop when |(C*x)^N / N!| < epsilon
    // We can check instead (C * x)^N < epsilon
    // which gives us the bound N < log(epsilon) / log(|C*x|)

    #[allow(dead_code)]
    pub fn compute_exp(x: Ratio<BigInt>, c: Ratio<BigInt>, iterations: usize) -> Ratio<BigInt> {
        let mut acc = Ratio::new_raw(BigInt::from(1),BigInt::from(1));
        let mut numerator = BigInt::from(1);
        let mut denominator = BigInt::from(1);
        let x_time_c = x * c;
        for i in 1..iterations {
            numerator *= x_time_c.numer();
            denominator *= i * x_time_c.denom();
            acc += Ratio::new_raw(numerator.clone(), denominator.clone());
        }
        acc
    }


    // TODO: remove this allow dead_code directive when function is called or future_snark is activated
    #[allow(dead_code)]
    pub fn compute_target_bytes(phi_f: f64, stake: Stake, total_stake: Stake) -> BigInt {
        use num_integer::Integer;
        use num_traits::{Zero, Num};

        let modulus = BigInt::from_str_radix(
            "73eda753299d7d483339d80809a1d80553bda402fffe5bfeffffffff00000001",
            // "1",
            16,
        )
        .unwrap();

        let w = Ratio::new_raw(BigInt::from(stake), BigInt::from(total_stake));
        let c =
            Ratio::from_float((-phi_f).ln_1p()).expect("Only fails if the float is infinite or NaN.");

        // With Taylor series 2
        let exp_wc = compute_exp(c.clone(), w.clone(), 50);
        let t_taylor = Ratio::from(modulus.clone()) - Ratio::from(modulus.clone()) * exp_wc.clone();

        // Floor division
        let (t_int, remainder) = t_taylor.numer().div_rem(t_taylor.denom());
        assert!(t_int >= BigInt::zero());

        // If exact division and t_int > 0, subtract 1
        // let target =
        if remainder.is_zero() && !t_int.is_zero() {
            t_int - 1
        } else {
            t_int
        }

        // let (_, bytes) = target.to_bytes_le();
        // bytes
        // target
    }
}

cfg_rug! {
    use rug::{Float, integer::Order, ops::Pow};
    /// The crate `rug` has sufficient optimizations to not require a taylor approximation with early
    /// stop. The difference between the current implementation and the one using the optimization
    /// above is around 10% faster. We perform the computations with 117 significant bits of
    /// precision, since this is enough to represent the fraction of a single lovelace. We have that
    /// 1e6 lovelace equals 1 ada, and there is 45 billion ada in circulation. Meaning there are
    /// 4.5e16 lovelace, so 1e-17 is sufficient to represent fractions of the stake distribution. In
    /// order to keep the error in the 1e-17 range, we need to carry out the computations with 34
    /// decimal digits (in order to represent the 4.5e16 ada without any rounding errors, we need
    /// double that precision).
    pub(crate) fn is_lottery_won(phi_f: f64, ev: [u8; 64], stake: Stake, total_stake: Stake) -> bool {
        // If phi_f = 1, then we automatically break with true
        if (phi_f - 1.0).abs() < f64::EPSILON {
            return true;
        }
        let ev = rug::Integer::from_digits(&ev, Order::LsfLe);
        let ev_max: Float = Float::with_val(117, 2).pow(512);
        let q = ev / ev_max;

        let w = Float::with_val(117, stake) / Float::with_val(117, total_stake);
        let phi = Float::with_val(117, 1.0) - Float::with_val(117, 1.0 - phi_f).pow(w);

        q < phi
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use num_bigint::{BigInt, Sign};
    use num_rational::Ratio;
    use proptest::prelude::*;
    // Implementation of `is_lottery_won` without approximation. We only get the precision of f64 here.
    fn trivial_is_lottery_won(phi_f: f64, ev: [u8; 64], stake: Stake, total_stake: Stake) -> bool {
        let ev_max = BigInt::from(2u8).pow(512);
        let ev = BigInt::from_bytes_le(Sign::Plus, &ev);
        let q = Ratio::new_raw(ev, ev_max);

        let w = stake as f64 / total_stake as f64;
        let phi = Ratio::from_float(1.0 - (1.0 - phi_f).powf(w)).unwrap();
        q < phi
    }

    proptest! {
        #![proptest_config(ProptestConfig::with_cases(50))]

        #[test]
        /// Checking the `is_lottery_won` function.
        fn is_lottery_won_check_precision_against_trivial_implementation(
            phi_f in 0.01..0.5f64,
            ev_1 in any::<[u8; 32]>(),
            ev_2 in any::<[u8; 32]>(),
            total_stake in 100_000_000..1_000_000_000u64,
            stake in 1_000_000..50_000_000u64
        ) {
            let mut ev = [0u8; 64];
            ev.copy_from_slice(&[&ev_1[..], &ev_2[..]].concat());

            let quick_result = trivial_is_lottery_won(phi_f, ev, stake, total_stake);
            let result = is_lottery_won(phi_f, ev, stake, total_stake);
            assert_eq!(quick_result, result);
        }

        #[cfg(any(feature = "num-integer-backend", target_family = "wasm", windows))]
        #[test]
        /// Checking the early break of Taylor computation
        fn taylor_comparison_breaks_early(
            x in -0.9..0.9f64,
        ) {
            let exponential = num_traits::float::Float::exp(x);
            let cmp_n = Ratio::from_float(exponential - 2e-10_f64).unwrap();
            let cmp_p = Ratio::from_float(exponential + 2e-10_f64).unwrap();
            assert!(taylor_comparison(1000, cmp_n, Ratio::from_float(x).unwrap()));
            assert!(!taylor_comparison(1000, cmp_p, Ratio::from_float(x).unwrap()));
        }
    }

    #[cfg(any(feature = "num-integer-backend", target_family = "wasm", windows))]
    #[cfg(test)]
    mod tests_eligibility_for_snark {
        use super::*;

        #[cfg(any(feature = "num-integer-backend", target_family = "wasm", windows))]
        #[test]
        fn test_zero_stake_bytes_stable() {
            let phi_f = 0.05;
            let total_stake = 45_000_000_000;

            for _ in 0..100 {
                let target = compute_target_bytes(phi_f, 0, total_stake);
                assert!(target == BigInt::ZERO);
            }
        }

        #[cfg(any(feature = "num-integer-backend", target_family = "wasm", windows))]
        #[test]
        fn test_one_stake_bytes_stable() {
            let phi_f = 0.05;
            let total_stake = 45_000_000_000;
            let first_target = compute_target_bytes(phi_f, 1, total_stake);
            for _ in 0..100 {
                let target = compute_target_bytes(phi_f, 1, total_stake);
                assert!(target == first_target);
            }
        }

        #[cfg(any(feature = "num-integer-backend", target_family = "wasm", windows))]
        #[test]
        fn test_following_stake_same_order() {
            let phi_f = 0.05;
            let total_stake = 45_000_000_000;

            let mut prev_target = compute_target_bytes(phi_f, 99_999, total_stake);
            for stake in 100_000..100_100 {
                let target = compute_target_bytes(phi_f, stake, total_stake);
                assert!(prev_target < target);
                prev_target = target;
            }
        }

        #[cfg(any(feature = "num-integer-backend", target_family = "wasm", windows))]
        #[test]
        fn test_minimal_stake_difference_stable() {
            let phi_f = 0.05;
            let total_stake = 45_000_000_000;
            for _ in 0..100 {
                let zero_target = compute_target_bytes(phi_f, 0, total_stake);
                let first_target = compute_target_bytes(phi_f, 1, total_stake);
                assert!(zero_target < first_target);
                let second_target = compute_target_bytes(phi_f, 2, total_stake);
                assert!(first_target < second_target);
            }
        }

        #[cfg(any(feature = "num-integer-backend", target_family = "wasm", windows))]
        #[test]
        fn test_full_stake() {
            let phi_f = 0.05;
            let total_stake = 45_000_000_000;

            let full_target = compute_target_bytes(phi_f, total_stake, total_stake);
            for _ in 0..100 {
                let target = compute_target_bytes(phi_f, total_stake, total_stake);
                assert!(full_target == target);
            }
        }

        #[cfg(any(feature = "num-integer-backend", target_family = "wasm", windows))]
        #[test]
        fn test_following_max_stake_same_order() {
            let phi_f = 0.05;
            let total_stake = 45_000_000_000;

            let mut prev_target = compute_target_bytes(phi_f, total_stake, total_stake);
            for i in 1..=100 {
                let target = compute_target_bytes(phi_f, total_stake - i, total_stake);
                assert!(prev_target > target);
                prev_target = target;
            }
        }

        proptest! {
            #![proptest_config(ProptestConfig::with_cases(50))]

            #[test]
            #[cfg(any(feature = "num-integer-backend", target_family = "wasm", windows))]
            fn following_stake_same_order(
                phi_f in 0.01..0.5f64,
                total_stake in 100_000_000..1_000_000_000u64,
                stake in 10_000_000..50_000_000u64,
            ) {
                let base_target = compute_target_bytes(phi_f, stake, total_stake);
                let next_target = compute_target_bytes(phi_f, stake + 1, total_stake);

                assert!(base_target < next_target);
            }

            #[test]
            #[cfg(any(feature = "num-integer-backend", target_family = "wasm", windows))]
            fn following_small_stake_same_order(
                phi_f in 0.01..0.5f64,
                total_stake in 100_000_000..1_000_000_000u64,
                stake in 100_000..500_000u64,
            ) {
                let base_target = compute_target_bytes(phi_f, stake, total_stake);
                let next_target = compute_target_bytes(phi_f, stake + 1, total_stake);

                assert!(base_target < next_target);
            }

            #[test]
            #[cfg(any(feature = "num-integer-backend", target_family = "wasm", windows))]
            fn same_stake_same_result(
                phi_f in 0.01..0.5f64,
                total_stake in 100_000_000..1_000_000_000u64,
                stake in 10_000_000..50_000_000u64,
            ) {
                let target = compute_target_bytes(phi_f, stake, total_stake);
                let same_target = compute_target_bytes(phi_f, stake, total_stake);

                assert!(target == same_target);
            }

            #[test]
            #[cfg(any(feature = "num-integer-backend", target_family = "wasm", windows))]
            fn same_small_stake_same_result(
                phi_f in 0.01..0.5f64,
                total_stake in 100_000_000..1_000_000_000u64,
                stake in 100_000..500_000u64,
            ) {
                let target = compute_target_bytes(phi_f, stake, total_stake);
                let same_target = compute_target_bytes(phi_f, stake, total_stake);

                assert!(target == same_target);
            }

        }

        mod golden {

            use super::*;

            const GOLDEN_BYTES_ZERO: [u8; 32] = [
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                0, 0, 0, 0,
            ];

            const GOLDEN_BYTES_ONE: [u8; 32] = [
                27, 48, 173, 178, 223, 192, 192, 121, 170, 65, 40, 134, 214, 2, 53, 145, 123, 46,
                191, 195, 147, 215, 156, 110, 100, 74, 145, 0, 0, 0, 0, 0,
            ];

            const GOLDEN_BYTES_TWO: [u8; 32] = [
                137, 195, 57, 199, 124, 159, 238, 181, 189, 47, 128, 105, 4, 191, 232, 76, 181,
                167, 38, 144, 16, 249, 56, 221, 200, 148, 34, 1, 0, 0, 0, 0,
            ];

            const GOLDEN_BYTES_MAX_STAKE: [u8; 32] = [
                9, 38, 93, 151, 8, 144, 186, 241, 181, 159, 51, 71, 83, 107, 160, 234, 47, 37, 82,
                100, 201, 147, 177, 246, 91, 70, 174, 91, 247, 225, 203, 5,
            ];

            const GOLDEN_BYTES_MAX_STAKE_MINUS_ONE: [u8; 32] = [
                27, 109, 86, 146, 19, 122, 192, 25, 95, 249, 227, 17, 232, 123, 152, 70, 56, 53,
                181, 234, 0, 26, 207, 192, 175, 63, 36, 91, 247, 225, 203, 5,
            ];

            const GOLDEN_BYTES_MAX_STAKE_MINUS_TWO: [u8; 32] = [
                243, 79, 224, 18, 113, 92, 32, 249, 151, 225, 83, 134, 17, 78, 22, 178, 195, 189,
                81, 57, 60, 243, 235, 138, 3, 57, 154, 90, 247, 225, 203, 5,
            ];

            const GOLDEN_BYTES_FOLLOWING_STAKE_MEDIUM: [[u8; 32]; 10] = [
                [
                    36, 127, 2, 13, 109, 64, 79, 19, 29, 60, 122, 98, 220, 146, 193, 19, 45, 207,
                    242, 141, 26, 186, 100, 167, 77, 171, 34, 178, 221, 0, 0, 0,
                ],
                [
                    216, 37, 165, 224, 218, 68, 125, 139, 171, 137, 191, 201, 145, 196, 233, 121,
                    194, 128, 186, 199, 141, 199, 40, 0, 177, 245, 179, 178, 221, 0, 0, 0,
                ],
                [
                    164, 219, 217, 137, 150, 70, 220, 167, 118, 136, 189, 251, 100, 105, 39, 113,
                    85, 181, 134, 11, 234, 30, 236, 88, 20, 64, 69, 179, 221, 0, 0, 0,
                ],
                [
                    217, 111, 239, 70, 183, 5, 21, 141, 137, 171, 93, 169, 89, 162, 12, 47, 202,
                    109, 87, 89, 47, 192, 174, 177, 119, 138, 214, 179, 221, 0, 0, 0,
                ],
                [
                    18, 212, 231, 177, 208, 175, 197, 20, 159, 216, 134, 101, 114, 144, 43, 233, 4,
                    171, 44, 177, 93, 171, 112, 10, 219, 212, 103, 180, 221, 0, 0, 0,
                ],
                [
                    69, 225, 190, 141, 64, 83, 232, 206, 33, 104, 29, 165, 176, 84, 22, 213, 233,
                    109, 6, 19, 117, 224, 49, 99, 62, 31, 249, 180, 221, 0, 0, 0,
                ],
                [
                    157, 91, 175, 147, 124, 82, 57, 3, 44, 37, 3, 191, 20, 16, 95, 40, 93, 183,
                    228, 126, 117, 95, 242, 187, 161, 105, 138, 181, 221, 0, 0, 0,
                ],
                [
                    21, 53, 118, 64, 96, 215, 157, 178, 135, 77, 23, 236, 157, 227, 151, 24, 67,
                    136, 199, 244, 94, 40, 178, 20, 5, 180, 27, 182, 221, 0, 0, 0,
                ],
                [
                    212, 14, 148, 161, 122, 70, 138, 152, 174, 145, 54, 71, 74, 240, 82, 219, 127,
                    225, 174, 116, 49, 59, 113, 109, 104, 254, 172, 182, 221, 0, 0, 0,
                ],
                [
                    79, 249, 140, 34, 92, 178, 104, 44, 202, 20, 59, 205, 22, 87, 34, 166, 247,
                    195, 154, 254, 236, 151, 47, 198, 203, 72, 62, 183, 221, 0, 0, 0,
                ],
            ];

            const GOLDEN_BYTES_FOLLOWING_STAKE_MAX: [[u8; 32]; 10] = [
                [
                    9, 38, 93, 151, 8, 144, 186, 241, 181, 159, 51, 71, 83, 107, 160, 234, 47, 37,
                    82, 100, 201, 147, 177, 246, 91, 70, 174, 91, 247, 225, 203, 5,
                ],
                [
                    27, 109, 86, 146, 19, 122, 192, 25, 95, 249, 227, 17, 232, 123, 152, 70, 56,
                    53, 181, 234, 0, 26, 207, 192, 175, 63, 36, 91, 247, 225, 203, 5,
                ],
                [
                    243, 79, 224, 18, 113, 92, 32, 249, 151, 225, 83, 134, 17, 78, 22, 178, 195,
                    189, 81, 57, 60, 243, 235, 138, 3, 57, 154, 90, 247, 225, 203, 5,
                ],
                [
                    15, 209, 230, 16, 54, 181, 10, 97, 122, 4, 35, 111, 112, 61, 154, 96, 249, 189,
                    39, 80, 123, 31, 8, 85, 87, 50, 16, 90, 247, 225, 203, 5,
                ],
                [
                    47, 124, 171, 230, 126, 141, 144, 110, 63, 108, 59, 135, 164, 165, 164, 133, 0,
                    53, 55, 47, 190, 158, 35, 31, 171, 43, 134, 89, 247, 225, 203, 5,
                ],
                [
                    238, 55, 132, 66, 73, 242, 78, 137, 63, 129, 209, 121, 76, 226, 181, 84, 0, 34,
                    128, 214, 4, 113, 62, 233, 254, 36, 252, 88, 247, 225, 203, 5,
                ],
                [
                    140, 108, 152, 24, 78, 109, 26, 98, 242, 9, 100, 226, 5, 79, 78, 1, 32, 132, 2,
                    70, 79, 150, 88, 179, 82, 30, 114, 88, 247, 225, 203, 5,
                ],
                [
                    248, 127, 156, 148, 219, 125, 170, 241, 238, 42, 188, 76, 109, 71, 238, 190,
                    134, 90, 190, 125, 157, 14, 114, 125, 166, 23, 232, 87, 247, 225, 203, 5,
                ],
                [
                    17, 167, 139, 11, 175, 17, 69, 119, 235, 102, 237, 52, 30, 39, 22, 193, 91,
                    164, 179, 125, 239, 217, 138, 71, 250, 16, 94, 87, 247, 225, 203, 5,
                ],
                [
                    29, 12, 96, 237, 206, 253, 105, 119, 189, 158, 85, 7, 179, 73, 70, 59, 198, 96,
                    226, 69, 69, 248, 162, 17, 78, 10, 212, 86, 247, 225, 203, 5,
                ],
            ];

            #[cfg(any(feature = "num-integer-backend", target_family = "wasm", windows))]
            fn golden_value_zero_stake() -> BigInt {
                let phi_f = 0.05;
                let stake = 0;
                let total_stake = 45_000_000_000;

                // let target = compute_target_bytes(phi_f, stake, total_stake);
                // let (_, mut bytes) = target.to_bytes_le();
                // bytes.resize(32, 0);
                // println!("{:?}", bytes);
                // target
                compute_target_bytes(phi_f, stake, total_stake)
            }

            #[cfg(any(feature = "num-integer-backend", target_family = "wasm", windows))]
            fn golden_value_one_stake() -> BigInt {
                let phi_f = 0.05;
                let stake = 1;
                let total_stake = 45_000_000_000;

                compute_target_bytes(phi_f, stake, total_stake)
            }

            #[cfg(any(feature = "num-integer-backend", target_family = "wasm", windows))]
            fn golden_value_two_stake() -> BigInt {
                let phi_f = 0.05;
                let stake = 2;
                let total_stake = 45_000_000_000;

                compute_target_bytes(phi_f, stake, total_stake)
            }

            #[cfg(any(feature = "num-integer-backend", target_family = "wasm", windows))]
            fn golden_value_max_stake() -> BigInt {
                let phi_f = 0.05;
                let total_stake = 45_000_000_000;

                compute_target_bytes(phi_f, total_stake, total_stake)
            }

            #[cfg(any(feature = "num-integer-backend", target_family = "wasm", windows))]
            fn golden_value_max_stake_minus_one() -> BigInt {
                let phi_f = 0.05;
                let total_stake = 45_000_000_000;

                compute_target_bytes(phi_f, total_stake - 1, total_stake)
            }

            #[cfg(any(feature = "num-integer-backend", target_family = "wasm", windows))]
            fn golden_value_max_stake_minus_two() -> BigInt {
                let phi_f = 0.05;
                let total_stake = 45_000_000_000;

                compute_target_bytes(phi_f, total_stake - 2, total_stake)
            }

            #[cfg(any(feature = "num-integer-backend", target_family = "wasm", windows))]
            fn golden_value_following_stake_medium() -> Vec<BigInt> {
                let phi_f = 0.05;
                let total_stake = 45_000_000_000;
                let mut golden_values = vec![];

                for stake in 100_000..100_010 {
                    let target = compute_target_bytes(phi_f, stake, total_stake);
                    golden_values.push(target);
                }
                golden_values
            }

            #[cfg(any(feature = "num-integer-backend", target_family = "wasm", windows))]
            fn golden_value_following_stake_max() -> Vec<BigInt> {
                let phi_f = 0.05;
                let total_stake = 45_000_000_000;
                let mut golden_values = vec![];

                for i in 0..10 {
                    let target = compute_target_bytes(phi_f, total_stake - i, total_stake);
                    golden_values.push(target);
                }
                golden_values
            }

            #[cfg(any(feature = "num-integer-backend", target_family = "wasm", windows))]
            #[test]
            fn golden_check_small_values() {
                let golden_target_0 = BigInt::from_bytes_le(Sign::Plus, &GOLDEN_BYTES_ZERO);
                let golden_target_1 = BigInt::from_bytes_le(Sign::Plus, &GOLDEN_BYTES_ONE);
                let golden_target_2 = BigInt::from_bytes_le(Sign::Plus, &GOLDEN_BYTES_TWO);

                assert_eq!(golden_target_0, golden_value_zero_stake());
                assert_eq!(golden_target_1, golden_value_one_stake());
                assert_eq!(golden_target_2, golden_value_two_stake());
            }

            #[cfg(any(feature = "num-integer-backend", target_family = "wasm", windows))]
            #[test]
            fn golden_check_max_values() {
                let golden_target_max = BigInt::from_bytes_le(Sign::Plus, &GOLDEN_BYTES_MAX_STAKE);
                let golden_target_max_1 =
                    BigInt::from_bytes_le(Sign::Plus, &GOLDEN_BYTES_MAX_STAKE_MINUS_ONE);
                let golden_target_max_2 =
                    BigInt::from_bytes_le(Sign::Plus, &GOLDEN_BYTES_MAX_STAKE_MINUS_TWO);

                assert_eq!(golden_target_max, golden_value_max_stake());
                assert_eq!(golden_target_max_1, golden_value_max_stake_minus_one());
                assert_eq!(golden_target_max_2, golden_value_max_stake_minus_two());
            }

            #[cfg(any(feature = "num-integer-backend", target_family = "wasm", windows))]
            #[test]
            fn golden_check_following_stake_medium() {
                let golden_target_vector = golden_value_following_stake_medium();

                for (t1, t2_bytes) in golden_target_vector
                    .iter()
                    .zip(GOLDEN_BYTES_FOLLOWING_STAKE_MEDIUM.iter())
                {
                    let t2 = BigInt::from_bytes_le(Sign::Plus, t2_bytes);
                    assert_eq!(t1, &t2);
                }
            }

            #[cfg(any(feature = "num-integer-backend", target_family = "wasm", windows))]
            #[test]
            fn golden_check_following_stake_max() {
                let golden_target_vector = golden_value_following_stake_max();

                for (t1, t2_bytes) in golden_target_vector
                    .iter()
                    .zip(GOLDEN_BYTES_FOLLOWING_STAKE_MAX.iter())
                {
                    let t2 = BigInt::from_bytes_le(Sign::Plus, t2_bytes);
                    assert_eq!(t1, &t2);
                }
            }
        }
    }
}
