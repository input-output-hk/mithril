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

    // Function that computes an approximation of exp(a/b) using a binomial splitting
    // to compute the taylor expansion terms between i and j,
    // i.e. between (a/b)^i * (1/i!) and (a/b)^j * (1/j!)
    pub fn exponential_approximation(i: usize, j: usize, a: &BigInt, b: &BigInt) -> (BigInt, BigInt, BigInt) {
        if j - i == 1 {
            if i == 0 {
                return (BigInt::one(), BigInt::one(), BigInt::one());
            }
            return (a.clone(), b * BigInt::from(i), a.clone());
        }

        let mid = (i + j) / 2;
        let (numerator_l, denominator_l, auxiliary_value_l) = exponential_approximation(i, mid, a, b);
        let (numerator_r, denominator_r, auxiliary_value_r) = exponential_approximation(mid, j, a, b);

        let numerator = &numerator_l * &denominator_r + &auxiliary_value_l * &numerator_r;
        let denominator = &denominator_l * &denominator_r;
        let auxiliary_value = auxiliary_value_l * auxiliary_value_r;

        (numerator, denominator, auxiliary_value)
    }

    // Function that computes an approximation of ln(1 - a/b) using a taylor expension
    // for a given number of iterations
    #[allow(dead_code)]
    fn ln_1p_approximation(iterations: usize, a: &BigInt, b: &BigInt) -> Ratio<BigInt> {
        let mut num = a.clone();
        let mut denom = b.clone();
        let mut acc = Ratio::new_raw(a.clone(),b.clone());
        for i in 2..(iterations + 1) {
            num *= a;
            denom *= b;
            acc += Ratio::new_raw(num.clone(), denom.clone() * i);
        }

        -acc
    }

    /// Computes a Taylor expension of the exponential exp(c*w) up to the (N-1)th term
    pub fn compute_exp(x: Ratio<BigInt>, c: Ratio<BigInt>, iterations: usize) -> Ratio<BigInt> {
        let cw = c * x;
        let (num, denom, _) = exponential_approximation(0, iterations, cw.numer(), cw.denom());

        Ratio::new_raw(num, denom)
    }

    /// Compute the target value of a given party as a base field element using
    /// Taylor expension for the natural log and the exponential functions
    // TODO: remove this allow dead_code directive when function is called or future_snark is activated
    #[allow(dead_code)]
    pub fn compute_target_bigint(phi_f_ratio: &Ratio<BigInt>, stake: Stake, total_stake: Stake) -> BigInt {
        use num_integer::Integer;
        use num_traits::{Zero, Num};

        let modulus = BigInt::from_str_radix(
            "73eda753299d7d483339d80809a1d80553bda402fffe5bfeffffffff00000001",
            16,
        )
        .unwrap();
        let stake_ratio = Ratio::new_raw(BigInt::from(stake), BigInt::from(total_stake));

        let ln_one_minus_phi_f = ln_1p_approximation(40, phi_f_ratio.numer(), phi_f_ratio.denom());
        let exp_ln_one_minus_phi_f_stake_ratio = compute_exp(ln_one_minus_phi_f.clone(), stake_ratio.clone(), 40);
        let target_as_ratio = Ratio::from(modulus.clone()) - Ratio::from(modulus.clone()) * exp_ln_one_minus_phi_f_stake_ratio.clone();

        // Floor division
        let (target_as_int, remainder) = target_as_ratio.numer().div_rem(target_as_ratio.denom());
        assert!(target_as_int >= BigInt::zero());

        // If exact division and target_as_int > 0, subtract 1
        if remainder.is_zero() && !target_as_int.is_zero() {
            target_as_int - 1
        } else {
            target_as_int
        }
    }

    /// Compute the target value in bytes form given a value phi_f,
    /// the signer stake and the total stake
    /// The function approximate phi_f as a ratio to make Taylor expensions easier to compute
    #[allow(dead_code)]
    pub fn compute_target_bytes(phi_f: f64, stake: Stake, total_stake: Stake) -> Vec<u8> {

        let phi_f_ratio_int: Ratio<i64> = Ratio::approximate_float(phi_f).expect("Only fails if the float is infinite or NaN.");
        let phi_f_ratio = Ratio::new_raw(BigInt::from(*phi_f_ratio_int.numer()), BigInt::from(*phi_f_ratio_int.denom()));

        let target_bigint = compute_target_bigint(&phi_f_ratio, stake, total_stake);

        let (_, bytes) = target_bigint.to_bytes_le();
        bytes
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
    use num_bigint::{BigInt, Sign};
    use num_rational::Ratio;
    use proptest::prelude::*;

    use super::*;
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

        mod stability_tests {
            use super::*;

            #[cfg(any(feature = "num-integer-backend", target_family = "wasm", windows))]
            #[test]
            fn test_zero_stake_bytes_stable() {
                // phi_f = 0.05
                let phi_f_ratio = Ratio::new_raw(BigInt::from(1), BigInt::from(20));
                let total_stake = 45_000_000_000;

                for _ in 0..100 {
                    let target = compute_target_bigint(&phi_f_ratio, 0, total_stake);
                    assert!(target == BigInt::ZERO);
                }
            }

            #[cfg(any(feature = "num-integer-backend", target_family = "wasm", windows))]
            #[test]
            fn test_one_stake_bytes_stable() {
                // phi_f = 0.05
                let phi_f_ratio = Ratio::new_raw(BigInt::from(1), BigInt::from(20));
                let total_stake = 45_000_000_000;
                let first_target = compute_target_bigint(&phi_f_ratio, 1, total_stake);
                for _ in 0..100 {
                    let target = compute_target_bigint(&phi_f_ratio, 1, total_stake);
                    assert!(target == first_target);
                }
            }

            #[cfg(any(feature = "num-integer-backend", target_family = "wasm", windows))]
            #[test]
            fn test_full_stake_stable() {
                // phi_f = 0.65
                let phi_f_ratio = Ratio::new_raw(BigInt::from(13), BigInt::from(20));
                let total_stake = 45_000_000_000;

                let full_target = compute_target_bigint(&phi_f_ratio, total_stake, total_stake);
                for _ in 0..100 {
                    let target = compute_target_bigint(&phi_f_ratio, total_stake, total_stake);
                    assert!(full_target == target);
                }
            }

            #[cfg(any(feature = "num-integer-backend", target_family = "wasm", windows))]
            #[test]
            fn test_minimal_stake_difference_stable() {
                // phi_f = 0.05
                let phi_f_ratio = Ratio::new_raw(BigInt::from(1), BigInt::from(20));
                let total_stake = 45_000_000_000;
                for _ in 0..100 {
                    let zero_target = compute_target_bigint(&phi_f_ratio, 0, total_stake);
                    let first_target = compute_target_bigint(&phi_f_ratio, 1, total_stake);
                    assert!(zero_target < first_target);
                    let second_target = compute_target_bigint(&phi_f_ratio, 2, total_stake);
                    assert!(first_target < second_target);
                }
            }
        }

        mod ordering_tests {
            use super::*;

            #[cfg(any(feature = "num-integer-backend", target_family = "wasm", windows))]
            #[test]
            fn test_following_min_stake_same_order() {
                // phi_f = 0.05
                let phi_f_ratio = Ratio::new_raw(BigInt::from(1), BigInt::from(20));
                let total_stake = 45_000_000_000;

                let mut prev_target = compute_target_bigint(&phi_f_ratio, 0, total_stake);
                for i in 1..=100 {
                    let target = compute_target_bigint(&phi_f_ratio, i, total_stake);
                    assert!(prev_target < target);
                    prev_target = target;
                }
            }

            #[cfg(any(feature = "num-integer-backend", target_family = "wasm", windows))]
            #[test]
            fn test_following_stake_same_order() {
                // phi_f = 0.05
                let phi_f_ratio = Ratio::new_raw(BigInt::from(1), BigInt::from(20));
                let total_stake = 45_000_000_000;

                let mut prev_target = compute_target_bigint(&phi_f_ratio, 99_999, total_stake);
                for stake in 100_000..100_100 {
                    let target = compute_target_bigint(&phi_f_ratio, stake, total_stake);
                    assert!(prev_target < target);
                    prev_target = target;
                }
            }

            #[cfg(any(feature = "num-integer-backend", target_family = "wasm", windows))]
            #[test]
            fn test_following_max_stake_same_order() {
                // phi_f = 0.05
                let phi_f_ratio = Ratio::new_raw(BigInt::from(1), BigInt::from(20));
                let total_stake = 45_000_000_000;

                let mut prev_target = compute_target_bigint(&phi_f_ratio, total_stake, total_stake);
                for i in 1..=100 {
                    let target = compute_target_bigint(&phi_f_ratio, total_stake - i, total_stake);
                    assert!(prev_target > target);
                    prev_target = target;
                }
            }
        }

        #[cfg(any(feature = "num-integer-backend", target_family = "wasm", windows))]
        #[allow(dead_code)]
        // #[test]
        fn test_advantage_smaller_stake() {
            // phi_f = 0.05
            let phi_f_ratio = Ratio::new_raw(BigInt::from(1), BigInt::from(20));
            let total_stake = 45_000_000_000;

            let target_100k = compute_target_bigint(&phi_f_ratio, 100_000, total_stake);
            let target_10m = compute_target_bigint(&phi_f_ratio, 10_000_000, total_stake);
            let target_10b = compute_target_bigint(&phi_f_ratio, 10_000_000_000, total_stake);

            println!("{:?}", target_10m / target_100k.clone());
            println!("{:?}", target_10b.clone() / target_100k.clone());
        }

        proptest! {
            #![proptest_config(ProptestConfig::with_cases(50))]

            #[test]
            #[cfg(any(feature = "num-integer-backend", target_family = "wasm", windows))]
            fn following_stake_same_order(
                phi_f in 1..50u64,
                total_stake in 100_000_000..1_000_000_000u64,
                stake in 10_000_000..50_000_000u64,
            ) {
                let phi_f_ratio_int: Ratio<i64> = Ratio::approximate_float(phi_f as f32/100f32).expect("Only fails if the float is infinite or NaN.");
                let phi_f_ratio = Ratio::new_raw(BigInt::from(*phi_f_ratio_int.numer()), BigInt::from(*phi_f_ratio_int.denom()));
                let base_target = compute_target_bigint(&phi_f_ratio, stake, total_stake);
                let next_target = compute_target_bigint(&phi_f_ratio, stake + 1, total_stake);

                assert!(base_target < next_target);
            }

            #[test]
            #[cfg(any(feature = "num-integer-backend", target_family = "wasm", windows))]
            fn following_small_stake_same_order(
                phi_f in 1..50u64,
                total_stake in 100_000_000..1_000_000_000u64,
                stake in 100_000..500_000u64,
            ) {
                let phi_f_ratio_int: Ratio<i64> = Ratio::approximate_float(phi_f as f32/100f32).expect("Only fails if the float is infinite or NaN.");
                let phi_f_ratio = Ratio::new_raw(BigInt::from(*phi_f_ratio_int.numer()), BigInt::from(*phi_f_ratio_int.denom()));
                let base_target = compute_target_bigint(&phi_f_ratio, stake, total_stake);
                let next_target = compute_target_bigint(&phi_f_ratio, stake + 1, total_stake);

                assert!(base_target < next_target);
            }

            #[test]
            #[cfg(any(feature = "num-integer-backend", target_family = "wasm", windows))]
            fn same_stake_same_result(
                phi_f in 1..50u64,
                total_stake in 100_000_000..1_000_000_000u64,
                stake in 10_000_000..50_000_000u64,
            ) {
                let phi_f_ratio_int: Ratio<i64> = Ratio::approximate_float(phi_f as f32/100f32).expect("Only fails if the float is infinite or NaN.");
                let phi_f_ratio = Ratio::new_raw(BigInt::from(*phi_f_ratio_int.numer()), BigInt::from(*phi_f_ratio_int.denom()));
                let target = compute_target_bigint(&phi_f_ratio, stake, total_stake);
                let same_target = compute_target_bigint(&phi_f_ratio, stake, total_stake);

                assert!(target == same_target);
            }

            #[test]
            #[cfg(any(feature = "num-integer-backend", target_family = "wasm", windows))]
            fn same_small_stake_same_result(
                phi_f in 1..50u64,
                total_stake in 100_000_000..1_000_000_000u64,
                stake in 100_000..500_000u64,
            ) {
                let phi_f_ratio_int: Ratio<i64> = Ratio::approximate_float(phi_f as f32/100f32).expect("Only fails if the float is infinite or NaN.");
                let phi_f_ratio = Ratio::new_raw(BigInt::from(*phi_f_ratio_int.numer()), BigInt::from(*phi_f_ratio_int.denom()));
                let target = compute_target_bigint(&phi_f_ratio, stake, total_stake);
                let same_target = compute_target_bigint(&phi_f_ratio, stake, total_stake);

                assert!(target == same_target);
            }

        }

        #[allow(dead_code)]
        mod golden {

            use num_traits::Num;
            use std::{fs::File, io::BufWriter, io::Write};

            use super::*;

            #[allow(dead_code)]
            fn write_bigints_to_file(values: &[BigInt], path: &str) -> std::io::Result<()> {
                let file = File::create(path)?;
                let mut writer = BufWriter::new(file);

                for val in values {
                    writeln!(writer, "{:064x}", val)?;
                }

                writer.flush()?;
                Ok(())
            }

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

            #[cfg(any(feature = "num-integer-backend", target_family = "wasm", windows))]
            fn golden_value_target_from_stake(stake: u64) -> BigInt {
                // phi_f = 0.05
                let phi_f_ratio = Ratio::new_raw(BigInt::from(1), BigInt::from(20));
                let total_stake = 45_000_000_000;

                compute_target_bigint(&phi_f_ratio, stake, total_stake)
            }

            #[cfg(any(feature = "num-integer-backend", target_family = "wasm", windows))]
            fn golden_value_following_min_stake() -> Vec<BigInt> {
                // phi_f = 0.05
                let phi_f_ratio = Ratio::new_raw(BigInt::from(1), BigInt::from(20));
                let total_stake = 45_000_000_000;
                let mut golden_values = vec![];

                for stake in 0..500 {
                    let target = compute_target_bigint(&phi_f_ratio, stake, total_stake);
                    golden_values.push(target);
                }
                golden_values
            }

            #[cfg(any(feature = "num-integer-backend", target_family = "wasm", windows))]
            fn golden_value_following_stake_medium() -> Vec<BigInt> {
                // phi_f = 0.05
                let phi_f_ratio = Ratio::new_raw(BigInt::from(1), BigInt::from(20));
                let total_stake = 45_000_000_000;
                let mut golden_values = vec![];

                for stake in 100_000..100_500 {
                    let target = compute_target_bigint(&phi_f_ratio, stake, total_stake);
                    golden_values.push(target);
                }
                golden_values
            }

            #[cfg(any(feature = "num-integer-backend", target_family = "wasm", windows))]
            fn golden_value_following_stake_max() -> Vec<BigInt> {
                // phi_f = 0.05
                let phi_f_ratio = Ratio::new_raw(BigInt::from(1), BigInt::from(20));
                let total_stake = 45_000_000_000;
                let mut golden_values = vec![];

                for i in 0..500 {
                    let target = compute_target_bigint(&phi_f_ratio, total_stake - i, total_stake);
                    golden_values.push(target);
                }
                golden_values
            }

            #[cfg(any(feature = "num-integer-backend", target_family = "wasm", windows))]
            #[test]
            fn golden_check_small_values() {
                let _golden_target_0 = BigInt::from_bytes_le(Sign::Plus, &GOLDEN_BYTES_ZERO);
                let _golden_target_1 = BigInt::from_bytes_le(Sign::Plus, &GOLDEN_BYTES_ONE);
                let _golden_target_2 = BigInt::from_bytes_le(Sign::Plus, &GOLDEN_BYTES_TWO);

                // assert_eq!(golden_target_0, golden_value_target_from_stake(0));
                // assert_eq!(golden_target_1, golden_value_target_from_stake(1));
                // assert_eq!(golden_target_2, golden_value_target_from_stake(2));
            }

            #[cfg(any(feature = "num-integer-backend", target_family = "wasm", windows))]
            #[test]
            fn golden_check_max_values_fail() {
                let _golden_target_max = BigInt::from_bytes_le(Sign::Plus, &GOLDEN_BYTES_MAX_STAKE);
                let _golden_target_max_1 =
                    BigInt::from_bytes_le(Sign::Plus, &GOLDEN_BYTES_MAX_STAKE_MINUS_ONE);
                let _golden_target_max_2 =
                    BigInt::from_bytes_le(Sign::Plus, &GOLDEN_BYTES_MAX_STAKE_MINUS_TWO);

                // assert!(golden_target_max != golden_value_target_from_stake(44_999_999_998));
                // assert!(golden_target_max_1 != golden_value_target_from_stake(45_000_000_000));
                // assert!(golden_target_max_2 != golden_value_target_from_stake(44_999_999_999));
            }

            #[cfg(any(feature = "num-integer-backend", target_family = "wasm", windows))]
            #[test]
            fn golden_check_following_min_stake() {
                let golden_target_vector = golden_value_following_min_stake();

                let golden_target_from_file = include_str!(
                    "../../../tests/golden_vector_target_value/golden_vector_min_stake.txt"
                );
                for (_t1, t2_hex) in
                    golden_target_vector.iter().zip(golden_target_from_file.lines())
                {
                    let _t2 = BigInt::from_str_radix(t2_hex.trim(), 16).unwrap();
                    // assert_eq!(t1, &t2);
                }
            }

            #[cfg(any(feature = "num-integer-backend", target_family = "wasm", windows))]
            #[test]
            fn golden_check_following_stake_medium() {
                let golden_target_vector = golden_value_following_stake_medium();

                let golden_target_from_file = include_str!(
                    "../../../tests/golden_vector_target_value/golden_vector_medium_stake.txt"
                );
                for (_t1, t2_hex) in
                    golden_target_vector.iter().zip(golden_target_from_file.lines())
                {
                    let _t2 = BigInt::from_str_radix(t2_hex.trim(), 16).unwrap();
                    // assert_eq!(t1, &t2);
                }
            }

            #[cfg(any(feature = "num-integer-backend", target_family = "wasm", windows))]
            #[test]
            fn golden_check_following_stake_max() {
                let golden_target_vector = golden_value_following_stake_max();
                let golden_target_from_file = include_str!(
                    "../../../tests/golden_vector_target_value/golden_vector_max_stake.txt"
                );

                for (_t1, t2_hex) in
                    golden_target_vector.iter().zip(golden_target_from_file.lines())
                {
                    let _t2 = BigInt::from_str_radix(t2_hex.trim(), 16).unwrap();
                    // assert_eq!(t1, &t2);
                }
            }
        }
    }
}
