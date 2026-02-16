cfg_num_integer! {
    use num_bigint::BigInt;
    use num_traits::One;
    use num_rational::Ratio;


    /// Computes a Taylor expansion of the exponential exp(c*w) up to the (N-1)th term
    /// exp(c*x) = 1 + c*x + (c*x)^2/2! + (c*x)^3/3! + ... + (c*x)^{N-1}/(N-1)! + O((c*x)^N)
    /// We want to stop when the next term is less than our precision target, that is epsilon = 2^{-128}
    /// Hence we stop when |(c*x)^N / N!| < epsilon
    /// We can check instead (c * x)^N < epsilon
    /// which gives us the bound N < log(epsilon) / log(|c*x|)
    pub fn compute_exp(x: &Ratio<BigInt>, c: &Ratio<BigInt>, iterations: usize) -> Ratio<BigInt> {
        let cw = c * x;
        let (num, denom, _) = exponential_approximation(0, iterations, cw.numer(), cw.denom());

        Ratio::new_raw(num, denom)
    }

    // ADD COMMENT FOR THIS APPROXIMATION
    /// Function that computes an approximation of exp(a/b) using a binomial splitting
    /// to compute the taylor expansion terms between i and j,
    /// i.e. between (a/b)^first_term * (1/first_term!) and (a/b)^last_term * (1/last_term!)
    pub fn exponential_approximation(first_term: usize, last_term: usize, a: &BigInt, b: &BigInt) -> (BigInt, BigInt, BigInt) {
        if last_term - first_term == 1 {
            if first_term == 0 {
                return (BigInt::one(), BigInt::one(), BigInt::one());
            }
            return (a.clone(), b * BigInt::from(first_term), a.clone());
        }

        let mid = (first_term + last_term) / 2;
        let (numerator_l, denominator_l, auxiliary_value_l) = exponential_approximation(first_term, mid, a, b);
        let (numerator_r, denominator_r, auxiliary_value_r) = exponential_approximation(mid, last_term, a, b);

        let numerator = &numerator_l * &denominator_r + &auxiliary_value_l * &numerator_r;
        let denominator = &denominator_l * &denominator_r;
        let auxiliary_value = auxiliary_value_l * auxiliary_value_r;

        (numerator, denominator, auxiliary_value)
    }

    /// Function that computes an approximation of ln(1 - a/b) using a taylor expansion
    /// for a given number of iterations
    /// ln(1 - a/b) = -a/b - ((a/b)^2)/2) - ((a/b)^3)/3) - ... - ((a/b)^(N-1))/N-1) + o((a/b)^N)
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

    /// Compute the target value of a given party as a base field element using
    /// Taylor expansion for the natural log and the exponential functions
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

        // The number of iteration of the ln approximation is set at 40 for now to get
        // around 92 bits of precision for a value phi_f=0.2
        let ln_one_minus_phi_f = ln_1p_approximation(40, phi_f_ratio.numer(), phi_f_ratio.denom());
        // The number of iteration of the exponential approximation is set at 40 for now to get
        // around 92 bits of precision for a value phi_f=0.2
        let exp_ln_one_minus_phi_f_stake_ratio = compute_exp(&ln_one_minus_phi_f, &stake_ratio, 40);

        let modulus_ratio = Ratio::from(modulus);
        let target_as_ratio = modulus_ratio.clone() - modulus_ratio * exp_ln_one_minus_phi_f_stake_ratio;

        // Floor division
        let (target_as_int, remainder) = target_as_ratio.numer().div_rem(target_as_ratio.denom());
        assert!(target_as_int >= BigInt::zero());

        // Truncate the lower bits of the target value
        // The number of truncated bits is set at 163 for now to match the 92 bits precision
        // of the approximations (255 - 92 = 163)
        let truncated_target: BigInt = (target_as_int >> 163) << 163;

        // If exact division and truncated_target > 0, subtract 1
        if remainder.is_zero() && !truncated_target.is_zero() {
            truncated_target - 1
        } else {
            truncated_target
        }
    }

    /// Compute the target value in bytes form given a value phi_f,
    /// the signer stake and the total stake
    /// The function approximate phi_f as a ratio to make Taylor expansions easier to compute
    #[allow(dead_code)]
    pub fn compute_target_bytes(phi_f: f64, stake: Stake, total_stake: Stake) -> Vec<u8> {

        let phi_f_ratio_int: Ratio<i64> = Ratio::approximate_float(phi_f).expect("Only fails if the float is infinite or NaN.");
        let phi_f_ratio = Ratio::new_raw(BigInt::from(*phi_f_ratio_int.numer()), BigInt::from(*phi_f_ratio_int.denom()));

        let target_bigint = compute_target_bigint(&phi_f_ratio, stake, total_stake);

        let (_, bytes) = target_bigint.to_bytes_le();
        bytes
    }
}

#[cfg(feature = "future_snark")]
use crate::{
    LotteryIndex, LotteryTargetValue, SignatureError, Stake, StmResult, UniqueSchnorrSignature,
    signature_scheme::{BaseFieldElement, DST_LOTTERY, compute_poseidon_digest},
};

#[cfg(feature = "future_snark")]
// TODO: remove this allow dead_code directive when function is called or future_snark is activated
#[allow(dead_code)]
/// Computes the lottery target value for SNARK proof system.
///
/// The target value determines the probability of winning the lottery based on the
/// participant's stake relative to the total stake. A higher stake results in a higher
/// target value, increasing the probability of eligibility.
///
/// The target value is computed using the following formula:
///
/// We need to check that:
/// signer_lottery_hash / p < 1 - (1 - phi_f)^w
/// <=> signer_lottery_hash < p * (1 - (1 - phi_f)^w)
/// where p is the modulus of the field, phi_f a parameter constant comprised in ]0,1], w a variable in ]0,1]
/// and signer_lottery_hash is the hash computed using the signer's signature and lottery index.
///
/// Since the modulus is a 255 bits number, we need to compute (1 - (1 - phi_f)^w) with enough precision
/// to maintain the lottery functional, i.e. have different targets for different stakes, however close they are.
///
/// In order to do that we change the expression:
/// 1 - (1 - phi_f)^w = 1 - exp(w * ln(1 - phi_f))
/// and we use Taylor expansion to approximate the exponential and natural logarithm functions to a given precision.
///
/// Once the precise expression obtained, we can compute the target value as:
/// target = floor(p * (1 - exp(w * ln(1 - phi_f))))
/// and truncate the unecessary bits that are below the precision threshold to ensure more stability in the result.
pub fn compute_target_value(phi_f: f64, stake: Stake, total_stake: Stake) -> LotteryTargetValue {
    // If phi_f = 1, then we automatically break with true
    if (phi_f - 1.0).abs() < f64::EPSILON {
        return -BaseFieldElement::get_one();
    }

    let mut bytes: Vec<u8> = compute_target_bytes(phi_f, stake, total_stake);
    bytes.resize(32, 0);
    BaseFieldElement::from_bytes(&bytes).unwrap()
}

#[cfg(feature = "future_snark")]
// TODO: remove this allow dead_code directive when function is called or future_snark is activated
#[allow(dead_code)]
/// Computes the lottery prefix hash from a message.
/// The prefix is computed by prepending `DST_LOTTERY`
/// to the message and hashing the result using `compute_poseidon_digest`.
pub fn lottery_prefix(msg: &[BaseFieldElement]) -> BaseFieldElement {
    let mut prefix = vec![DST_LOTTERY];
    prefix.extend_from_slice(msg);
    compute_poseidon_digest(&prefix)
}

#[cfg(feature = "future_snark")]
// TODO: remove this allow dead_code directive when function is called or future_snark is activated
#[allow(dead_code)]
/// Verifies if a lottery index is eligible based on the signature and target value.
///
/// This function checks whether a given index wins the lottery by computing an
/// evaluation value from the signature's commitment point and the index, then
/// comparing it against the target value. An index is eligible if its
/// evaluation value is less than or equal to the target.
///
/// The evaluation is computed as: `ev = Poseidon(prefix, sigma_x, sigma_y, index)`
/// where `(sigma_x, sigma_y)` are the coordinates of the signature's commitment point.
pub fn check_index(
    signature: &UniqueSchnorrSignature,
    index: LotteryIndex,
    m: u64,
    prefix: BaseFieldElement,
    target: LotteryTargetValue,
) -> StmResult<()> {
    if index > m {
        return Err(SignatureError::IndexBoundFailed(index, m).into());
    }

    let idx = BaseFieldElement::from(index);
    let (sigma_x, sigma_y) = signature.commitment_point.get_coordinates();
    let ev = compute_poseidon_digest(&[prefix, sigma_x, sigma_y, idx]);

    // check if ev <= target
    if ev > target {
        return Err(SignatureError::LotteryLost.into());
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use num_bigint::{BigInt, Sign};
    use num_rational::Ratio;
    use proptest::prelude::*;
    use rand_core::OsRng;

    // use crate::LotteryTargetValue;
    #[cfg(feature = "future_snark")]
    use crate::{SchnorrSigningKey, signature_scheme::BaseFieldElement};

    #[cfg(feature = "future_snark")]
    use super::{
        check_index, compute_target_bigint, compute_target_bytes, compute_target_value,
        lottery_prefix,
    };

    #[cfg(test)]
    mod test_bytes_computation {
        use super::*;

        #[cfg(any(feature = "num-integer-backend", target_family = "wasm", windows))]
        #[cfg(feature = "future_snark")]
        #[test]
        fn test_zero_stake_bytes() {
            let phi_f = 0.05;
            let total_stake = 45_000_000_000;

            for _ in 0..100 {
                let target = compute_target_bytes(phi_f, 0, total_stake);
                assert!(target == vec![0u8]);
            }
        }
    }

    #[cfg(feature = "future_snark")]
    #[test]
    fn test_target_phi_f_one() {
        // Case where `phi_f` is exactly 1
        let phi_f = 1.0;
        let stake = 50;
        let total_stake = 100;

        let result = compute_target_value(phi_f, stake, total_stake);

        // Since `phi_f` is 1, the function should return the maximum target (-F::ONE)
        assert_eq!(result, -BaseFieldElement::get_one());
    }

    #[cfg(feature = "future_snark")]
    #[test]
    fn test_target_half_stake() {
        // Case where `stake` is exactly half of `total_stake`
        let phi_f = 0.5;
        let stake = 50;
        let total_stake = 100;

        let result = compute_target_value(phi_f, stake, total_stake);

        // Validate that result is in the expected range
        assert!(result != -BaseFieldElement::get_one());
    }

    #[cfg(feature = "future_snark")]
    #[test]
    fn test_check_index() {
        let phi_f = 0.2;
        let stake = 30;
        let total_stake = 100;

        let target = compute_target_value(phi_f, stake, total_stake);
        println!("Target = {:?}", target);

        let sk = SchnorrSigningKey::generate(&mut OsRng).unwrap();
        let msg = BaseFieldElement::random(&mut OsRng);
        let sig = sk.sign(&msg.to_bytes(), &mut OsRng).unwrap();

        let m = 100;
        let mut counter = 0;
        let prefix = lottery_prefix(&[msg]);
        for i in 0..m {
            if check_index(&sig, i, m, prefix, target).is_ok() {
                println!("Index: {}", i);
                counter += 1;
            }
        }
        println!("Total eligible indices:{:?}", counter);
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
