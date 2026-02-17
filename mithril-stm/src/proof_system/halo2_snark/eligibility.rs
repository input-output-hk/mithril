cfg_num_integer! {
    use num_bigint::BigInt;
    use num_integer::Integer;
    use num_rational::Ratio;
    use num_traits::{Num, One};

    #[cfg(feature = "future_snark")]
    use crate::{
        LotteryIndex, LotteryTargetValue, SignatureError, Stake, StmResult, UniqueSchnorrSignature,
        signature_scheme::{BaseFieldElement, compute_poseidon_digest, DST_LOTTERY},
    };

    /// Modulus of the Jubjub Base Field as a hexadecimal number
    const JUBJUB_BASE_FIELD_MODULUS: &str = "73eda753299d7d483339d80809a1d80553bda402fffe5bfeffffffff00000001";
    /// Set the number of iterations of the Taylor expansions. The higher the number, the more precise
    /// the value is. A value of 40 provides ~92 bits precision for phi_f=0.2
    const TAYLOR_EXPANSION_ITERATIONS: usize = 40;
    /// Set the number of bits to truncate from the target value. It should be in relation
    /// with the number of iterations as we want to truncate the bits not used for precision.
    /// A value of 163 matches 92 bits of precision (255 - 92 = 163, where 255 is the
    /// number of bits of the modulus)
    const TRUNCATION_BITS: usize = 163;

    #[cfg(feature = "future_snark")]
    // TODO: remove this allow dead_code directive when function is called or future_snark is activated
    #[allow(dead_code)]
    /// Computes the lottery target value for SNARK proof system as a base field element.
    ///
    /// The target value determines the probability of winning the lottery based on the
    /// participant's stake relative to the total stake. A higher stake results in a higher
    /// target value, increasing the probability of eligibility.
    ///
    /// The target value is computed using the following formula, we need to check that:
    /// signer_lottery_hash < p * (1 - (1 - phi_f)^w)
    /// where p is the modulus of the field, phi_f is the protocol security parameter constant comprised in ]0,1],
    /// w = stake/total_stake and signer_lottery_hash is the hash computed using the
    /// signer's signature and the index tested for the lottery.
    /// Since the modulus is a 255 bits number, we need to compute (1 - (1 - phi_f)^w) with enough precision
    /// to maintain the lottery functional, i.e. have different targets for different stakes
    /// and maintain the same order, however close they are.
    /// In addition, we need this computation to be deterministic as it will be computed by all signers to create
    /// the merkle_tree. The output should be the same no matter where it is computed (i.e. different config, OS, ...).
    ///
    /// In order to do that we change the expression:
    /// 1 - (1 - phi_f)^w = 1 - exp(w * ln(1 - phi_f))
    /// and we use Taylor expansion to approximate the exponential and natural logarithm functions to a given precision.
    ///
    /// Once the precise expression obtained, we can compute the target value as:
    /// target = floor(p * (1 - exp(w * ln(1 - phi_f))))
    /// and truncate the unnecessary bits that are below the precision threshold to ensure more stability in the result.
    ///
    /// Input: (ln(1 - phi_f) approximation, stake of the signer, total_stake)
    ///
    /// Output: the lottery target value used for the lottery eligibility
    pub fn compute_target_value(ln_one_minus_phi_f: &Ratio<BigInt>, stake: Stake, total_stake: Stake) -> LotteryTargetValue {
        // It is safe to use .expect() as the value used is a constant so the creation of
        // the BigInt will never fail
        let modulus = BigInt::from_str_radix(
            JUBJUB_BASE_FIELD_MODULUS,
            16,
        )
        .expect("Hardcoded modulus of the Jubjub base field hex string is valid");

        let stake_ratio = Ratio::new_raw(BigInt::from(stake), BigInt::from(total_stake));

        let exp_ln_one_minus_phi_f_stake_ratio = compute_exponential_taylor_expansion(ln_one_minus_phi_f, &stake_ratio, TAYLOR_EXPANSION_ITERATIONS);

        let modulus_ratio = Ratio::from(modulus);
        let target_as_ratio = modulus_ratio.clone() - modulus_ratio * exp_ln_one_minus_phi_f_stake_ratio;

        // Floor division
        let (target_as_int, _) = target_as_ratio.numer().div_rem(target_as_ratio.denom());

        // Truncate the lower bits of the target value
        // The value TRUNCATION_BITS depends on the precision we have for the approximations
        let truncated_target: BigInt = (target_as_int >> TRUNCATION_BITS) << TRUNCATION_BITS;

        let (_, mut bytes) = truncated_target.to_bytes_le();
        bytes.resize(32, 0);
        // It is safe to use .expect() as the value resulting from the computation is always lower than
        // the Jubjub modulus
        BaseFieldElement::from_bytes(&bytes).expect("Input bytes are always lower than the Jubjub modulus hence canonical.")
    }

    /// Computes a Taylor expansion of the exponential exp(c*w) up to the (N-1)th term
    /// where N corresponds to the number of iterations
    /// exp(c*w) = 1 + c*w + (c*w)^2/2! + (c*w)^3/3! + ... + (c*w)^{N-1}/(N-1)! + O((c*w)^N)
    /// We want to stop when the next term is less than our precision target epsilon
    /// Hence we stop when |(c*w)^N / N!| < epsilon
    /// We can check instead (c * w)^N < epsilon which gives us the bound N < log(epsilon) / log(|c*w|)
    /// Setting epsilon to a specific precision gives use the number of iterations we need to do
    ///
    /// Since the value c * w is a float between 0 and 1, it is expressed as a Ratio of BigInt and the exponential
    /// approximation is adapted to that form
    pub fn compute_exponential_taylor_expansion(c: &Ratio<BigInt>, w: &Ratio<BigInt>, iterations: usize) -> Ratio<BigInt> {
        let cw = c * w;
        let (num, denom, _) = exponential_approximation(0, iterations, cw.numer(), cw.denom());

        Ratio::new_raw(num, denom)
    }


    /// Function that computes an approximation of exp(a/b) using a binomial splitting
    /// to compute the taylor expansion terms between first_term and last_term,
    /// i.e. between (a/b)^first_term * (1/first_term!) and (a/b)^last_term * (1/last_term!)
    ///
    /// We have that exp(a/b) = 1 + a/b + (a/b)^2/2! + (a/b)^3/3! + ... + (a/b)^{N-1}/(N-1)! + O((a/b)^N)
    /// This function splits this computation in the middle and recursively compute each side by finding the quotient
    /// X/Y that equals the sum of the terms in the split. Those terms are recursively combined to obtain the final
    /// approximation of exp(a/b)
    pub fn exponential_approximation(first_term: usize, last_term: usize, a: &BigInt, b: &BigInt) -> (BigInt, BigInt, BigInt) {
        if last_term - first_term == 1 {
            if first_term == 0 {
                return (BigInt::one(), BigInt::one(), BigInt::one());
            }
            return (a.clone(), b * BigInt::from(first_term), a.clone());
        }

        let middle = (first_term + last_term) / 2;
        // Computes the terms to the left of the middle
        let (numerator_left, denominator_left, auxiliary_value_left) = exponential_approximation(first_term, middle, a, b);
        // Computes the terms to the right of the middle
        let (numerator_right, denominator_right, auxiliary_value_right) = exponential_approximation(middle, last_term, a, b);

        let numerator = &numerator_left * &denominator_right + &auxiliary_value_left * &numerator_right;
        let denominator = &denominator_left * &denominator_right;
        let auxiliary_value = auxiliary_value_left * auxiliary_value_right;

        // returns the numerator and denominator of the computed terms
        // as well as an additional value to help with the rest of the computation
        (numerator, denominator, auxiliary_value)
    }

    /// Function that computes an approximation of ln(1 - a/b) using a taylor expansion
    /// for a given number N of iterations
    /// ln(1 - a/b) = -a/b - ((a/b)^2)/2) - ((a/b)^3)/3) - ... - ((a/b)^(N))/N) + o((a/b)^(N+1))
    ///
    /// It performs a straighforward for loop in a naive way updating the numerator and denominator
    /// every loop and the accumulator using the new values. The numerator stores the power of a and
    /// the denominator the power of b.
    #[allow(dead_code)]
    fn ln_1p_taylor_expansion(iterations: usize, a: &BigInt, b: &BigInt) -> Ratio<BigInt> {
        let mut numerator = a.clone();
        let mut denominator = b.clone();
        let mut accumulator = Ratio::new_raw(a.clone(),b.clone());
        for i in 2..(iterations + 1) {
            numerator *= a;
            denominator *= b;
            accumulator += Ratio::new_raw(numerator.clone(), denominator.clone() * i);
        }

        -accumulator
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
    /// The evaluation is computed as: `ev = Poseidon(prefix, commitment_point_x, commitment_point_y, index)`
    /// where `(commitment_point_x, commitment_point_y)` are the coordinates of the signature's commitment point.
    pub fn verify_lottery_eligibility(
        signature: &UniqueSchnorrSignature,
        lottery_index: LotteryIndex,
        m: u64,
        prefix: BaseFieldElement,
        target: LotteryTargetValue,
    ) -> StmResult<()> {
        if lottery_index > m {
            return Err(SignatureError::IndexBoundFailed(lottery_index, m).into());
        }

        let lottery_index_as_base_field_element = BaseFieldElement::from(lottery_index);
        let (commitment_point_x, commitment_point_y) = signature.commitment_point.get_coordinates();
        let lottery_evaluation = compute_poseidon_digest(&[prefix, commitment_point_x, commitment_point_y, lottery_index_as_base_field_element]);

        // check if ev <= target
        if lottery_evaluation > target {
            return Err(SignatureError::LotteryLost.into());
        }

        Ok(())
    }

    #[cfg(feature = "future_snark")]
    // TODO: remove this allow dead_code directive when function is called or future_snark is activated
    #[allow(dead_code)]
    /// Computes the lottery prefix hash from a message.
    /// The prefix is computed by prepending `DST_LOTTERY`
    /// to the message and hashing the result using `compute_poseidon_digest`.
    pub fn compute_lottery_prefix(msg: &[BaseFieldElement]) -> BaseFieldElement {
        let mut prefix = vec![DST_LOTTERY];
        prefix.extend_from_slice(msg);
        compute_poseidon_digest(&prefix)
    }
}

#[cfg(any(feature = "num-integer-backend", target_family = "wasm", windows))]
#[cfg(feature = "future_snark")]
#[cfg(test)]
mod tests {
    use num_bigint::BigInt;
    use num_rational::Ratio;
    use num_traits::ToPrimitive;
    use proptest::prelude::*;
    use rand_core::OsRng;

    use crate::{LotteryTargetValue, SchnorrSigningKey, signature_scheme::BaseFieldElement};

    use super::{
        compute_exponential_taylor_expansion, compute_lottery_prefix, compute_target_value,
        ln_1p_taylor_expansion, verify_lottery_eligibility,
    };

    #[test]
    fn advantage_small_enough() {
        let phi_f = 0.2;
        let phi_f_ratio_int: Ratio<i64> =
            Ratio::approximate_float(phi_f).expect("Only fails if the float is infinite or NaN.");
        let phi_f_ratio = Ratio::new_raw(
            BigInt::from(*phi_f_ratio_int.numer()),
            BigInt::from(*phi_f_ratio_int.denom()),
        );
        let ln_one_minus_phi_f =
            ln_1p_taylor_expansion(40, phi_f_ratio.numer(), phi_f_ratio.denom());
        let total_stake = 10_000;
        let stake = 7_500;
        let split = 10;

        let stake_ratio_full = Ratio::new_raw(BigInt::from(stake), BigInt::from(total_stake));
        let phi_full =
            compute_exponential_taylor_expansion(&ln_one_minus_phi_f, &stake_ratio_full, 40);

        let stake_ratio_split =
            Ratio::new_raw(BigInt::from(stake / split), BigInt::from(total_stake));
        let phi_split =
            compute_exponential_taylor_expansion(&ln_one_minus_phi_f, &stake_ratio_split, 40);

        let adv = phi_full.clone() - phi_split.pow(split);
        assert!(adv.to_f64().unwrap() < 1e-10);
    }

    mod lottery_computations {
        use super::*;

        #[test]
        fn check_valid_index() {
            let phi_f = 0.2;
            let phi_f_ratio_int: Ratio<i64> = Ratio::approximate_float(phi_f)
                .expect("Only fails if the float is infinite or NaN.");
            let phi_f_ratio = Ratio::new_raw(
                BigInt::from(*phi_f_ratio_int.numer()),
                BigInt::from(*phi_f_ratio_int.denom()),
            );
            let ln_one_minus_phi_f =
                ln_1p_taylor_expansion(40, phi_f_ratio.numer(), phi_f_ratio.denom());

            let stake = 30;
            let total_stake = 100;

            let lottery_target_value =
                compute_target_value(&ln_one_minus_phi_f, stake, total_stake);
            println!("Target = {:?}", lottery_target_value);

            let sk = SchnorrSigningKey::generate(&mut OsRng);
            let msg = BaseFieldElement::random(&mut OsRng);
            let sig = sk.sign(&[msg], &mut OsRng).unwrap();

            let m = 100;
            let mut counter = 0;
            let prefix = compute_lottery_prefix(&[msg]);
            for i in 0..m {
                if verify_lottery_eligibility(&sig, i, m, prefix, lottery_target_value).is_ok() {
                    println!("Index: {}", i);
                    counter += 1;
                }
            }
            println!("Total eligible indices:{:?}", counter);
        }

        #[test]
        fn lottery_fails_for_target_zero() {
            let lottery_target_value = LotteryTargetValue::from(0);
            let sk = SchnorrSigningKey::generate(&mut OsRng);
            let msg = BaseFieldElement::random(&mut OsRng);
            let sig = sk.sign(&[msg], &mut OsRng).unwrap();
            let m = 100;
            let prefix = compute_lottery_prefix(&[msg]);

            for i in 0..m {
                let result = verify_lottery_eligibility(&sig, i, m, prefix, lottery_target_value);
                result.expect_err("Lottery eligibility should always fail if target is 0.");
            }
        }

        #[test]
        fn lottery_fails_for_index_greater_m() {
            let lottery_target_value = LotteryTargetValue::from(0);
            let sk = SchnorrSigningKey::generate(&mut OsRng);
            let msg = BaseFieldElement::random(&mut OsRng);
            let sig = sk.sign(&[msg], &mut OsRng).unwrap();
            let m = 100;
            let prefix = compute_lottery_prefix(&[msg]);

            for i in (m + 1)..(m + 50) {
                let result = verify_lottery_eligibility(&sig, i, m, prefix, lottery_target_value);
                result.expect_err(
                    "Lottery eligibility should always fail if index is greater than m.",
                );
            }
        }
    }

    #[cfg(test)]
    mod stability_of_target_value {
        use super::*;

        mod stability_tests {
            use super::*;

            #[test]
            fn zero_stake_bytes_stable() {
                // phi_f = 0.05
                let phi_f_ratio = Ratio::new_raw(BigInt::from(1), BigInt::from(20));
                let ln_one_minus_phi_f =
                    ln_1p_taylor_expansion(40, phi_f_ratio.numer(), phi_f_ratio.denom());

                let total_stake = 45_000_000_000;

                for _ in 0..10 {
                    let target = compute_target_value(&ln_one_minus_phi_f, 0, total_stake);
                    assert_eq!(target, BaseFieldElement::from(0));
                }
            }

            #[test]
            fn one_stake_bytes_stable() {
                // phi_f = 0.05
                let phi_f_ratio = Ratio::new_raw(BigInt::from(1), BigInt::from(20));
                let ln_one_minus_phi_f =
                    ln_1p_taylor_expansion(40, phi_f_ratio.numer(), phi_f_ratio.denom());
                let total_stake = 45_000_000_000;
                let first_target = compute_target_value(&ln_one_minus_phi_f, 1, total_stake);

                for _ in 0..10 {
                    let target = compute_target_value(&ln_one_minus_phi_f, 1, total_stake);
                    assert_eq!(target, first_target);
                }
            }

            #[test]
            fn full_stake_stable() {
                // phi_f = 0.65
                let phi_f_ratio = Ratio::new_raw(BigInt::from(13), BigInt::from(20));
                let ln_one_minus_phi_f =
                    ln_1p_taylor_expansion(40, phi_f_ratio.numer(), phi_f_ratio.denom());
                let total_stake = 45_000_000_000;

                let full_target =
                    compute_target_value(&ln_one_minus_phi_f, total_stake, total_stake);
                for _ in 0..10 {
                    let target =
                        compute_target_value(&ln_one_minus_phi_f, total_stake, total_stake);
                    assert_eq!(full_target, target);
                }
            }

            #[test]
            fn minimal_stake_difference_stable() {
                // phi_f = 0.05
                let phi_f_ratio = Ratio::new_raw(BigInt::from(1), BigInt::from(20));
                let ln_one_minus_phi_f =
                    ln_1p_taylor_expansion(40, phi_f_ratio.numer(), phi_f_ratio.denom());
                let total_stake = 45_000_000_000;
                for _ in 0..10 {
                    let zero_target = compute_target_value(&ln_one_minus_phi_f, 0, total_stake);
                    let first_target = compute_target_value(&ln_one_minus_phi_f, 1, total_stake);
                    assert!(zero_target < first_target);
                    let second_target = compute_target_value(&ln_one_minus_phi_f, 2, total_stake);
                    assert!(first_target < second_target);
                }
            }
        }

        mod stable_ordering {
            use super::*;

            #[test]
            fn following_min_stake_same_order() {
                // phi_f = 0.05
                let phi_f_ratio = Ratio::new_raw(BigInt::from(1), BigInt::from(20));
                let ln_one_minus_phi_f =
                    ln_1p_taylor_expansion(40, phi_f_ratio.numer(), phi_f_ratio.denom());
                let total_stake = 45_000_000_000;

                let mut prev_target = compute_target_value(&ln_one_minus_phi_f, 0, total_stake);
                for i in 1..=10 {
                    let target = compute_target_value(&ln_one_minus_phi_f, i, total_stake);
                    assert!(prev_target < target);
                    prev_target = target;
                }
            }

            #[test]
            fn following_stake_same_order() {
                // phi_f = 0.05
                let phi_f_ratio = Ratio::new_raw(BigInt::from(1), BigInt::from(20));
                let ln_one_minus_phi_f =
                    ln_1p_taylor_expansion(40, phi_f_ratio.numer(), phi_f_ratio.denom());
                let total_stake = 45_000_000_000;

                let mut prev_target =
                    compute_target_value(&ln_one_minus_phi_f, 99_999, total_stake);
                for stake in 100_000..100_010 {
                    let target = compute_target_value(&ln_one_minus_phi_f, stake, total_stake);
                    assert!(prev_target < target);
                    prev_target = target;
                }
            }

            #[test]
            fn following_max_stake_same_order() {
                // phi_f = 0.05
                let phi_f_ratio = Ratio::new_raw(BigInt::from(1), BigInt::from(20));
                let ln_one_minus_phi_f =
                    ln_1p_taylor_expansion(40, phi_f_ratio.numer(), phi_f_ratio.denom());
                let total_stake = 45_000_000_000;

                let mut prev_target =
                    compute_target_value(&ln_one_minus_phi_f, total_stake, total_stake);
                for i in 1..=10 {
                    let target =
                        compute_target_value(&ln_one_minus_phi_f, total_stake - i, total_stake);
                    assert!(prev_target > target);
                    prev_target = target;
                }
            }
        }

        proptest! {
            #![proptest_config(ProptestConfig::with_cases(10))]

            #[test]
            fn following_stake_same_order(
                phi_f in 1..50u64,
                total_stake in 100_000_000..1_000_000_000u64,
                stake in 10_000_000..50_000_000u64,
            ) {
                let phi_f_ratio_int: Ratio<i64> = Ratio::approximate_float(phi_f as f32/100f32).expect("Only fails if the float is infinite or NaN.");
                let phi_f_ratio = Ratio::new_raw(BigInt::from(*phi_f_ratio_int.numer()), BigInt::from(*phi_f_ratio_int.denom()));
                                let ln_one_minus_phi_f =
                    ln_1p_taylor_expansion(40, phi_f_ratio.numer(), phi_f_ratio.denom());
                let base_target = compute_target_value(&ln_one_minus_phi_f, stake, total_stake);
                let next_target = compute_target_value(&ln_one_minus_phi_f, stake + 1, total_stake);

                assert!(base_target < next_target);
            }

            #[test]
            fn following_small_stake_same_order(
                phi_f in 1..50u64,
                total_stake in 100_000_000..1_000_000_000u64,
                stake in 100_000..500_000u64,
            ) {
                let phi_f_ratio_int: Ratio<i64> = Ratio::approximate_float(phi_f as f32/100f32).expect("Only fails if the float is infinite or NaN.");
                let phi_f_ratio = Ratio::new_raw(BigInt::from(*phi_f_ratio_int.numer()), BigInt::from(*phi_f_ratio_int.denom()));
                                let ln_one_minus_phi_f =
                    ln_1p_taylor_expansion(40, phi_f_ratio.numer(), phi_f_ratio.denom());
                let base_target = compute_target_value(&ln_one_minus_phi_f, stake, total_stake);
                let next_target = compute_target_value(&ln_one_minus_phi_f, stake + 1, total_stake);

                assert!(base_target < next_target);
            }

            #[test]
            fn same_stake_same_result(
                phi_f in 1..50u64,
                total_stake in 100_000_000..1_000_000_000u64,
                stake in 10_000_000..50_000_000u64,
            ) {
                let phi_f_ratio_int: Ratio<i64> = Ratio::approximate_float(phi_f as f32/100f32).expect("Only fails if the float is infinite or NaN.");
                let phi_f_ratio = Ratio::new_raw(BigInt::from(*phi_f_ratio_int.numer()), BigInt::from(*phi_f_ratio_int.denom()));
                                let ln_one_minus_phi_f =
                    ln_1p_taylor_expansion(40, phi_f_ratio.numer(), phi_f_ratio.denom());
                let target = compute_target_value(&ln_one_minus_phi_f, stake, total_stake);
                let same_target = compute_target_value(&ln_one_minus_phi_f, stake, total_stake);

                assert_eq!(target, same_target);
            }

            #[test]
            fn same_small_stake_same_result(
                phi_f in 1..50u64,
                total_stake in 100_000_000..1_000_000_000u64,
                stake in 100_000..500_000u64,
            ) {
                let phi_f_ratio_int: Ratio<i64> = Ratio::approximate_float(phi_f as f32/100f32).expect("Only fails if the float is infinite or NaN.");
                let phi_f_ratio = Ratio::new_raw(BigInt::from(*phi_f_ratio_int.numer()), BigInt::from(*phi_f_ratio_int.denom()));
                                let ln_one_minus_phi_f =
                    ln_1p_taylor_expansion(40, phi_f_ratio.numer(), phi_f_ratio.denom());
                let target = compute_target_value(&ln_one_minus_phi_f, stake, total_stake);
                let same_target = compute_target_value(&ln_one_minus_phi_f, stake, total_stake);
                assert_eq!(target, same_target);
            }

        }

        mod golden {

            use super::*;

            const GOLDEN_BYTES_ZERO: [u8; 32] = [
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                0, 0, 0, 0,
            ];

            const GOLDEN_BYTES_ONE: [u8; 32] = [
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 144, 215, 156, 110,
                100, 74, 145, 0, 0, 0, 0, 0,
            ];

            const GOLDEN_BYTES_TWO: [u8; 32] = [
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 8, 249, 56, 221, 200,
                148, 34, 1, 0, 0, 0, 0,
            ];

            const GOLDEN_BYTES_MAX_STAKE_MINUS_TWO: [u8; 32] = [
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 64, 170, 99, 48, 235,
                56, 154, 90, 247, 225, 203, 5,
            ];

            const GOLDEN_BYTES_MAX_STAKE_MINUS_ONE: [u8; 32] = [
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 209, 70, 102, 151,
                63, 36, 91, 247, 225, 203, 5,
            ];

            const GOLDEN_BYTES_MAX_STAKE: [u8; 32] = [
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 200, 74, 41, 156, 67,
                70, 174, 91, 247, 225, 203, 5,
            ];

            fn golden_value_target_from_stake(stake: u64, total_stake: u64) -> BaseFieldElement {
                // phi_f = 0.05
                let phi_f_ratio = Ratio::new_raw(BigInt::from(1), BigInt::from(20));
                let ln_one_minus_phi_f =
                    ln_1p_taylor_expansion(40, phi_f_ratio.numer(), phi_f_ratio.denom());

                compute_target_value(&ln_one_minus_phi_f, stake, total_stake)
            }

            fn golden_value_following_min_stake() -> Vec<BaseFieldElement> {
                // phi_f = 0.05
                let phi_f_ratio = Ratio::new_raw(BigInt::from(1), BigInt::from(20));
                let ln_one_minus_phi_f =
                    ln_1p_taylor_expansion(40, phi_f_ratio.numer(), phi_f_ratio.denom());
                let total_stake = 45_000_000_000;
                let mut golden_values = vec![];

                for stake in 0..50 {
                    let target = compute_target_value(&ln_one_minus_phi_f, stake, total_stake);
                    golden_values.push(target);
                }
                golden_values
            }

            fn golden_value_following_stake_medium() -> Vec<BaseFieldElement> {
                // phi_f = 0.05
                let phi_f_ratio = Ratio::new_raw(BigInt::from(1), BigInt::from(20));
                let ln_one_minus_phi_f =
                    ln_1p_taylor_expansion(40, phi_f_ratio.numer(), phi_f_ratio.denom());
                let total_stake = 45_000_000_000;
                let mut golden_values = vec![];

                for stake in 100_000..100_050 {
                    let target = compute_target_value(&ln_one_minus_phi_f, stake, total_stake);
                    golden_values.push(target);
                }
                golden_values
            }

            fn golden_value_following_stake_max() -> Vec<BaseFieldElement> {
                // phi_f = 0.05
                let phi_f_ratio = Ratio::new_raw(BigInt::from(1), BigInt::from(20));
                let ln_one_minus_phi_f =
                    ln_1p_taylor_expansion(40, phi_f_ratio.numer(), phi_f_ratio.denom());
                let total_stake = 45_000_000_000;
                let mut golden_values = vec![];

                for i in 0..50 {
                    let target =
                        compute_target_value(&ln_one_minus_phi_f, total_stake - i, total_stake);
                    golden_values.push(target);
                }
                golden_values
            }

            #[test]
            fn golden_check_small_values() {
                let golden_target_0 = BaseFieldElement::from_bytes(&GOLDEN_BYTES_ZERO).unwrap();
                let golden_target_1 = BaseFieldElement::from_bytes(&GOLDEN_BYTES_ONE).unwrap();
                let golden_target_2 = BaseFieldElement::from_bytes(&GOLDEN_BYTES_TWO).unwrap();

                assert_eq!(
                    golden_target_0,
                    golden_value_target_from_stake(0, 45_000_000_000)
                );
                assert_eq!(
                    golden_target_1,
                    golden_value_target_from_stake(1, 45_000_000_000)
                );
                assert_eq!(
                    golden_target_2,
                    golden_value_target_from_stake(2, 45_000_000_000)
                );
            }

            #[test]
            fn golden_check_max_values_fail() {
                let golden_target_max =
                    BaseFieldElement::from_bytes(&GOLDEN_BYTES_MAX_STAKE).unwrap();
                let golden_target_max_1 =
                    BaseFieldElement::from_bytes(&GOLDEN_BYTES_MAX_STAKE_MINUS_ONE).unwrap();
                let golden_target_max_2 =
                    BaseFieldElement::from_bytes(&GOLDEN_BYTES_MAX_STAKE_MINUS_TWO).unwrap();

                assert!(
                    golden_target_max
                        != golden_value_target_from_stake(44_999_999_998, 45_000_000_000)
                );
                assert!(
                    golden_target_max_1
                        != golden_value_target_from_stake(45_000_000_000, 45_000_000_000)
                );
                assert!(
                    golden_target_max_2
                        != golden_value_target_from_stake(44_999_999_999, 45_000_000_000)
                );
            }

            #[test]
            fn golden_check_following_min_stake() {
                let golden_target_vector = golden_value_following_min_stake();

                let golden_target_from_file =
                    include_str!("./golden_vectors/golden_vector_min_stake.txt");
                for (t1, t2_str) in golden_target_vector.iter().zip(golden_target_from_file.lines())
                {
                    let t2: Vec<u8> = serde_json::from_str(t2_str).unwrap();
                    let t2_base_field = BaseFieldElement::from_bytes(&t2).unwrap();
                    assert_eq!(t1, &t2_base_field);
                }
            }

            #[test]
            fn golden_check_following_stake_medium() {
                let golden_target_vector = golden_value_following_stake_medium();

                let golden_target_from_file =
                    include_str!("./golden_vectors/golden_vector_medium_stake.txt");
                for (t1, t2_str) in golden_target_vector.iter().zip(golden_target_from_file.lines())
                {
                    let t2: Vec<u8> = serde_json::from_str(t2_str).unwrap();
                    let t2_base_field = BaseFieldElement::from_bytes(&t2).unwrap();
                    assert_eq!(t1, &t2_base_field);
                }
            }

            #[test]
            fn golden_check_following_stake_max() {
                let golden_target_vector = golden_value_following_stake_max();
                let golden_target_from_file =
                    include_str!("./golden_vectors/golden_vector_max_stake.txt");

                for (t1, t2_str) in golden_target_vector.iter().zip(golden_target_from_file.lines())
                {
                    let t2: Vec<u8> = serde_json::from_str(t2_str).unwrap();
                    let t2_base_field = BaseFieldElement::from_bytes(&t2).unwrap();
                    assert_eq!(t1, &t2_base_field);
                }
            }
        }
    }
}
