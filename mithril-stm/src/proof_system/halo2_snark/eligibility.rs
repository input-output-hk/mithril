cfg_num_integer! {
    use num_bigint::BigInt;
    use num_rational::Ratio;

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
    pub fn compute_target_bytes(phi_f: f64, stake: Stake, total_stake: Stake) -> Vec<u8> {
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
        let target = if remainder.is_zero() && !t_int.is_zero() {
            t_int - 1
        } else {
            t_int
        };

        let (_, bytes) = target.to_bytes_le();
        bytes
    }
}

cfg_rug! {
    #[cfg(feature = "future_snark")]
    use rug::{Float, integer::Order, ops::Pow, Integer, float::Round};

    #[cfg(feature = "future_snark")]
    use std::cmp::Ordering;

    #[cfg(feature = "future_snark")]
    pub fn compute_target_bytes(phi_f: f64, stake: Stake, total_stake: Stake) -> Vec<u8> {
        // JubjubBase modulus
        let modulus = Integer::from_str_radix(
            "73eda753299d7d483339d80809a1d80553bda402fffe5bfeffffffff00000001",
            16,
        )
        .unwrap();

        let w = Float::with_val(117, stake) / Float::with_val(117, total_stake);
        let phi = Float::with_val(117, 1.0) - Float::with_val(117, 1.0 - phi_f).pow(w.clone());

        // increase precision
        let phi_high = Float::with_val(300, phi.clone());
        let t = modulus * phi_high;

        let (t_int, order) = t.to_integer_round(Round::Zero).unwrap();
        assert!(t_int >= 0);

        let target: Integer = match order {
            Ordering::Less => t_int,
            Ordering::Equal => {
                if t_int == 0 { t_int }
                else { t_int - 1 }
            }
            Ordering::Greater => unreachable!(),
        };
        target.to_digits(Order::LsfLe)
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
/// The probability formula follows: `phi = 1 - (1 - phi_f)^(stake/total_stake)`
/// where `phi_f` is the base probability parameter.
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

    use proptest::prelude::*;
    use rand_core::OsRng;

    use crate::LotteryTargetValue;
    #[cfg(feature = "future_snark")]
    use crate::{SchnorrSigningKey, signature_scheme::BaseFieldElement};

    #[cfg(feature = "future_snark")]
    use super::{check_index, compute_target_bytes, compute_target_value, lottery_prefix};

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
                println!("{:?}", target);
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

    #[cfg(feature = "future_snark")]
    #[cfg(any(feature = "num-integer-backend", target_family = "wasm", windows))]
    #[test]
    fn test_following_stake() {
        let phi_f = 0.05;
        let total_stake = 1_000_000_000;

        let mut prev_target = compute_target_value(phi_f, 99_999, total_stake);
        for stake in 100_000..100_100 {
            let target = compute_target_value(phi_f, stake, total_stake);
            assert!(prev_target < target);
            prev_target = target;
        }
    }

    #[cfg(feature = "future_snark")]
    #[cfg(any(feature = "num-integer-backend", target_family = "wasm", windows))]
    #[test]
    fn test_greatest_stake_difference() {
        let phi_f = 0.05;
        let total_stake = 45_000_000_000;

        let first_target = compute_target_value(phi_f, 1, total_stake);
        for _ in 0..100 {
            let target = compute_target_value(phi_f, 1, total_stake);
            assert!(first_target == target);
        }
    }

    #[cfg(feature = "future_snark")]
    #[cfg(any(feature = "num-integer-backend", target_family = "wasm", windows))]
    #[test]
    fn test_minimal_stake_difference() {
        let phi_f = 0.05;
        let total_stake = 45_000_000_000;
        for _ in 0..100 {
            let zero_target = compute_target_value(phi_f, 0, total_stake);
            let first_target = compute_target_value(phi_f, 1, total_stake);
            assert!(zero_target < first_target);
            let second_target = compute_target_value(phi_f, 2, total_stake);
            assert!(first_target < second_target);
        }
    }

    #[cfg(any(feature = "num-integer-backend", target_family = "wasm", windows))]
    #[test]
    fn test_zero_stake() {
        let phi_f = 0.05;
        let total_stake = 45_000_000_000;

        for _ in 0..100 {
            use ff::Field;
            use midnight_curves::Fq;

            let target = compute_target_value(phi_f, 0, total_stake);
            assert!(target == BaseFieldElement(Fq::ZERO));
        }
    }

    #[cfg(any(feature = "num-integer-backend", target_family = "wasm", windows))]
    #[test]
    fn test_full_stake() {
        let phi_f = 0.05;
        let total_stake = 45_000_000_000;

        for _ in 0..100 {
            let target = compute_target_value(phi_f, total_stake, total_stake);
            println!("{:?}", target);
        }
    }

    proptest! {
        #![proptest_config(ProptestConfig::with_cases(50))]

        #[test]
        #[cfg(any(feature = "num-integer-backend", target_family = "wasm", windows))]
        #[cfg(feature = "future_snark")]
        fn following_stake_same_order(
            phi_f in 0.01..0.5f64,
            total_stake in 100_000_000..1_000_000_000u64,
            stake in 10_000_000..50_000_000u64,
        ) {
            let base_target = compute_target_value(phi_f, stake, total_stake);
            let next_target = compute_target_value(phi_f, stake + 1, total_stake);

            assert!(base_target < next_target);
        }

        #[test]
        #[cfg(any(feature = "num-integer-backend", target_family = "wasm", windows))]
        #[cfg(feature = "future_snark")]
        fn following_small_stake_same_order(
            phi_f in 0.01..0.5f64,
            total_stake in 100_000_000..1_000_000_000u64,
            stake in 100_000..500_000u64,
        ) {
            let base_target = compute_target_value(phi_f, stake, total_stake);
            let next_target = compute_target_value(phi_f, stake + 1, total_stake);

            assert!(base_target < next_target);
        }

        #[test]
        #[cfg(any(feature = "num-integer-backend", target_family = "wasm", windows))]
        #[cfg(feature = "future_snark")]
        fn same_stake_same_result(
            phi_f in 0.01..0.5f64,
            total_stake in 100_000_000..1_000_000_000u64,
            stake in 10_000_000..50_000_000u64,
        ) {
            let target = compute_target_value(phi_f, stake, total_stake);
            let same_target = compute_target_value(phi_f, stake, total_stake);

            assert!(target == same_target);
        }

        #[test]
        #[cfg(any(feature = "num-integer-backend", target_family = "wasm", windows))]
        #[cfg(feature = "future_snark")]
        fn same_small_stake_same_result(
            phi_f in 0.01..0.5f64,
            total_stake in 100_000_000..1_000_000_000u64,
            stake in 100_000..500_000u64,
        ) {
            let target = compute_target_value(phi_f, stake, total_stake);
            let same_target = compute_target_value(phi_f, stake, total_stake);

            assert!(target == same_target);
        }

    }

    #[cfg(feature = "future_snark")]
    #[allow(dead_code)]
    mod golden {

        use super::*;

        const GOLDEN_BYTES: [u8; 32] = [
            148, 238, 112, 99, 72, 221, 111, 145, 176, 46, 27, 63, 63, 67, 168, 136, 178, 99, 191,
            162, 138, 191, 134, 41, 213, 234, 22, 52, 148, 172, 129, 7,
        ];

        const GOLDEN_VECTOR_BYTES: [[u8; 32]; 20] = [
            [
                148, 238, 112, 99, 72, 221, 111, 145, 176, 46, 27, 63, 63, 67, 168, 136, 178, 99,
                191, 162, 138, 191, 134, 41, 213, 234, 22, 52, 148, 172, 129, 7,
            ],
            [
                233, 21, 27, 137, 13, 104, 201, 190, 31, 24, 237, 214, 185, 44, 180, 245, 13, 165,
                176, 218, 72, 187, 87, 45, 22, 222, 116, 119, 111, 138, 191, 7,
            ],
            [
                4, 231, 227, 187, 23, 20, 119, 1, 139, 115, 114, 67, 253, 178, 78, 107, 73, 9, 33,
                181, 8, 194, 249, 95, 158, 213, 18, 123, 253, 68, 253, 7,
            ],
            [
                109, 93, 190, 129, 115, 224, 79, 214, 34, 55, 164, 243, 10, 174, 128, 120, 13, 81,
                232, 11, 4, 221, 93, 45, 146, 223, 176, 99, 82, 220, 58, 8,
            ],
            [
                120, 148, 65, 138, 176, 179, 129, 224, 33, 31, 137, 207, 125, 246, 201, 4, 1, 213,
                133, 24, 23, 193, 191, 106, 1, 139, 144, 74, 130, 80, 120, 8,
            ],
            [
                76, 206, 156, 146, 183, 204, 192, 160, 168, 189, 98, 41, 29, 69, 207, 209, 68, 101,
                126, 109, 47, 254, 144, 96, 236, 118, 123, 61, 161, 161, 181, 8,
            ],
            [
                173, 167, 200, 232, 201, 127, 75, 97, 172, 250, 14, 190, 75, 66, 99, 211, 211, 227,
                192, 242, 118, 41, 97, 195, 138, 221, 201, 62, 195, 207, 242, 8,
            ],
            [
                18, 84, 26, 107, 215, 191, 118, 255, 199, 20, 173, 88, 59, 191, 186, 246, 213, 209,
                11, 122, 230, 211, 113, 190, 214, 27, 105, 69, 252, 218, 47, 9,
            ],
            [
                34, 180, 163, 222, 62, 208, 248, 25, 255, 195, 76, 186, 212, 103, 61, 204, 198,
                109, 154, 67, 127, 45, 109, 49, 94, 53, 226, 60, 96, 195, 108, 9,
            ],
            [
                213, 142, 74, 65, 218, 150, 120, 128, 185, 111, 216, 178, 249, 1, 191, 246, 54, 6,
                230, 12, 254, 245, 126, 65, 92, 84, 96, 5, 3, 137, 169, 9,
            ],
            [
                246, 24, 65, 141, 23, 205, 200, 22, 233, 237, 118, 172, 124, 133, 169, 241, 135,
                114, 188, 113, 7, 206, 216, 92, 29, 70, 183, 115, 248, 43, 230, 9,
            ],
            [
                203, 186, 215, 179, 8, 108, 146, 89, 63, 241, 63, 156, 242, 80, 201, 161, 172, 162,
                248, 43, 47, 31, 117, 206, 175, 243, 105, 81, 84, 172, 34, 10,
            ],
            [
                154, 104, 6, 6, 28, 199, 79, 121, 143, 91, 228, 142, 132, 172, 198, 168, 220, 168,
                74, 250, 108, 165, 182, 254, 226, 214, 176, 92, 42, 10, 95, 10,
            ],
            [
                153, 241, 128, 114, 9, 226, 92, 211, 45, 175, 46, 106, 166, 147, 147, 23, 59, 109,
                223, 198, 45, 41, 78, 124, 151, 107, 128, 72, 142, 69, 155, 10,
            ],
            [
                85, 63, 254, 33, 188, 112, 141, 149, 128, 237, 190, 112, 199, 191, 51, 29, 224,
                219, 31, 97, 209, 45, 156, 231, 97, 157, 143, 188, 147, 94, 215, 10,
            ],
            [
                131, 219, 241, 199, 92, 108, 1, 28, 202, 67, 46, 24, 155, 1, 243, 183, 72, 11, 92,
                81, 168, 60, 145, 217, 130, 49, 94, 85, 78, 85, 19, 11,
            ],
            [
                145, 133, 1, 46, 45, 76, 15, 247, 209, 13, 46, 165, 135, 82, 107, 94, 106, 232,
                138, 194, 5, 8, 221, 221, 53, 45, 59, 164, 209, 41, 79, 11,
            ],
            [
                3, 149, 140, 147, 172, 185, 98, 189, 222, 182, 187, 13, 85, 224, 124, 149, 117, 50,
                2, 55, 83, 228, 10, 151, 89, 56, 75, 47, 49, 220, 138, 11,
            ],
            [
                101, 57, 80, 96, 145, 41, 229, 158, 88, 163, 117, 147, 221, 36, 203, 104, 161, 165,
                238, 243, 226, 5, 249, 33, 115, 252, 142, 113, 128, 108, 198, 11,
            ],
            [
                233, 239, 235, 236, 174, 118, 1, 10, 113, 133, 116, 112, 156, 16, 105, 234, 111,
                53, 11, 56, 31, 145, 231, 204, 14, 128, 233, 218, 210, 218, 1, 12,
            ],
        ];

        #[cfg(any(feature = "num-integer-backend", target_family = "wasm", windows))]
        fn golden_value_num_int() -> LotteryTargetValue {
            let phi_f = 0.05;
            let stake = 30;
            let total_stake = 100;

            compute_target_value(phi_f, stake, total_stake)
        }

        #[cfg(not(any(target_family = "wasm", target_env = "musl", windows)))]
        fn golden_value_rug() -> LotteryTargetValue {
            let phi_f = 0.05;
            let stake = 30;
            let total_stake = 100;

            compute_target_value(phi_f, stake, total_stake)
        }

        fn golden_vector() -> Vec<LotteryTargetValue> {
            let phi_f = 0.2;
            let total_stake = 100;
            (30..50)
                .map(|stake| compute_target_value(phi_f, stake, total_stake))
                .collect()
        }

        #[cfg(any(feature = "num-integer-backend", target_family = "wasm", windows))]
        // #[test]
        fn golden_check_num_int() {
            let _golden_target = BaseFieldElement::from_bytes(&GOLDEN_BYTES).unwrap();
            println!("{:?}", golden_value_num_int());
            // assert_eq!(golden_target, golden_value_num_int());
        }

        #[cfg(not(any(target_family = "wasm", target_env = "musl", windows)))]
        // #[test]
        fn golden_check_rug() {
            let _golden_target = BaseFieldElement::from_bytes(&GOLDEN_BYTES).unwrap();
            println!("{:?}", golden_value_rug());

            // assert_eq!(golden_target, golden_value_rug());
        }
        // #[test]
        fn golden_vector_check() {
            let golden_vector = golden_vector();

            for (golden_bytes, golden_target) in GOLDEN_VECTOR_BYTES.iter().zip(golden_vector) {
                let target = BaseFieldElement::from_bytes(golden_bytes).unwrap();
                assert_eq!(target, golden_target);
            }
        }
    }
}
