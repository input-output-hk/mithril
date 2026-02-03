cfg_num_integer! {
    use num_bigint::BigInt;
    use num_rational::Ratio;
    use num_rational::BigRational;
    use std::ops::Neg;

    #[cfg(feature = "future_snark")]
    fn compute_phi_fixed(x: Ratio<BigInt>, w: Ratio<BigInt>, iterations: usize) -> Ratio<BigInt> {
        let mut phi = Ratio::zero();
        let mut term = Ratio::one();

        for n in 1..iterations {
            // Calculate the next coefficient: term *= (k - (n-1)) / n
            let multiplier = (&w - Ratio::from(BigInt::from(n - 1))) / Ratio::from(BigInt::from(n));
            term = term * multiplier * &x;

            if n % 2 == 1 {
                phi += &term;
            } else {
                phi -= &term;
            }
        }
        phi
    }

    #[cfg(feature = "future_snark")]
    fn compute_phi(x: BigRational, a: BigInt, b: BigInt, precision_bits: usize) -> BigRational {
        let n = BigRational::new(a, b);
        let mut phi = BigRational::zero();
        let mut term = n.clone() * &x; // First term: nx
        let mut k = 1;

        // We stop when the current term is so small it doesn't affect the target precision
        // Or set a fixed high iteration count.
        let threshold = BigRational::new(BigInt::one(), BigInt::one() << precision_bits);

        while term.abs() > threshold && k < 100 {
            if k % 2 == 1 {
                phi += &term;
            } else {
                phi -= &term;
            }

            // Compute next term iteratively to save cycles:
            // term_{k+1} = term_k * (x) * (n - k) / (k + 1)
            let n_minus_k = &n - BigRational::from_integer(BigInt::from(k));
            let k_plus_1 = BigRational::from_integer(BigInt::from(k + 1));

            term = term * &x * n_minus_k / k_plus_1;
            k += 1;
        }

        phi
    }

    #[cfg(feature = "future_snark")]
    // TODO: remove this allow dead_code directive when function is called or future_snark is activated
    #[allow(dead_code)]
    pub fn compute_target_bytes(phi_f: f64, stake: Stake, total_stake: Stake) -> Vec<u8> {
        use num_integer::Integer;
        use num_traits::{Zero, Num};

        let modulus = BigInt::from_str_radix(
            "73eda753299d7d483339d80809a1d80553bda402fffe5bfeffffffff00000001",
            // "1",
            16,
        )
        .unwrap();

        // let modulus = BigInt::from(1u64<<16);

        println!("modulus = {:?}", modulus);

        // OLD way
        // let c =
        //     Ratio::from_float((1.0 - phi_f.clone()).ln()).expect("Only fails if the float is infinite or NaN.");
        // let phi = (w.clone() * c.clone()).neg();
        // let t = Ratio::from(modulus.clone()) * phi.clone();
        // println!("t = {:?}", t);
        // println!("c = {:?}", c);
        // println!("w = {:?}", w);
        // println!("phi = {:?}", phi);


        // With Taylor series 1
        let phi_f_ratio = Ratio::new_raw(BigInt::from(1), BigInt::from(5));
        let w = Ratio::new_raw(BigInt::from(stake), BigInt::from(total_stake));

        // let phi_taylor1 = compute_phi(phi_f_ratio.clone(), BigInt::from(stake), BigInt::from(total_stake), 117);
        // let t_taylor1 = (phi_taylor1.numer() * modulus.clone()) / phi_taylor1.denom();
        // println!("t_taylor1 = {:?}", t_taylor1);
        // println!("phi_taylor1 = {:?}", phi_taylor1);

        // With Taylor series 2
        let phi_taylor = compute_phi_fixed(phi_f_ratio.clone(), w.clone(), 200);
        // let t_taylor = (phi_taylor.numer() * modulus.clone()) / phi_taylor.denom();
        let t_taylor =  Ratio::from(modulus.clone()) * phi_taylor.clone();
        println!("t_taylor = {:?}", t_taylor);
        println!("phi_taylor = {:?}", phi_taylor);


        // With Basic f64
        // let phi_test = 1.0 - (1.0 - phi_f).powf(stake as f64/total_stake as f64);
        // println!("phi_test = {:?}", phi_test);
        // let t_test = phi_test * ((1u64<<16) as f64);
        // println!("t_test = {:?}", t_test);



        // Floor division
        let (t_int, remainder) = t_taylor.numer().div_rem(t_taylor.denom());
        assert!(t_int >= BigInt::zero());
        println!("t_int = {:?}", t_int);

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
            // "1",
            16,
        )
        .unwrap();

        // let modulus = Integer::from(1u64<<16);

        println!("modulus = {:?}", modulus);

        let w = Float::with_val(117, stake) / Float::with_val(117, total_stake);
        let phi = Float::with_val(117, 1.0) - Float::with_val(117, 1.0 - phi_f).pow(w.clone());

        // increase precision
        let phi_high = Float::with_val(300, phi.clone());
        println!("phi_high = {:?}", phi_high);

        let t = modulus * phi_high;
        println!("t_rug = {:?}", t);

        let (t_int, order) = t.to_integer_round(Round::Zero).unwrap();
        assert!(t_int >= 0);
        println!("t_int = {:?}", t_int);

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

use num_traits::{One, Signed, Zero};

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
    use super::{check_index, compute_target_value, lottery_prefix};

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
    #[test]
    fn test_following_stake() {
        let phi_f = 0.2;
        let total_stake = 10000000;

        let mut prev_target = compute_target_value(phi_f, 99, total_stake);
        for stake in 1000000..1000100 {
            let target = compute_target_value(phi_f, stake, total_stake);
            assert!(prev_target < target);
            prev_target = target;
        }
    }

    proptest! {
        #![proptest_config(ProptestConfig::with_cases(50))]

        #[test]
        /// Checking that following stakes always have the same order.
        fn following_stake_same_order(
            phi_f in 0.01..0.5f64,
            total_stake in 100_000_000..1_000_000_000u64,
            stake in 10_000_000..50_000_000u64,
        ) {
            let base_target = compute_target_value(phi_f, stake, total_stake);
            let next_target = compute_target_value(phi_f, stake + 1, total_stake);

            assert!(base_target < next_target);
        }

    }

    #[cfg(feature = "future_snark")]
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
            let phi_f = 0.2;
            let stake = 30;
            let total_stake = 100;

            compute_target_value(phi_f, stake, total_stake)
        }

        #[cfg(not(any(target_family = "wasm", target_env = "musl", windows)))]
        fn golden_value_rug() -> LotteryTargetValue {
            let phi_f = 0.2;
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
        #[test]
        fn golden_check_num_int() {
            let _golden_target = BaseFieldElement::from_bytes(&GOLDEN_BYTES).unwrap();
            println!("{:?}", golden_value_num_int());
            // assert_eq!(golden_target, golden_value_num_int());
        }

        #[cfg(not(any(target_family = "wasm", target_env = "musl", windows)))]
        #[test]
        fn golden_check_rug() {
            let _golden_target = BaseFieldElement::from_bytes(&GOLDEN_BYTES).unwrap();
            println!("{:?}", golden_value_rug());

            // assert_eq!(golden_target, golden_value_rug());
        }
        #[test]
        fn golden_vector_check() {
            let golden_vector = golden_vector();

            for (golden_bytes, golden_target) in GOLDEN_VECTOR_BYTES.iter().zip(golden_vector) {
                let target = BaseFieldElement::from_bytes(golden_bytes).unwrap();
                assert_eq!(target, golden_target);
            }
        }
    }
}
