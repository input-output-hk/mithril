use rug::{Float, Integer, float::Round, integer::Order, ops::Pow};
use std::cmp::Ordering;

#[cfg(feature = "future_snark")]
use crate::{
    LotteryIndex, LotteryTargetValue, Stake, StmResult,
    signature_scheme::{
        BaseFieldElement, DST_LOTTERY, UniqueSchnorrSignature, compute_poseidon_digest,
    },
};

#[cfg(feature = "future_snark")]
// TODO: remove this allow dead_code directive when function is called or future_snark is activated
#[allow(dead_code)]
/// Target value type used in the lottery for snark proof system
use super::error::LotteryError;

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
///
/// If `phi_f == 1.0`, returns the maximum possible target value (field modulus - 1).
///
/// Returns `Ok(TargetValue)` representing the computed threshold, or an error if:
/// - `phi_f` is outside the valid range `[0.0, 1.0]`
/// - Numeric conversion fails during computation
pub fn compute_target_value(
    phi_f: f64,
    stake: Stake,
    total_stake: Stake,
) -> StmResult<LotteryTargetValue> {
    // modulus - 1
    let ev_max = -BaseFieldElement::get_one();
    if !(0.0..=1.0).contains(&phi_f) {
        return Err(LotteryError::InvalidPhiValue(phi_f).into());
    }

    // If phi_f = 1, then we automatically break with true
    if (phi_f - 1.0).abs() < f64::EPSILON {
        return Ok(ev_max);
    }

    // JubjubBase modulus
    let modulus = Integer::from_str_radix(
        "73eda753299d7d483339d80809a1d80553bda402fffe5bfeffffffff00000001",
        16,
    )
    .map_err(|_| LotteryError::BaseFieldElementConversion)?;

    let w = Float::with_val(117, stake) / Float::with_val(117, total_stake);
    let phi = Float::with_val(117, 1.0) - Float::with_val(117, 1.0 - phi_f).pow(w);
    // increase precision
    let phi_high = Float::with_val(300, phi);

    let t_float = modulus * phi_high;
    let (t_integer, order) = t_float
        .to_integer_round(Round::Zero)
        .ok_or(LotteryError::FloatToIntegerConversion)?;

    if t_integer < 0 {
        return Err(LotteryError::InvalidIntegerValue(t_integer.to_u64_wrapping()).into());
    }

    let target = match order {
        Ordering::Less => t_integer,
        Ordering::Equal => {
            if t_integer == 0 {
                // hashing to 0 has negligible probability
                t_integer
            } else {
                t_integer - 1
            }
        }
        Ordering::Greater => unreachable!(),
    };

    let mut bytes: Vec<u8> = target.to_digits(Order::LsfLe);
    bytes.resize(32, 0);

    BaseFieldElement::from_bytes(&bytes)
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
) -> Result<(), LotteryError> {
    if index > m {
        return Err(LotteryError::IndexBoundFailed(index, m));
    }

    let idx = BaseFieldElement::from(index);
    let (sigma_x, sigma_y) = signature.commitment_point.get_coordinates();
    let ev = compute_poseidon_digest(&[prefix, sigma_x, sigma_y, idx]);

    // check if ev <= target
    if ev > target {
        return Err(LotteryError::SnarkLotteryVerification);
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use rand_core::OsRng;

    use crate::SchnorrSigningKey;

    use super::*;

    #[test]
    fn test_target_phi_f_one() {
        // Case where `phi_f` is exactly 1
        let phi_f = 1.0;
        let stake = 50;
        let total_stake = 100;

        let result = compute_target_value(phi_f, stake, total_stake).unwrap();

        // Since `phi_f` is 1, the function should return the maximum target (-F::ONE)
        assert_eq!(result, -BaseFieldElement::get_one());
    }

    #[test]
    fn test_target_half_stake() {
        // Case where `stake` is exactly half of `total_stake`
        let phi_f = 0.5;
        let stake = 50;
        let total_stake = 100;

        let result = compute_target_value(phi_f, stake, total_stake).unwrap();

        // Validate that result is in the expected range
        assert!(result != -BaseFieldElement::get_one());
    }

    #[test]
    fn test_check_index() {
        let phi_f = 0.2;
        let stake = 30;
        let total_stake = 100;

        let target = compute_target_value(phi_f, stake, total_stake).unwrap();
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
}
