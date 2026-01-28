cfg_num_integer! {
    use num_bigint::BigInt;
    use num_rational::Ratio;
    use std::ops::Neg;

    #[cfg(feature = "future_snark")]
    // TODO: remove this allow dead_code directive when function is called or future_snark is activated
    #[allow(dead_code)]
    pub fn compute_target_bytes(phi_f: f64, stake: Stake, total_stake: Stake) -> Vec<u8> {
        use num_integer::Integer;
        use num_traits::{Zero, Num};

        let modulus = BigInt::from_str_radix(
            "73eda753299d7d483339d80809a1d80553bda402fffe5bfeffffffff00000001",
            16,
        )
        .unwrap();

        let c =
            Ratio::from_float((1.0 - phi_f).ln()).expect("Only fails if the float is infinite or NaN.");
        let w = Ratio::new_raw(BigInt::from(stake), BigInt::from(total_stake));

        let phi = (w * c).neg();
        let t = Ratio::from(modulus) * phi;

        // Floor division
        let (t_int, remainder) = t.numer().div_rem(t.denom());
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
    use rug::{Float, integer::Order, ops::Pow};

    #[cfg(feature = "future_snark")]
    pub fn compute_target_bytes(phi_f: f64, stake: Stake, total_stake: Stake) -> Vec<u8> {
        // JubjubBase modulus
        let modulus = Integer::from_str_radix(
            "73eda753299d7d483339d80809a1d80553bda402fffe5bfeffffffff00000001",
            16,
        )
        .unwrap();

        let w = Float::with_val(117, stake) / Float::with_val(117, total_stake);
        let phi = Float::with_val(117, 1.0) - Float::with_val(117, 1.0 - phi_f).pow(w);
        // increase precision
        let phi_high = Float::with_val(300, phi);

        let t = modulus * phi_high;
        let (t_int, order) = t.to_integer_round(Round::Zero).unwrap();
        assert!(t_int >= 0);

        let target = match order {
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
use crate::{LotteryTargetValue, Stake, signature_scheme::BaseFieldElement};

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

#[cfg(test)]
mod tests {
    #[cfg(feature = "future_snark")]
    use super::compute_target_value;
    #[cfg(feature = "future_snark")]
    use crate::signature_scheme::BaseFieldElement;

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
}
