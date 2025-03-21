//! Sterling parameter setup helpers

#![allow(dead_code)]
#![allow(clippy::extra_unused_type_parameters)]

use statrs::distribution::Binomial;
use statrs::distribution::DiscreteCDF;
use std::f64::consts::LN_2;

pub(crate) fn phi(w: f64, f: f64) -> f64 {
    1.0 - f.powf(w)
}

pub(crate) fn analyze(
    adv: f64,
    m: f64,
    hon: f64,
    f: f64,
    soundness_param: f64,
    completeness_param: f64,
) -> (u64, u64, u64) {
    let prob_adv = phi(adv, f);
    let prob_hon = phi(hon, f);
    let mut kadv = (m * prob_adv).round();
    let mut khon = (m * prob_hon).round();

    let binom = Binomial::new(prob_adv, m as u64).unwrap();
    let logsf = 1.0 - binom.cdf(kadv as u64 - 1);
    let mut secadv = -logsf.ln() / LN_2;

    while secadv < soundness_param {
        kadv += 1.0;
        let binom = Binomial::new(prob_adv, m as u64).unwrap();
        let logsf = 1.0 - binom.cdf(kadv as u64 - 1);
        secadv = -logsf.ln() / LN_2;
    }
    kadv -= 1.0;

    let binom = Binomial::new(prob_hon, m as u64).unwrap();
    let logcdf = binom.cdf(khon as u64).ln();
    let mut relhon = -logcdf / LN_2;

    while relhon < completeness_param {
        khon -= 1.0;
        let binom = Binomial::new(prob_hon, m as u64).unwrap();
        let logcdf = binom.cdf(khon as u64).ln();
        relhon = -logcdf / LN_2;
    }
    khon += 1.0;
    (m as u64, kadv as u64, khon as u64)
}

pub(crate) fn compute_m(adv_percentage: f64, soundness_param: f64, f: f64, constant: f64) -> f64 {
    let x = ((1.0 - adv_percentage) / adv_percentage).sqrt();
    let first_guess = x * adv_percentage;

    let prob_adv = phi(adv_percentage, f);
    let prob_tar = phi(first_guess, f);
    let overshoot = prob_tar / prob_adv;
    let a = overshoot - 1.0;
    let mu = soundness_param * (2_f64.ln() / (a * a) * (2.0 + a));
    let mut m = mu / prob_adv;
    m = m.round();
    m * constant
}

pub(crate) fn compute_k_adv(soundness_param: f64, m: f64, adv: f64, f: f64) -> u64 {
    let prob_adv = phi(adv, f);
    let mut kadv = (m * prob_adv).round();

    let binom = Binomial::new(prob_adv, m as u64).unwrap();
    let logsf = 1.0 - binom.cdf(kadv as u64 - 1);
    let mut secadv = -logsf.ln() / LN_2;

    while secadv < soundness_param {
        kadv += 1.0;
        let binom = Binomial::new(prob_adv, m as u64).unwrap();
        let logsf = 1.0 - binom.cdf(kadv as u64 - 1);
        secadv = -logsf.ln() / LN_2;
    }

    (kadv - 1.0) as u64
}

pub(crate) fn compute_k_hon(m: f64, hon: f64, f: f64, completeness_param: f64) -> u64 {
    let prob_hon = phi(hon, f);
    let mut khon = (m * prob_hon).round();

    let binom = Binomial::new(prob_hon, m as u64).unwrap();
    let logcdf = binom.cdf(khon as u64).ln();
    let mut relhon = -logcdf / LN_2;

    while relhon < completeness_param {
        khon -= 1.0;
        let binom = Binomial::new(prob_hon, m as u64).unwrap();
        let logcdf = binom.cdf(khon as u64).ln();
        relhon = -logcdf / LN_2;
    }
    (khon - 1.0) as u64
}

#[cfg(test)]
mod tests {
    use crate::stm_telescope::utils::{compute_k_adv, compute_k_hon, compute_m};

    #[test]
    fn zort() {
        let adv_percentage = 0.05;
        let hon_percentage = 0.95;
        let f = 0.9;
        let soundness_param = 128.0;
        let completeness_param = 32.0;

        let constant = 1.0;

        let m = compute_m(adv_percentage, soundness_param, f, constant);
        println!("{}", m);

        let k_adv = compute_k_adv(soundness_param, m, adv_percentage, f);
        println!("{}", k_adv);

        let k_hon = compute_k_hon(m, hon_percentage, f, completeness_param);
        println!("{}", k_hon);
    }
}
