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
        kadv = kadv + 1.0;
        let binom = Binomial::new(prob_adv, m as u64).unwrap();
        let logsf = 1.0 - binom.cdf(kadv as u64 - 1);
        secadv = -logsf.ln() / LN_2;
    }
    kadv = kadv - 1.0;
    // let binom = Binomial::new(prob_adv, m as u64).unwrap();
    // let logsf = 1.0 - binom.cdf(kadv as u64 - 1);
    // secadv = -logsf.ln() / LN_2;
    // println!("Adversarial kmax = {}, sec = {}",kadv, secadv);

    let binom = Binomial::new(prob_hon, m as u64).unwrap();
    let logcdf = binom.cdf(khon as u64).ln();
    let mut relhon = -logcdf / LN_2;

    while relhon < completeness_param {
        khon = khon - 1.0;
        let binom = Binomial::new(prob_hon, m as u64).unwrap();
        let logcdf = binom.cdf(khon as u64).ln();
        relhon = -logcdf / LN_2;
    }
    khon = khon + 1.0;
    // let binom = Binomial::new(prob_hon, m as u64).unwrap();
    // let logcdf = binom.cdf(khon as u64).ln();
    // relhon = -logcdf / LN_2;

    return (m as u64, kadv as u64, khon as u64);

    // println!("Honest kmin= {}, rel = {:.4}", khon, relhon);
    // println!("===================================");
    // println!("For {} bit security, and {} bit reliability for {} forging and {} participating.", SEP - 1.0, SEP_REL -
    //     1.0,
    //          adv, hon);
    // println!("Set mithril nodes to phi: {:.2}, m= {}", 1.0 - f, m);
    // println!("K value can be any between: K_adv = Nf = {}, K_hon = Np = {}", kadv, khon);
    // println!("Set ALBA prover for Np/Nf ratio= {:.4}, {} bit sec., and {} bit rel.", khon as f64 / kadv as f64, SEP -
    //     1.0, SEP_REL - 1.0);
    // println!("===================================");
}
