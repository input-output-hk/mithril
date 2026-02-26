use midnight_circuits::{hash::poseidon::PoseidonChip, instructions::hash::HashCPU};
use midnight_curves::Fq as JubjubBase;

use crate::signature_scheme::{
    BaseFieldElement, DOMAIN_SEPARATION_TAG_LOTTERY, DOMAIN_SEPARATION_TAG_SIGNATURE,
    compute_poseidon_digest,
};

#[test]
fn signature_and_lottery_domain_tags_do_not_collide() {
    assert_ne!(
        DOMAIN_SEPARATION_TAG_SIGNATURE, DOMAIN_SEPARATION_TAG_LOTTERY.0,
        "signature and lottery domain separation tags must be distinct"
    );
}

#[test]
fn signature_digest_matches_hard_coded_signature_domain_tag_formula() {
    let signature_transcript_inputs = vec![
        BaseFieldElement::from(11),
        BaseFieldElement::from(22),
        BaseFieldElement::from(33),
        BaseFieldElement::from(44),
    ];

    let signature_digest_via_stm = compute_poseidon_digest(&signature_transcript_inputs);

    let hard_coded_signature_domain_tag = JubjubBase::from_raw([0x5349_474E_5F44_5354, 0, 0, 0]);
    let mut signature_digest_manual_inputs = vec![hard_coded_signature_domain_tag];
    signature_digest_manual_inputs.extend(signature_transcript_inputs.iter().map(|value| value.0));

    let signature_digest_via_hard_coded_formula = BaseFieldElement(
        PoseidonChip::<JubjubBase>::hash(&signature_digest_manual_inputs),
    );

    assert_eq!(
        signature_digest_via_stm, signature_digest_via_hard_coded_formula,
        "signature digest computed via STM helper must match hard-coded signature-domain formula"
    );
}

#[test]
fn lottery_prefix_matches_hard_coded_lottery_domain_tag_formula() {
    let merkle_root = JubjubBase::from(123u64);
    let msg = JubjubBase::from(456u64);

    let lottery_prefix_via_stm_constant =
        PoseidonChip::<JubjubBase>::hash(&[DOMAIN_SEPARATION_TAG_LOTTERY.0, merkle_root, msg]);

    let hard_coded_lottery_domain_tag = JubjubBase::from_raw([3, 3, 0, 0]);
    let lottery_prefix_via_hard_coded_formula =
        PoseidonChip::<JubjubBase>::hash(&[hard_coded_lottery_domain_tag, merkle_root, msg]);

    assert_eq!(
        BaseFieldElement(lottery_prefix_via_stm_constant),
        BaseFieldElement(lottery_prefix_via_hard_coded_formula),
        "lottery prefix must use the lottery domain separation tag as first Poseidon input"
    );
}

#[test]
#[should_panic(expected = "circuit lottery prefix is expected to differ between circuit and stm")]
fn lottery_prefix_circuit_vs_stm_helper() {
    let merkle_root = JubjubBase::from(123u64);
    let msg = JubjubBase::from(456u64);

    let lottery_prefix_via_circuit_formula =
        PoseidonChip::<JubjubBase>::hash(&[DOMAIN_SEPARATION_TAG_LOTTERY.0, merkle_root, msg]);

    let lottery_prefix_via_stm = compute_poseidon_digest(&[
        DOMAIN_SEPARATION_TAG_LOTTERY,
        BaseFieldElement(merkle_root),
        BaseFieldElement(msg),
    ]);

    assert_eq!(
        BaseFieldElement(lottery_prefix_via_circuit_formula),
        lottery_prefix_via_stm,
        "circuit lottery prefix is expected to differ between circuit and stm until mismatch is fixed"
    );
}
