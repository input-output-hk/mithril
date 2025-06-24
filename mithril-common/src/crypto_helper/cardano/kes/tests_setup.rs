use std::{fs, path::PathBuf};

use kes_summed_ed25519::kes::Sum6Kes;
use kes_summed_ed25519::traits::KesSk;

use crate::crypto_helper::{cardano::ColdKeyGenerator, OpCert};
use crate::crypto_helper::{KESPeriod, ProtocolPartyId, SerDeShelleyFileFormat, Sum6KesBytes};

/// Create KES cryptographic material for testing purposes.
#[allow(dead_code)]
pub(crate) fn create_kes_cryptographic_material(
    party_idx: u64,
    kes_period: KESPeriod,
    test_directory: &str,
) -> (ProtocolPartyId, PathBuf, PathBuf) {
    let temp_dir = std::env::temp_dir()
        .join("create_kes_cryptographic_material")
        .join(format!("{test_directory}_{party_idx}"));
    fs::create_dir_all(&temp_dir).expect("temp dir creation should not fail");
    let keypair = ColdKeyGenerator::create_deterministic_keypair([party_idx as u8; 32]);
    let mut dummy_buffer = [0u8; Sum6Kes::SIZE + 4];
    let mut dummy_seed = [party_idx as u8; 32];
    let (mut kes_secret_key, kes_verification_key) =
        Sum6Kes::keygen(&mut dummy_buffer, &mut dummy_seed);
    for _ in 0..kes_period {
        kes_secret_key
            .update()
            .expect("KES secret key update should not fail");
    }
    let mut kes_bytes = Sum6KesBytes([0u8; Sum6Kes::SIZE + 4]);
    kes_bytes.0.copy_from_slice(&kes_secret_key.clone_sk());
    let operational_certificate = OpCert::new(kes_verification_key, 0, 0, keypair);
    let kes_secret_key_file = temp_dir.join(format!("kes{party_idx}.skey"));
    kes_bytes
        .to_file(&kes_secret_key_file)
        .expect("KES secret key file export should not fail");
    let operational_certificate_file = temp_dir.join(format!("pool{party_idx}.cert"));
    operational_certificate
        .to_file(&operational_certificate_file)
        .expect("operational certificate file export should not fail");
    let party_id = operational_certificate
        .compute_protocol_party_id()
        .expect("compute protocol party id should not fail");

    (party_id, operational_certificate_file, kes_secret_key_file)
}
