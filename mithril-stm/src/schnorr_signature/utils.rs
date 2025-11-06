use midnight_curves::{
    Fq as JubjubBase, Fr as JubjubScalar, JubjubAffine, JubjubExtended, JubjubSubgroup,
};
use sha2::{Digest, Sha256};

use anyhow::{Result, anyhow};

/// Convert an arbitrary array of bytes into a Jubjub scalar field element
///
/// First hash the message to 256 bits use Sha256 then perform the conversion
pub fn hash_msg_to_jubjubbase(msg: &[u8]) -> Result<JubjubBase> {
    let mut hash = Sha256::new();
    hash.update(msg);
    let hmsg = hash.finalize();
    let mut output = [0u8; 32];
    if hmsg.len() == output.len() {
        output.copy_from_slice(&hmsg);
    } else {
        return Err(anyhow!(
            "Hash of the message does not have the correct lenght."
        ));
    }

    Ok(JubjubBase::from_raw([
        u64::from_le_bytes(output[0..8].try_into()?),
        u64::from_le_bytes(output[8..16].try_into()?),
        u64::from_le_bytes(output[16..24].try_into()?),
        u64::from_le_bytes(output[24..32].try_into()?),
    ]))
}

/// Extract the coordinates of a given point
///
/// This is mainly use to feed the Poseidon hash function
pub fn get_coordinates(point: JubjubSubgroup) -> (JubjubBase, JubjubBase) {
    let point_extended_representation = JubjubExtended::from(point);
    let point_affine_representation = JubjubAffine::from(point_extended_representation);
    let x_coordinate = point_affine_representation.get_u();
    let y_coordinate = point_affine_representation.get_v();

    (x_coordinate, y_coordinate)
}

/// Convert an element of the BLS12-381 base field to one of the Jubjub base field
pub fn jubjub_base_to_scalar(x: &JubjubBase) -> Result<JubjubScalar> {
    let bytes = x.to_bytes_le();

    Ok(JubjubScalar::from_raw([
        u64::from_le_bytes(bytes[0..8].try_into()?),
        u64::from_le_bytes(bytes[8..16].try_into()?),
        u64::from_le_bytes(bytes[16..24].try_into()?),
        u64::from_le_bytes(bytes[24..32].try_into()?),
    ]))
}
