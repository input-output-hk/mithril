use anyhow::anyhow;
use bech32::{self, Bech32, Hrp};

use crate::StdResult;

/// Encode to bech32 given Human Readable Part (hrp) and data
pub fn encode_bech32(human_readable_part: &str, data: &[u8]) -> StdResult<String> {
    let human_readable_part = Hrp::parse(human_readable_part).map_err(|e| anyhow!(e))?;
    bech32::encode::<Bech32>(human_readable_part, data).map_err(|e| anyhow!(e))
}

#[cfg(test)]
mod tests {
    use hex::FromHex;

    use super::encode_bech32;

    #[test]
    fn test_bech32_encode() {
        let hrp = "pool";
        let data =
            Vec::from_hex("edfa208d441511f9595ba80e8f3a7b07b6a80cbc9dda9d8e9d1dc039").unwrap();
        let encoded_data = encode_bech32(hrp, &data).unwrap();
        let expected_encoded_data =
            "pool1ahazpr2yz5gljk2m4q8g7wnmq7m2sr9unhdfmr5arhqrjnntwdz".to_string();

        assert_eq!(expected_encoded_data, encoded_data);
    }
}
