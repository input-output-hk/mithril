use serde::{Deserialize, Serialize};

use crate::error::RegisterError;

/// Used to set protocol parameters.
// todo: this is the criteria to consider parameters valid:
// Let A = max assumed adversarial stake
// Let a = A / max_stake
// Let p = φ(a)  // f needs tuning, something close to 0.2 is reasonable
// Then, we're secure if SUM[from i=k to i=m] Binomial(i successes, m experiments, p chance of success) <= 2^-100 or thereabouts.
// The latter turns to 1 - BinomialCDF(k-1,m,p)
#[derive(Debug, Clone, Copy, PartialEq, Serialize, Deserialize)]
pub struct Parameters {
    /// Security parameter, upper bound on indices.
    pub m: u64,
    /// Quorum parameter.
    pub k: u64,
    /// `f` in phi(w) = 1 - (1 - f)^w, where w is the stake of a participant..
    pub phi_f: f64,
}

impl Parameters {
    /// Convert to bytes
    /// # Layout
    /// * Security parameter, `m` (as u64)
    /// * Quorum parameter, `k` (as u64)
    /// * Phi f, as (f64)
    pub fn to_bytes(&self) -> [u8; 24] {
        let mut out = [0; 24];
        out[..8].copy_from_slice(&self.m.to_be_bytes());
        out[8..16].copy_from_slice(&self.k.to_be_bytes());
        out[16..].copy_from_slice(&self.phi_f.to_be_bytes());
        out
    }

    /// Extract the `Parameters` from a byte slice.
    /// # Error
    /// The function fails if the given string of bytes is not of required size.
    pub fn from_bytes(bytes: &[u8]) -> Result<Self, RegisterError> {
        let mut u64_bytes = [0u8; 8];
        u64_bytes.copy_from_slice(bytes.get(..8).ok_or(RegisterError::SerializationError)?);
        let m = u64::from_be_bytes(u64_bytes);
        u64_bytes.copy_from_slice(bytes.get(8..16).ok_or(RegisterError::SerializationError)?);
        let k = u64::from_be_bytes(u64_bytes);
        u64_bytes.copy_from_slice(bytes.get(16..24).ok_or(RegisterError::SerializationError)?);
        let phi_f = f64::from_be_bytes(u64_bytes);

        Ok(Self { m, k, phi_f })
    }
}

#[cfg(test)]
mod test {
    use crate::Parameters;

    const GOLDEN_PARAMETERS_JSON: &str = r#"
    {
        "m": 20973,
        "k": 2422,
        "phi_f": 0.2
    }
    "#;

    #[test]
    fn golden_parameters_deserialization() {
        let _: Parameters = serde_json::from_str(GOLDEN_PARAMETERS_JSON)
            .expect("Deserializing Parameters should not fail");
    }

    #[test]
    fn test_parameters_conversions() {
        let parameters_json = GOLDEN_PARAMETERS_JSON;

        let parameters_from_json: Parameters = serde_json::from_str(parameters_json)
            .expect("Deserializing Parameters should not fail");
        let parameters_from_json_to_json = serde_json::to_string(&parameters_from_json)
            .expect("Serializing Parameters to json should not fail");

        let parameters_from_bytes = Parameters::from_bytes(&parameters_from_json.to_bytes())
            .expect("Deserializing Parameters from bytes should not fail");
        let parameters_from_bytes_to_json = serde_json::to_string(&parameters_from_bytes)
            .expect("Serializing Parameters to json should not fail");

        assert_eq!(parameters_from_json_to_json, parameters_from_bytes_to_json);

        let mut parameters_from_json = parameters_from_json;
        parameters_from_json.phi_f = 0.0;

        let parameters_from_bytes = Parameters::from_bytes(&parameters_from_json.to_bytes())
            .expect("Deserializing Parameters from bytes should not fail");
        assert_eq!(0.0, parameters_from_bytes.phi_f);
    }
}
