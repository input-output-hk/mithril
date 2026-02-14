use mithril_common::entities::ClientError;

pub struct ProverBlockHashValidator {
    max_hashes: usize,
}

impl ProverBlockHashValidator {
    const LABEL: &'static str = "invalid_block_hashes";

    pub fn new(max_hashes: usize) -> Self {
        Self { max_hashes }
    }

    pub fn validate(&self, hashes: &[String]) -> Result<(), ClientError> {
        if hashes.len() > self.max_hashes {
            return Err(ClientError::new(
                Self::LABEL,
                format!(
                    "Block hashes list contains more than maximum allowed number of hashes: '{}'",
                    self.max_hashes
                ),
            ));
        }

        for hash in hashes {
            if hash.is_empty() {
                return Err(ClientError::new(Self::LABEL, "Block hash cannot be empty"));
            }

            if hash.chars().count() != 64 {
                return Err(ClientError::new(
                    Self::LABEL,
                    "Block hash must have 64 characters",
                ));
            }

            if !hash.chars().all(|c| c.is_ascii_hexdigit()) {
                return Err(ClientError::new(
                    Self::LABEL,
                    "Block hash must contain only hexadecimal characters",
                ));
            }
        }

        Ok(())
    }
}

#[cfg(test)]
impl Default for ProverBlockHashValidator {
    fn default() -> Self {
        Self::new(usize::MAX)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn prover_block_hash_validator_return_error_when_empty_hash() {
        let error = ProverBlockHashValidator::default()
            .validate(&["".to_string()])
            .expect_err("Should return an error");

        assert_eq!(
            error,
            ClientError::new("invalid_block_hashes", "Block hash cannot be empty")
        );
    }

    #[test]
    fn prover_block_hash_validator_return_error_when_hash_size_different_than_64() {
        let error = ProverBlockHashValidator::default()
            .validate(&["abc".to_string()])
            .expect_err("Should return an error");

        assert_eq!(
            error,
            ClientError::new("invalid_block_hashes", "Block hash must have 64 characters")
        );
    }

    #[test]
    fn prover_block_hash_validator_return_error_when_hash_contains_non_hexadecimal_characters() {
        for invalid_char in ["g", "x", ";", " ", "à"].iter() {
            let hash = format!("{}{}", "a".repeat(63), invalid_char);
            let error = ProverBlockHashValidator::default()
                .validate(std::slice::from_ref(&hash))
                .expect_err("Should return an error");
            assert_eq!(
                error,
                ClientError::new(
                    "invalid_block_hashes",
                    "Block hash must contain only hexadecimal characters"
                ),
                "Invalid hash: {hash}"
            );
        }
    }

    #[test]
    fn prover_block_hash_validator_when_hash_contains_only_hexadecimal_characters() {
        ProverBlockHashValidator::default()
            .validate(&[format!("bcd9{}", "a".repeat(60))])
            .expect("Should succeed");
    }

    #[test]
    fn prover_block_hash_validator_return_error_when_more_hashes_than_max_allowed() {
        let block_hashes = vec!["a".repeat(64), "b".repeat(64), "c".repeat(64)];
        let validator = ProverBlockHashValidator::new(2);

        let error = validator.validate(&block_hashes).expect_err("Should return an error");

        assert_eq!(
            error,
            ClientError::new(
                "invalid_block_hashes",
                format!(
                    "Block hashes list contains more than maximum allowed number of hashes: '{}'",
                    validator.max_hashes
                )
            )
        );
    }
}
