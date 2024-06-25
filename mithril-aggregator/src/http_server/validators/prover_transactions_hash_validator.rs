use mithril_common::entities::ClientError;

pub struct ProverTransactionsHashValidator {
    max_hashes: usize,
}

impl ProverTransactionsHashValidator {
    const LABEL: &'static str = "invalid_transaction_hashes";

    pub fn new(max_hashes: usize) -> Self {
        Self { max_hashes }
    }

    pub fn validate(&self, hashes: &[String]) -> Result<(), ClientError> {
        if hashes.len() > self.max_hashes {
            return Err(ClientError::new(
                Self::LABEL,
                format!(
                    "Transaction hashes list contains more than maximum allowed number of hashes: '{}'",
                    self.max_hashes
                ),
            ));
        }

        for hash in hashes {
            if hash.is_empty() {
                return Err(ClientError::new(
                    Self::LABEL,
                    "Transaction hash cannot be empty",
                ));
            }

            if hash.chars().count() != 64 {
                return Err(ClientError::new(
                    Self::LABEL,
                    "Transaction hash must have 64 characters",
                ));
            }

            if !hash.chars().all(|c| c.is_ascii_hexdigit()) {
                return Err(ClientError::new(
                    Self::LABEL,
                    "Transaction hash must contain only hexadecimal characters",
                ));
            }
        }

        Ok(())
    }
}

#[cfg(test)]
impl Default for ProverTransactionsHashValidator {
    fn default() -> Self {
        Self::new(usize::MAX)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn prover_transactions_hash_validator_return_error_when_empty_hash() {
        let error = ProverTransactionsHashValidator::default()
            .validate(&["".to_string()])
            .expect_err("Should return an error");

        assert_eq!(
            error,
            ClientError::new(
                "invalid_transaction_hashes",
                "Transaction hash cannot be empty"
            )
        );
    }

    #[test]
    fn prover_transactions_hash_validator_return_error_when_hash_size_different_than_64() {
        let error = ProverTransactionsHashValidator::default()
            .validate(&["abc".to_string()])
            .expect_err("Should return an error");

        assert_eq!(
            error,
            ClientError::new(
                "invalid_transaction_hashes",
                "Transaction hash must have 64 characters"
            )
        );
    }

    #[test]
    fn prover_transactions_hash_validator_return_error_when_hash_contains_non_hexadecimal_characters(
    ) {
        for invalid_char in ["g", "x", ";", " ", "à"].iter() {
            let hash = format!("{}{}", "a".repeat(63), invalid_char);
            let error = ProverTransactionsHashValidator::default()
                .validate(&[hash.clone()])
                .expect_err("Should return an error");
            assert_eq!(
                error,
                ClientError::new(
                    "invalid_transaction_hashes",
                    "Transaction hash must contain only hexadecimal characters"
                ),
                "Invalid hash: {}",
                hash
            );
        }
    }

    #[test]
    fn prover_transactions_hash_validator_when_hash_contains_only_hexadecimal_characters() {
        ProverTransactionsHashValidator::default()
            .validate(&[format!("bcd9{}", "a".repeat(60))])
            .expect("Should succeed");
    }

    #[test]
    fn prover_transactions_hash_validator_return_error_when_more_hashes_than_max_allowed() {
        let transactions_hashes = vec!["a".repeat(64), "b".repeat(64), "c".repeat(64)];
        let validator = ProverTransactionsHashValidator::new(2);

        let error = validator
            .validate(&transactions_hashes)
            .expect_err("Should return an error");

        assert_eq!(
            error,
            ClientError::new(
                "invalid_transaction_hashes",
                format!(
                    "Transaction hashes list contains more than maximum allowed number of hashes: '{}'",
                    validator.max_hashes
                )
            )
        );
    }
}
