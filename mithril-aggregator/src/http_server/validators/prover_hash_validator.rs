use std::fmt::{self, Display, Formatter};

use mithril_common::entities::ClientError;

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum HashKind {
    Transaction,
    Block,
}

impl HashKind {
    fn label(self) -> &'static str {
        match self {
            Self::Transaction => "invalid_transaction_hashes",
            Self::Block => "invalid_block_hashes",
        }
    }
}

impl Display for HashKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::Transaction => write!(f, "Transaction"),
            Self::Block => write!(f, "Block"),
        }
    }
}

pub struct ProverHashValidator {
    kind: HashKind,
    max_hashes: usize,
}

impl ProverHashValidator {
    pub fn new(kind: HashKind, max_hashes: usize) -> Self {
        Self { kind, max_hashes }
    }

    pub fn validate(&self, hashes: &[String]) -> Result<(), ClientError> {
        if hashes.len() > self.max_hashes {
            return Err(ClientError::new(
                self.kind.label(),
                format!(
                    "{} hashes list contains more than maximum allowed number of hashes: '{}'",
                    self.kind, self.max_hashes
                ),
            ));
        }

        for hash in hashes {
            if hash.is_empty() {
                return Err(ClientError::new(
                    self.kind.label(),
                    format!("{} hash cannot be empty", self.kind),
                ));
            }

            if hash.chars().count() != 64 {
                return Err(ClientError::new(
                    self.kind.label(),
                    format!("{} hash must have 64 characters", self.kind),
                ));
            }

            if !hash.chars().all(|c| c.is_ascii_hexdigit()) {
                return Err(ClientError::new(
                    self.kind.label(),
                    format!(
                        "{} hash must contain only hexadecimal characters",
                        self.kind
                    ),
                ));
            }
        }

        Ok(())
    }
}

#[cfg(test)]
impl Default for ProverHashValidator {
    fn default() -> Self {
        Self::new(HashKind::Transaction, usize::MAX)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn prover_hash_validator_display() {
        assert_eq!(HashKind::Transaction.to_string(), "Transaction");
        assert_eq!(HashKind::Block.to_string(), "Block");
    }

    #[test]
    fn prover_hash_validator_return_error_when_empty_hash() {
        let error = ProverHashValidator::new(HashKind::Transaction, usize::MAX)
            .validate(&["".to_string()])
            .expect_err("Should return an error");

        assert_eq!(
            error,
            ClientError::new("invalid_transaction_hashes", "Transaction hash cannot be empty")
        );
    }

    #[test]
    fn prover_hash_validator_return_error_when_hash_size_different_than_64() {
        let error = ProverHashValidator::new(HashKind::Block, usize::MAX)
            .validate(&["abc".to_string()])
            .expect_err("Should return an error");

        assert_eq!(
            error,
            ClientError::new("invalid_block_hashes", "Block hash must have 64 characters")
        );
    }

    #[test]
    fn prover_hash_validator_return_error_when_hash_contains_non_hexadecimal_characters() {
        for invalid_char in ["g", "x", ";", " ", "à"] {
            let hash = format!("{}{}", "a".repeat(63), invalid_char);
            let error = ProverHashValidator::new(HashKind::Transaction, usize::MAX)
                .validate(std::slice::from_ref(&hash))
                .expect_err("Should return an error");
            assert_eq!(
                error,
                ClientError::new(
                    "invalid_transaction_hashes",
                    "Transaction hash must contain only hexadecimal characters"
                ),
                "Invalid hash: {hash}"
            );
        }
    }

    #[test]
    fn prover_hash_validator_when_hash_contains_only_hexadecimal_characters() {
        ProverHashValidator::new(HashKind::Block, usize::MAX)
            .validate(&[format!("bcd9{}", "a".repeat(60))])
            .expect("Should succeed");
    }

    #[test]
    fn prover_hash_validator_return_error_when_more_hashes_than_max_allowed() {
        let block_hashes = vec!["a".repeat(64), "b".repeat(64), "c".repeat(64)];
        let validator = ProverHashValidator::new(HashKind::Block, 2);

        let error = validator
            .validate(&block_hashes)
            .expect_err("Should return an error");

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
