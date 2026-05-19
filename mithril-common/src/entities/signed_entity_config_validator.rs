use std::collections::BTreeSet;

use thiserror::Error;

use crate::entities::{
    CardanoBlocksTransactionsSigningConfig, CardanoTransactionsSigningConfig,
    SignedEntityTypeDiscriminants,
};

/// Validates the consistency of signed entity configuration inputs.
///
/// This validator checks whether the set of enabled signed entity discriminants is compatible
/// with the available signing configuration, without depending on a concrete configuration type.
///
/// It is intended to be reusable by different configuration sources that expose the same
/// validation data.
pub struct SignedEntityConfigValidator;

impl SignedEntityConfigValidator {
    /// Checks that all given discriminants have the required signing configuration present.
    ///
    ///- Returns `Ok` when all required configurations are present.
    ///- Returns `Err(...)` with the usable and non-usable discriminants when one or more required
    ///  configurations are missing.
    pub fn check_consistency(
        enabled_discriminants: &BTreeSet<SignedEntityTypeDiscriminants>,
        cardano_transactions_signing_config: &Option<CardanoTransactionsSigningConfig>,
        cardano_blocks_transactions_signing_config: &Option<CardanoBlocksTransactionsSigningConfig>,
    ) -> Result<(), InconsistentSignedEntityConfigError> {
        let (usable_discriminants, not_usable_discriminants): (BTreeSet<_>, BTreeSet<_>) =
            enabled_discriminants
                .iter()
                .partition(|discriminant| match discriminant {
                    SignedEntityTypeDiscriminants::CardanoTransactions => {
                        cardano_transactions_signing_config.is_some()
                    }
                    SignedEntityTypeDiscriminants::CardanoBlocksTransactions => {
                        cardano_blocks_transactions_signing_config.is_some()
                    }
                    // All other discriminants require no additional config and are always usable
                    SignedEntityTypeDiscriminants::MithrilStakeDistribution
                    | SignedEntityTypeDiscriminants::CardanoStakeDistribution
                    | SignedEntityTypeDiscriminants::CardanoImmutableFilesFull
                    | SignedEntityTypeDiscriminants::CardanoDatabase => true,
                });

        if not_usable_discriminants.is_empty() {
            Ok(())
        } else {
            Err(InconsistentSignedEntityConfigError {
                usable_discriminants,
                not_usable_discriminants,
            })
        }
    }
}

/// [SignedEntityConfigValidator::check_consistency] error
#[derive(Error, Debug, Clone, PartialEq, Eq)]
#[error(
    "The following signed entity can't be used '{not_usable_discriminants:?}': missing associated signing configuration"
)]
pub struct InconsistentSignedEntityConfigError {
    /// The subset of the allowed discriminants that can be used.
    pub usable_discriminants: BTreeSet<SignedEntityTypeDiscriminants>,
    /// The discriminants that can't be used because the configuration is inconsistent.
    pub not_usable_discriminants: BTreeSet<SignedEntityTypeDiscriminants>,
}

#[cfg(test)]
mod tests {
    use crate::test::double::Dummy;

    use SignedEntityTypeDiscriminants::*;

    use super::*;

    #[test]
    fn valid_with_no_discriminants_and_no_additional_config() {
        assert_eq!(
            Ok(()),
            SignedEntityConfigValidator::check_consistency(&BTreeSet::new(), &None, &None)
        );
    }

    #[test]
    fn valid_if_no_discriminants_with_additional_config_are_allowed() {
        assert_eq!(
            Ok(()),
            SignedEntityConfigValidator::check_consistency(
                &BTreeSet::from([CardanoStakeDistribution, CardanoDatabase]),
                &None,
                &None
            )
        );
    }

    #[test]
    fn valid_if_all_discriminants_with_additional_config_are_allowed_and_their_configs_are_set() {
        assert_eq!(
            Ok(()),
            SignedEntityConfigValidator::check_consistency(
                &BTreeSet::from([CardanoTransactions, CardanoBlocksTransactions]),
                &Some(CardanoTransactionsSigningConfig::dummy()),
                &Some(CardanoBlocksTransactionsSigningConfig::dummy())
            )
        );
    }

    #[test]
    fn invalid_if_cardano_transactions_is_allowed_but_its_config_is_not_set() {
        assert_eq!(
            Err(InconsistentSignedEntityConfigError {
                usable_discriminants: BTreeSet::new(),
                not_usable_discriminants: BTreeSet::from([CardanoTransactions])
            }),
            SignedEntityConfigValidator::check_consistency(
                &BTreeSet::from([CardanoTransactions]),
                &None,
                &Some(CardanoBlocksTransactionsSigningConfig::dummy()),
            )
        );
    }

    #[test]
    fn invalid_if_cardano_blocks_transactions_is_allowed_but_its_config_is_not_set() {
        assert_eq!(
            Err(InconsistentSignedEntityConfigError {
                usable_discriminants: BTreeSet::new(),
                not_usable_discriminants: BTreeSet::from([CardanoBlocksTransactions])
            }),
            SignedEntityConfigValidator::check_consistency(
                &BTreeSet::from([CardanoBlocksTransactions]),
                &Some(CardanoTransactionsSigningConfig::dummy()),
                &None,
            )
        );
    }

    #[test]
    fn invalid_with_all_discriminants_allowed_but_no_additional_configs_set() {
        let expected_not_usable_discriminants =
            BTreeSet::from([CardanoTransactions, CardanoBlocksTransactions]);

        assert_eq!(
            Err(InconsistentSignedEntityConfigError {
                usable_discriminants: SignedEntityTypeDiscriminants::all()
                    .difference(&expected_not_usable_discriminants)
                    .cloned()
                    .collect(),
                not_usable_discriminants: expected_not_usable_discriminants
            }),
            SignedEntityConfigValidator::check_consistency(
                &SignedEntityTypeDiscriminants::all(),
                &None,
                &None,
            )
        );
    }
}
