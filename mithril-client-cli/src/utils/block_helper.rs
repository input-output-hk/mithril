use mithril_client::common::{BlockNumber, BlockNumberOffset};

/// Calculates the block depth from the latest certified block number and security parameter.
pub fn compute_depth(
    latest_certified_block_number: BlockNumber,
    security_parameter: BlockNumberOffset,
    block_number: BlockNumber,
) -> BlockNumber {
    latest_certified_block_number + security_parameter - block_number
}

//tests
#[cfg(test)]
mod compute_depth {
    use super::*;

    #[test]
    fn test_must_be_the_latest_certified_block_number_plus_security_parameter_minus_block_number() {
        let latest_certified_block_number = BlockNumber(100);
        let security_parameter = BlockNumberOffset(10);
        let block_number = BlockNumber(90);

        let depth = compute_depth(
            latest_certified_block_number,
            security_parameter,
            block_number,
        );
        assert_eq!(
            depth,
            latest_certified_block_number + security_parameter - block_number
        );
    }
}
