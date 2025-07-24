use anyhow::Context;

use crate::StdResult;
use crate::crypto_helper::{
    MKProof, MKTree, MKTreeNode, MKTreeStoreInMemory, ProtocolInitializer, ProtocolParameters,
};

/// Extension trait adding test utilities to [ProtocolInitializer]
pub trait ProtocolInitializerTestExtension {
    /// `TEST ONLY` - Override the protocol parameters of the `Initializer`
    fn override_protocol_parameters(&mut self, protocol_parameters: &ProtocolParameters);
}

impl ProtocolInitializerTestExtension for ProtocolInitializer {
    fn override_protocol_parameters(&mut self, protocol_parameters: &ProtocolParameters) {
        self.stm_initializer.params = protocol_parameters.to_owned();
    }
}

/// Extension trait adding test utilities to [MKProof]
pub trait MKProofTestExtension {
    /// `TEST ONLY` - Build a [MKProof] based on the given leaves
    fn from_leaves<T: Into<MKTreeNode> + Clone>(leaves: &[T]) -> StdResult<MKProof>;

    /// `TEST ONLY` - Build a [MKProof] based on the given leaves
    fn from_subset_of_leaves<T: Into<MKTreeNode> + Clone>(
        leaves: &[T],
        leaves_to_verify: &[T],
    ) -> StdResult<MKProof>;
}

impl MKProofTestExtension for MKProof {
    fn from_leaves<T: Into<MKTreeNode> + Clone>(leaves: &[T]) -> StdResult<MKProof> {
        Self::from_subset_of_leaves(leaves, leaves)
    }

    fn from_subset_of_leaves<T: Into<MKTreeNode> + Clone>(
        leaves: &[T],
        leaves_to_verify: &[T],
    ) -> StdResult<MKProof> {
        fn list_to_mknode<T: Into<MKTreeNode> + Clone>(hashes: &[T]) -> Vec<MKTreeNode> {
            hashes.iter().map(|h| h.clone().into()).collect()
        }

        let leaves = list_to_mknode(leaves);
        let leaves_to_verify = list_to_mknode(leaves_to_verify);

        let mktree = MKTree::<MKTreeStoreInMemory>::new(&leaves)
            .with_context(|| "MKTree creation should not fail")?;
        mktree.compute_proof(&leaves_to_verify)
    }
}
