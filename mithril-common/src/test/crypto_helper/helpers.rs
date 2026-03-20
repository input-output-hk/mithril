use crate::{
    StdResult,
    crypto_helper::{MKMap, MKMapNode, MKTree, MKTreeNode, MKTreeStorer},
    entities::BlockRange,
};

pub(crate) mod mkmap_helpers {
    use super::*;

    pub fn fold_nodes_per_block_range_into_mkmap<N, I, S>(
        nodes_per_block_range: I,
    ) -> StdResult<MKMap<BlockRange, MKMapNode<BlockRange, S>, S>>
    where
        N: Into<MKTreeNode> + Clone,
        I: IntoIterator<Item = (BlockRange, Vec<N>)>,
        S: MKTreeStorer,
    {
        let mk_map = MKMap::<_, _, S>::new(
            nodes_per_block_range
                .into_iter()
                .try_fold(
                    vec![],
                    |mut acc, (block_range, nodes)| -> StdResult<Vec<(_, MKMapNode<_, S>)>> {
                        acc.push((block_range, MKTree::<S>::new(&nodes)?.into()));
                        Ok(acc)
                    },
                )?
                .as_slice(),
        )?;

        Ok(mk_map)
    }
}
