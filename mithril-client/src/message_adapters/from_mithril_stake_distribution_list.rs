use mithril_common::messages::{
    MessageAdapter, MithrilStakeDistributionListItemMessage, MithrilStakeDistributionListMessage,
};

pub struct FromMithrilStakeDistributionListAdapter;

impl
    MessageAdapter<
        MithrilStakeDistributionListMessage,
        Vec<MithrilStakeDistributionListItemMessage>,
    > for FromMithrilStakeDistributionListAdapter
{
    fn adapt(
        message: MithrilStakeDistributionListMessage,
    ) -> Vec<MithrilStakeDistributionListItemMessage> {
        message.into_iter().collect()
    }
}

// There are no tests for now since collecting a Vec<T> into Vec<T> needs no
// tests but MithrilStakeDistributionListItemMessage may change in the future
// into a real type (and not be just an alias of Vec<…>).
