pub mod certificate;
pub mod mithril_stake_distribution;
pub mod proof;
pub mod snapshot;
pub mod statistics;

mod middleware {
    use crate::extensions::fake::FakeAggregatorCalls;
    use std::convert::Infallible;
    use warp::Filter;

    pub fn with_calls_middleware(
        calls: FakeAggregatorCalls,
    ) -> impl Filter<Extract = (FakeAggregatorCalls,), Error = Infallible> + Clone {
        warp::any().map(move || calls.clone())
    }
}
