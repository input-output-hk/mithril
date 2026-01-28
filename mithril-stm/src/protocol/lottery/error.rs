/// Errors which can be outputted by key registration.
#[derive(Debug, Clone, thiserror::Error)]
pub enum LotteryError {
    /// Snark lottery verification error
    #[error("Snark lottery verification failed.")]
    SnarkLotteryVerification,

    /// There is an index out of bounds
    #[error("Received index, {0}, is higher than what the security parameter allows, {1}.")]
    IndexBoundFailed(u64, u64),
}
