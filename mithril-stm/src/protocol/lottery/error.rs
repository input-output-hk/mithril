/// Errors which can be outputted by key registration.
#[derive(Debug, Clone, thiserror::Error)]
pub enum LotteryError {
    /// Snark lottery verification error
    #[error("Snark lottery verification failed.")]
    SnarkLotteryVerification,

    /// Invalid phi_f value
    #[error("phi_f must be in the range [0,1]. Received phi_f: {0}.")]
    InvalidPhiValue(f64),

    /// Base field element conversion error
    #[error("Base field element conversion failed.")]
    BaseFieldElementConversion,

    /// Float to integer conversion error
    #[error("Float to integer conversion failed.")]
    FloatToIntegerConversion,

    /// Invalid integer value
    #[error("Conversion from float to integer should be less than 0, received: {0}.")]
    InvalidIntegerValue(u64),

    /// There is an index out of bounds
    #[error("Received index, {0}, is higher than what the security parameter allows, {1}.")]
    IndexBoundFailed(u64, u64),
}
