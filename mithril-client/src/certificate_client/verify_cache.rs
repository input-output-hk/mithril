use async_trait::async_trait;

/// API that defines how to cache certificates validation results.
#[cfg_attr(test, mockall::automock)]
#[cfg_attr(target_family = "wasm", async_trait(?Send))]
#[cfg_attr(not(target_family = "wasm"), async_trait)]
pub trait CertificateVerifierCache: Sync + Send {}

#[cfg(test)]
mod tests {}
