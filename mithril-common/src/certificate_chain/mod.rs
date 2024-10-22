//! Tools to retrieve, validate the Certificate Chain created by an aggregator

mod certificate_genesis;
mod certificate_retriever;
mod certificate_verifier;
cfg_test_tools! {
    mod fake_certificate_retriever;
}

pub use certificate_genesis::CertificateGenesisProducer;
pub use certificate_retriever::{CertificateRetriever, CertificateRetrieverError};
pub use certificate_verifier::{
    CertificateVerifier, CertificateVerifierError, MithrilCertificateVerifier,
};

cfg_test_tools! {
    pub use fake_certificate_retriever::FakeCertificaterRetriever;
}
