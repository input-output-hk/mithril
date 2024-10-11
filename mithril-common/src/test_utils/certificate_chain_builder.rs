use std::{cmp::min, collections::HashMap, sync::Arc};

use crate::{
    certificate_chain::CertificateGenesisProducer,
    crypto_helper::{
        ProtocolAggregateVerificationKey, ProtocolClerk, ProtocolGenesisSigner,
        ProtocolGenesisVerifier, ProtocolParameters,
    },
    entities::{
        Certificate, CertificateMetadata, CertificateSignature, Epoch, ProtocolMessagePartKey,
    },
    test_utils::{fake_data, MithrilFixture, MithrilFixtureBuilder, SignerFixture},
};

/// Context used while building a certificate chain. For tests only.
pub struct CertificateChainBuilderContext<'a> {
    pub index_certificate: usize,
    #[allow(dead_code)]
    pub total_certificates: usize,
    pub epoch: Epoch,
    pub fixture: &'a MithrilFixture,
    pub next_fixture: &'a MithrilFixture,
}

/// A builder for creating a certificate chain. For tests only.
///
/// # Example usage
///
/// ```
///     use mithril_common::crypto_helper::ProtocolParameters;
///     use mithril_common::test_utils::CertificateChainBuilder;
///
///     let (certificate_chain, _protocol_genesis_verifier) = CertificateChainBuilder::new()
///         .with_total_certificates(5)
///         .with_certificates_per_epoch(2)
///         .with_protocol_parameters(ProtocolParameters {
///             m: 100,
///             k: 5,
///             phi_f: 0.65,
///         }).build();
///
///     assert_eq!(5, certificate_chain.len());
/// ```
pub struct CertificateChainBuilder {
    total_certificates: u64,
    certificates_per_epoch: u64,
    protocol_parameters: ProtocolParameters,
}

impl CertificateChainBuilder {
    /// Create a new [CertificateChainBuilder] instance.
    pub fn new() -> Self {
        let protocol_parameters = ProtocolParameters {
            m: 100,
            k: 5,
            phi_f: 0.65,
        };
        Self {
            total_certificates: 5,
            certificates_per_epoch: 1,
            protocol_parameters,
        }
    }

    /// Set the total number of certificates to generate.
    pub fn with_total_certificates(mut self, total_certificates: u64) -> Self {
        self.total_certificates = total_certificates;

        self
    }

    /// Set the number of certificates per epoch.
    pub fn with_certificates_per_epoch(mut self, certificates_per_epoch: u64) -> Self {
        self.certificates_per_epoch = certificates_per_epoch;

        self
    }

    /// Set the protocol parameters.
    pub fn with_protocol_parameters(mut self, protocol_parameters: ProtocolParameters) -> Self {
        self.protocol_parameters = protocol_parameters;

        self
    }

    /// Build the certificate chain.
    pub fn build(self) -> (Vec<Certificate>, ProtocolGenesisVerifier) {
        let (genesis_signer, genesis_verifier) = CertificateChainBuilder::setup_genesis();
        let epochs = Self::setup_epochs(self.total_certificates, self.certificates_per_epoch);
        let fixtures_per_epoch =
            Self::setup_fixtures_for_epochs(&epochs, &self.protocol_parameters);
        let certificate_chain_length = epochs.len() - 1;
        let certificates = epochs
            .iter()
            .take(certificate_chain_length)
            .enumerate()
            .map(|(i, epoch)| {
                let fixture = fixtures_per_epoch.get(epoch).unwrap();
                let next_fixture = fixtures_per_epoch.get(&(*epoch + 1)).unwrap();
                let context = CertificateChainBuilderContext {
                    index_certificate: i,
                    total_certificates: certificate_chain_length,
                    epoch: *epoch,
                    fixture,
                    next_fixture,
                };
                match i {
                    0 => Self::create_genesis_certificate(&context, &genesis_signer),
                    _ => Self::create_standard_certificate(&context),
                }
            })
            .collect::<Vec<Certificate>>();
        let certificates_chained = Self::compute_chained_certificates(certificates);

        (certificates_chained, genesis_verifier)
    }

    fn compute_clerk_for_signers(signers: &[SignerFixture]) -> ProtocolClerk {
        let first_signer = &signers[0].protocol_signer;

        ProtocolClerk::from_signer(first_signer)
    }

    fn compute_avk_for_signers(signers: &[SignerFixture]) -> ProtocolAggregateVerificationKey {
        let clerk = Self::compute_clerk_for_signers(signers);

        clerk.compute_avk().into()
    }

    fn setup_genesis() -> (ProtocolGenesisSigner, ProtocolGenesisVerifier) {
        let genesis_signer = ProtocolGenesisSigner::create_deterministic_genesis_signer();
        let genesis_verifier = genesis_signer.create_genesis_verifier();

        (genesis_signer, genesis_verifier)
    }

    fn setup_epochs(total_certificates: u64, certificates_per_epoch: u64) -> Vec<Epoch> {
        (1..total_certificates + 2)
            .map(|i| match certificates_per_epoch {
                0 => panic!("expected at least 1 certificate per epoch"),
                1 => Epoch(i),
                _ => Epoch(i / certificates_per_epoch + 1),
            })
            .collect::<Vec<_>>()
    }

    fn setup_fixtures_for_epochs(
        epochs: &[Epoch],
        protocol_parameters: &ProtocolParameters,
    ) -> HashMap<Epoch, MithrilFixture> {
        let fixtures_per_epoch = epochs
            .iter()
            .map(|epoch| {
                // TODO: set that in the builder configuration?
                let total_signers = min(2 + **epoch as usize, 5);
                (
                    *epoch,
                    MithrilFixtureBuilder::default()
                        .with_protocol_parameters(protocol_parameters.to_owned().into())
                        .with_signers(total_signers)
                        .build(),
                )
            })
            .collect::<HashMap<_, _>>();

        fixtures_per_epoch
    }

    fn create_base_certificate(context: &CertificateChainBuilderContext) -> Certificate {
        let index = context.index_certificate;
        let epoch = context.epoch;
        let immutable_file_number = index as u64 * 10;
        let digest = format!("digest{index}");
        let certificate_hash = format!("certificate_hash-{index}");
        let avk = Self::compute_avk_for_signers(&context.fixture.signers_fixture());
        let next_avk = Self::compute_avk_for_signers(&context.next_fixture.signers_fixture());
        let next_protocol_parameters = &context.next_fixture.protocol_parameters();
        let mut base_certificate = fake_data::certificate(certificate_hash);
        base_certificate
            .protocol_message
            .set_message_part(ProtocolMessagePartKey::SnapshotDigest, digest);
        base_certificate.protocol_message.set_message_part(
            ProtocolMessagePartKey::NextAggregateVerificationKey,
            next_avk.to_json_hex().unwrap(),
        );
        base_certificate.protocol_message.set_message_part(
            ProtocolMessagePartKey::NextProtocolParameters,
            next_protocol_parameters.compute_hash(),
        );
        base_certificate
            .protocol_message
            .set_message_part(ProtocolMessagePartKey::CurrentEpoch, epoch.to_string());

        Certificate {
            epoch,
            aggregate_verification_key: avk.to_owned(),
            previous_hash: "".to_string(),
            signed_message: base_certificate.protocol_message.compute_hash(),
            #[allow(deprecated)]
            metadata: CertificateMetadata {
                immutable_file_number,
                ..base_certificate.metadata
            },
            ..base_certificate
        }
    }

    fn create_genesis_certificate(
        context: &CertificateChainBuilderContext,
        genesis_signer: &ProtocolGenesisSigner,
    ) -> Certificate {
        let epoch = context.epoch;
        let certificate = Self::create_base_certificate(context);
        let next_avk = Self::compute_avk_for_signers(&context.next_fixture.signers_fixture());
        let next_protocol_parameters = &context.next_fixture.protocol_parameters();
        let genesis_producer =
            CertificateGenesisProducer::new(Some(Arc::new(genesis_signer.to_owned())));
        let genesis_protocol_message = CertificateGenesisProducer::create_genesis_protocol_message(
            next_protocol_parameters,
            &next_avk,
            &epoch,
        )
        .unwrap();
        let genesis_signature = genesis_producer
            .sign_genesis_protocol_message(genesis_protocol_message)
            .unwrap();

        CertificateGenesisProducer::create_genesis_certificate(
            certificate.metadata.protocol_parameters,
            certificate.metadata.network,
            certificate.epoch,
            #[allow(deprecated)]
            certificate.metadata.immutable_file_number,
            next_avk,
            genesis_signature,
        )
        .unwrap()
    }

    fn create_standard_certificate(context: &CertificateChainBuilderContext) -> Certificate {
        let fixture = context.fixture;
        let mut certificate = Self::create_base_certificate(context);
        certificate.metadata.signers = fixture.stake_distribution_parties();
        let single_signatures = fixture
            .signers_fixture()
            .iter()
            .filter_map(|s| {
                s.protocol_signer
                    .sign(certificate.signed_message.as_bytes())
            })
            .collect::<Vec<_>>();
        let clerk = CertificateChainBuilder::compute_clerk_for_signers(&fixture.signers_fixture());
        let multi_signature = clerk
            .aggregate(&single_signatures, certificate.signed_message.as_bytes())
            .unwrap();
        certificate.signature = CertificateSignature::MultiSignature(
            certificate.signed_entity_type(),
            multi_signature.into(),
        );

        certificate
    }

    fn compute_chained_certificates(certificates: Vec<Certificate>) -> Vec<Certificate> {
        let mut certificates_chained: Vec<Certificate> = Vec::new();
        certificates
            .iter()
            .enumerate()
            .for_each(|(i, certificate)| {
                let mut certificate_new = certificate.clone();
                if i > 0 {
                    if let Some(previous_certificate) = certificates_chained.get(i - 1) {
                        certificate_new.previous_hash = previous_certificate.compute_hash();
                    }
                }
                certificate_new.hash = certificate_new.compute_hash();
                certificates_chained.push(certificate_new);
            });
        certificates_chained.reverse();

        certificates_chained
    }
}

impl Default for CertificateChainBuilder {
    fn default() -> Self {
        Self::new()
    }
}
