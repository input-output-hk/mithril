use crate::{
    certificate_chain::CertificateGenesisProducer,
    crypto_helper::{
        ProtocolAggregateVerificationKey, ProtocolClerk, ProtocolGenesisSigner,
        ProtocolGenesisVerifier, ProtocolParameters,
    },
    entities::{Certificate, CertificateSignature, Epoch, ProtocolMessage, ProtocolMessagePartKey},
    test_utils::{fake_data, MithrilFixture, MithrilFixtureBuilder, SignerFixture},
};

use crate::entities::CertificateMetadata;
use std::{
    cmp::min,
    collections::{BTreeSet, HashMap},
    sync::Arc,
};

/// Genesis certificate processor function type. For tests only.
pub type GenesisCertificateProcessorFunc =
    dyn Fn(Certificate, &CertificateChainBuilderContext, &ProtocolGenesisSigner) -> Certificate;

/// Standard certificate processor function type. For tests only.
pub type StandardCertificateProcessorFunc =
    dyn Fn(Certificate, &CertificateChainBuilderContext) -> Certificate;

/// Total signers per epoch processor function type. For tests only.
pub type TotalSignersPerEpochProcessorFunc = dyn Fn(Epoch) -> usize;

/// Context used while building a certificate chain. For tests only.
pub struct CertificateChainBuilderContext<'a> {
    pub index_certificate: usize,
    #[allow(dead_code)]
    pub total_certificates: usize,
    pub epoch: Epoch,
    pub fixture: &'a MithrilFixture,
    pub next_fixture: &'a MithrilFixture,
}

impl CertificateChainBuilderContext<'_> {
    /// Computes the protocol message seed.
    pub fn compute_protocol_message_seed(&self) -> ProtocolMessage {
        let mut protocol_message = ProtocolMessage::new();
        protocol_message.set_message_part(
            ProtocolMessagePartKey::NextAggregateVerificationKey,
            self.next_fixture.compute_and_encode_avk(),
        );
        protocol_message.set_message_part(
            ProtocolMessagePartKey::NextProtocolParameters,
            self.next_fixture.protocol_parameters().compute_hash(),
        );
        protocol_message
            .set_message_part(ProtocolMessagePartKey::CurrentEpoch, self.epoch.to_string());

        protocol_message
    }

    /// Checks if the current certificate is the last one.
    pub fn is_last_certificate(&self) -> bool {
        self.index_certificate == self.total_certificates - 1
    }
}

/// A builder for creating a certificate chain. For tests only.
///
/// # Simple example usage for building a fully valid certificate chain
///
/// ```
///     use mithril_common::crypto_helper::ProtocolParameters;
///     use mithril_common::test_utils::CertificateChainBuilder;
///
///     let (certificate_chain, _protocol_genesis_verifier) = CertificateChainBuilder::new()
///         .with_total_certificates(5)
///         .with_certificates_per_epoch(2)
///         .build();
///
///     assert_eq!(5, certificate_chain.len());
/// ```
///
/// # More complex example usage for building a fully valid certificate chain
///
/// ```
///     use std::cmp::min;
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
///         })
///         .with_total_signers_per_epoch_processor(&|epoch| min(1 + *epoch as usize, 10))
///         .build();
///
///     assert_eq!(5, certificate_chain.len());
/// ```
///
/// # Advanced example usage for building an adversarial certificate chain
///
/// ```
///     use mithril_common::entities::Epoch;
///     use mithril_common::crypto_helper::ProtocolParameters;
///     use mithril_common::test_utils::CertificateChainBuilder;
///
///     let (certificate_chain, _protocol_genesis_verifier) = CertificateChainBuilder::new()
///         .with_total_certificates(5)
///         .with_certificates_per_epoch(2)
///         .with_standard_certificate_processor(&|certificate, context| {
///             let mut certificate = certificate;
///             // Alter the epoch of the last certificate
///             if context.is_last_certificate() {
///                 certificate.epoch = Epoch(123);
///             }
///
///             certificate
///        })
///        .build();
///
///     assert_eq!(5, certificate_chain.len());
/// ```
pub struct CertificateChainBuilder<'a> {
    total_certificates: u64,
    certificates_per_epoch: u64,
    protocol_parameters: ProtocolParameters,
    total_signers_per_epoch_processor: &'a TotalSignersPerEpochProcessorFunc,
    genesis_certificate_processor: &'a GenesisCertificateProcessorFunc,
    standard_certificate_processor: &'a StandardCertificateProcessorFunc,
}

impl<'a> CertificateChainBuilder<'a> {
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
            total_signers_per_epoch_processor: &|epoch| min(2 + *epoch as usize, 5),
            genesis_certificate_processor: &|certificate, _, _| certificate,
            standard_certificate_processor: &|certificate, _| certificate,
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

    /// Set the total signers per epoch processor.
    pub fn with_total_signers_per_epoch_processor(
        mut self,
        total_signers_per_epoch_processor: &'a TotalSignersPerEpochProcessorFunc,
    ) -> Self {
        self.total_signers_per_epoch_processor = total_signers_per_epoch_processor;

        self
    }

    /// Set the genesis certificate processor.
    pub fn with_genesis_certificate_processor(
        mut self,
        genesis_certificate_processor: &'a GenesisCertificateProcessorFunc,
    ) -> Self {
        self.genesis_certificate_processor = genesis_certificate_processor;

        self
    }

    /// Set the standard certificate processor.
    pub fn with_standard_certificate_processor(
        mut self,
        standard_certificate_processor: &'a StandardCertificateProcessorFunc,
    ) -> Self {
        self.standard_certificate_processor = standard_certificate_processor;

        self
    }

    /// Build the certificate chain.
    pub fn build(self) -> (Vec<Certificate>, ProtocolGenesisVerifier) {
        let (genesis_signer, genesis_verifier) = CertificateChainBuilder::setup_genesis();
        let epochs = self.build_epochs();
        let fixtures_per_epoch = self.build_fixtures_for_epochs(&epochs);
        let genesis_certificate_processor = self.genesis_certificate_processor;
        let standard_certificate_processor = self.standard_certificate_processor;
        let total_certificates = epochs.len() - 1;
        let certificates = epochs
            .into_iter()
            .take(total_certificates)
            .enumerate()
            .map(|(index_certificate, epoch)| {
                let fixture = fixtures_per_epoch.get(&epoch).unwrap();
                let next_fixture = fixtures_per_epoch.get(&epoch.next()).unwrap();
                let context = CertificateChainBuilderContext {
                    index_certificate,
                    total_certificates,
                    epoch,
                    fixture,
                    next_fixture,
                };
                match index_certificate {
                    0 => genesis_certificate_processor(
                        self.build_genesis_certificate(&context, &genesis_signer),
                        &context,
                        &genesis_signer,
                    ),
                    _ => standard_certificate_processor(
                        self.build_standard_certificate(&context),
                        &context,
                    ),
                }
            })
            .collect::<Vec<Certificate>>();
        let certificates_chained = self.compute_chained_certificates(certificates);

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

    fn build_epochs(&self) -> Vec<Epoch> {
        let total_certificates = self.total_certificates;
        let certificates_per_epoch = self.certificates_per_epoch;
        (1..total_certificates + 2)
            .map(|i| match certificates_per_epoch {
                0 => panic!("expected at least 1 certificate per epoch"),
                1 => Epoch(i),
                _ => Epoch(i / certificates_per_epoch + 1),
            })
            .collect::<Vec<_>>()
    }

    fn build_fixtures_for_epochs(&self, epochs: &[Epoch]) -> HashMap<Epoch, MithrilFixture> {
        let fixtures_per_epoch = epochs
            .iter()
            .collect::<BTreeSet<_>>()
            .into_iter()
            .map(|epoch| {
                let total_signers = (self.total_signers_per_epoch_processor)(*epoch);
                let protocol_parameters = self.protocol_parameters.to_owned().into();
                (
                    *epoch,
                    MithrilFixtureBuilder::default()
                        .with_protocol_parameters(protocol_parameters)
                        .with_signers(total_signers)
                        .build(),
                )
            })
            .collect::<HashMap<_, _>>();

        fixtures_per_epoch
    }

    fn build_base_certificate(&self, context: &CertificateChainBuilderContext) -> Certificate {
        let index_certificate = context.index_certificate;
        let epoch = context.epoch;
        let immutable_file_number = index_certificate as u64 * 10;
        let certificate_hash = format!("certificate_hash-{index_certificate}");
        let avk = Self::compute_avk_for_signers(&context.fixture.signers_fixture());
        let protocol_parameters = context.fixture.protocol_parameters().to_owned();
        let base_certificate = fake_data::certificate(certificate_hash);
        let protocol_message = context.compute_protocol_message_seed();
        let signed_message = protocol_message.compute_hash();

        Certificate {
            epoch,
            aggregate_verification_key: avk.to_owned(),
            previous_hash: "".to_string(),
            protocol_message,
            signed_message,
            #[allow(deprecated)]
            metadata: CertificateMetadata {
                immutable_file_number,
                protocol_parameters,
                ..base_certificate.metadata
            },
            ..base_certificate
        }
    }

    fn build_genesis_certificate(
        &self,
        context: &CertificateChainBuilderContext,
        genesis_signer: &ProtocolGenesisSigner,
    ) -> Certificate {
        let epoch = context.epoch;
        let certificate = self.build_base_certificate(context);
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

    fn build_standard_certificate(&self, context: &CertificateChainBuilderContext) -> Certificate {
        let fixture = context.fixture;
        let mut certificate = self.build_base_certificate(context);
        certificate.metadata.signers = fixture.stake_distribution_parties();
        let mut protocol_message = certificate.protocol_message.clone();
        protocol_message.set_message_part(
            ProtocolMessagePartKey::SnapshotDigest,
            format!("digest-{}", context.index_certificate),
        );
        certificate.protocol_message = protocol_message;
        certificate.signed_message = certificate.protocol_message.compute_hash();
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

    fn compute_chained_certificates(&self, certificates: Vec<Certificate>) -> Vec<Certificate> {
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
                } else {
                    certificate_new.previous_hash = "".to_string();
                }
                certificate_new.hash = certificate_new.compute_hash();
                certificates_chained.push(certificate_new);
            });
        certificates_chained.reverse();

        certificates_chained
    }
}

impl<'a> Default for CertificateChainBuilder<'a> {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod test {
    use std::collections::BTreeMap;

    use super::*;

    #[test]
    fn certificate_chain_builder_context_computes_correct_protocol_message_seed() {
        let protocol_parameters = ProtocolParameters {
            m: 123,
            k: 45,
            phi_f: 0.67,
        };
        let next_protocol_parameters = ProtocolParameters {
            m: 100,
            k: 10,
            phi_f: 0.10,
        };
        let fixture = MithrilFixtureBuilder::default()
            .with_protocol_parameters(protocol_parameters.into())
            .with_signers(2)
            .build();
        let next_fixture = MithrilFixtureBuilder::default()
            .with_protocol_parameters(next_protocol_parameters.into())
            .with_signers(3)
            .build();
        let context = CertificateChainBuilderContext {
            index_certificate: 2,
            total_certificates: 5,
            epoch: Epoch(1),
            fixture: &fixture,
            next_fixture: &next_fixture,
        };
        let expected_next_avk_part_value = next_fixture.compute_and_encode_avk();
        let expected_next_protocol_parameters_part_value =
            next_fixture.protocol_parameters().compute_hash();

        let expected_current_epoch_part_value = context.epoch.to_string();

        let protocol_message = context.compute_protocol_message_seed();

        let mut expected_protocol_message = ProtocolMessage::new();
        expected_protocol_message.set_message_part(
            ProtocolMessagePartKey::NextAggregateVerificationKey,
            expected_next_avk_part_value,
        );
        expected_protocol_message.set_message_part(
            ProtocolMessagePartKey::NextProtocolParameters,
            expected_next_protocol_parameters_part_value,
        );
        expected_protocol_message.set_message_part(
            ProtocolMessagePartKey::CurrentEpoch,
            expected_current_epoch_part_value,
        );

        assert_eq!(expected_protocol_message, protocol_message);
    }

    #[test]
    fn certificate_chain_builder_context_checks_correctly_if_certificate_is_last() {
        let fixture = MithrilFixtureBuilder::default().with_signers(2).build();
        let context = CertificateChainBuilderContext {
            index_certificate: 4,
            total_certificates: 5,
            epoch: Epoch(1),
            fixture: &fixture,
            next_fixture: &fixture,
        };

        assert!(context.is_last_certificate());
    }

    #[test]
    fn builds_certificate_chain_with_correct_length() {
        let (certificate_chain, _) = CertificateChainBuilder::new()
            .with_total_certificates(5)
            .with_certificates_per_epoch(2)
            .build();

        assert_eq!(5, certificate_chain.len());
    }

    #[test]
    fn builds_valid_epochs() {
        let expected_epochs = vec![Epoch(1), Epoch(2), Epoch(2), Epoch(3), Epoch(3), Epoch(4)];

        let epochs = CertificateChainBuilder::default()
            .with_total_certificates(5)
            .with_certificates_per_epoch(2)
            .build_epochs();

        assert_eq!(expected_epochs, epochs);
    }

    #[test]
    fn builds_valid_fixtures_per_epochs() {
        let expected_total_signers = (1..=6).collect::<Vec<_>>();
        let certificate_chain_builder = CertificateChainBuilder::default()
            .with_total_certificates(5)
            .with_certificates_per_epoch(1)
            .with_total_signers_per_epoch_processor(&|epoch| *epoch as usize);
        let epochs = certificate_chain_builder.build_epochs();

        let epoch_fixtures =
            BTreeMap::from_iter(certificate_chain_builder.build_fixtures_for_epochs(&epochs));

        let total_signers = epoch_fixtures
            .into_values()
            .map(|fixture| fixture.signers().len())
            .collect::<Vec<_>>();
        assert_eq!(expected_total_signers, total_signers);
    }

    #[test]
    fn builds_valid_genesis_certificate() {
        let expected_protocol_parameters = ProtocolParameters {
            m: 123,
            k: 45,
            phi_f: 0.67,
        };
        let fixture = MithrilFixtureBuilder::default()
            .with_protocol_parameters(expected_protocol_parameters.into())
            .with_signers(2)
            .build();
        let next_fixture = MithrilFixtureBuilder::default()
            .with_protocol_parameters(expected_protocol_parameters.into())
            .with_signers(3)
            .build();
        let context = CertificateChainBuilderContext {
            index_certificate: 0,
            total_certificates: 5,
            epoch: Epoch(1),
            fixture: &fixture,
            next_fixture: &next_fixture,
        };
        let expected_protocol_message = context.compute_protocol_message_seed();
        let expected_signed_message = expected_protocol_message.compute_hash();
        let (protocol_genesis_signer, _) = CertificateChainBuilder::setup_genesis();

        let genesis_certificate = CertificateChainBuilder::default()
            .with_protocol_parameters(expected_protocol_parameters)
            .build_genesis_certificate(&context, &protocol_genesis_signer);

        assert!(genesis_certificate.is_genesis());
        assert_eq!(Epoch(1), genesis_certificate.epoch);
        assert_eq!(
            expected_protocol_parameters,
            genesis_certificate.metadata.protocol_parameters.into()
        );
        assert_eq!(0, genesis_certificate.metadata.signers.len());
        assert_eq!(
            expected_protocol_message,
            genesis_certificate.protocol_message
        );
        assert_eq!(expected_signed_message, genesis_certificate.signed_message);
    }

    #[test]
    fn builds_valid_standard_certificate() {
        let expected_protocol_parameters = ProtocolParameters {
            m: 123,
            k: 45,
            phi_f: 0.67,
        };
        let fixture = MithrilFixtureBuilder::default()
            .with_protocol_parameters(expected_protocol_parameters.into())
            .with_signers(2)
            .build();
        let next_fixture = MithrilFixtureBuilder::default()
            .with_protocol_parameters(expected_protocol_parameters.into())
            .with_signers(3)
            .build();
        let avk = fixture.compute_and_encode_avk();
        let context = CertificateChainBuilderContext {
            index_certificate: 2,
            total_certificates: 5,
            epoch: Epoch(1),
            fixture: &fixture,
            next_fixture: &next_fixture,
        };
        let mut expected_protocol_message = context.compute_protocol_message_seed();
        expected_protocol_message.set_message_part(
            ProtocolMessagePartKey::SnapshotDigest,
            format!("digest-{}", context.index_certificate),
        );
        let expected_signed_message = expected_protocol_message.compute_hash();

        let standard_certificate = CertificateChainBuilder::default()
            .with_protocol_parameters(expected_protocol_parameters)
            .build_standard_certificate(&context);

        assert!(!standard_certificate.is_genesis());
        assert_eq!(Epoch(1), standard_certificate.epoch);
        assert_eq!(
            expected_protocol_parameters,
            standard_certificate.metadata.protocol_parameters.into()
        );
        assert_eq!(2, standard_certificate.metadata.signers.len());
        assert_eq!(
            expected_protocol_message,
            standard_certificate.protocol_message
        );
        assert_eq!(expected_signed_message, standard_certificate.signed_message);
        assert_eq!(
            avk,
            standard_certificate
                .aggregate_verification_key
                .to_json_hex()
                .unwrap()
        );
    }

    #[test]
    fn builds_certificate_chain_correctly_chained() {
        let certificates = vec![
            fake_data::certificate("cert-1".to_string()),
            fake_data::certificate("cert-2".to_string()),
            fake_data::certificate("cert-3".to_string()),
        ];

        let certificates_chained =
            CertificateChainBuilder::default().compute_chained_certificates(certificates);

        assert_eq!("", certificates_chained[2].previous_hash);
        assert_eq!(
            certificates_chained[2].hash,
            certificates_chained[1].previous_hash
        );
        assert_eq!(
            certificates_chained[1].hash,
            certificates_chained[0].previous_hash
        );
    }

    #[test]
    fn builds_certificate_chain_with_alteration_on_genesis_certificate() {
        let (certificates, _) = CertificateChainBuilder::new()
            .with_total_certificates(5)
            .with_genesis_certificate_processor(&|certificate, _, _| {
                let mut certificate = certificate;
                certificate.signed_message = "altered_msg".to_string();

                certificate
            })
            .build();

        assert_eq!(
            "altered_msg".to_string(),
            certificates.last().unwrap().signed_message
        );
    }

    #[test]
    fn builds_certificate_chain_with_alteration_on_standard_certificates() {
        let total_certificates = 5;
        let expected_signed_messages = (1..total_certificates)
            .rev()
            .map(|i| format!("altered-msg-{}", i))
            .collect::<Vec<_>>();

        let (certificates, _) = CertificateChainBuilder::new()
            .with_total_certificates(total_certificates)
            .with_standard_certificate_processor(&|certificate, context| {
                let mut certificate = certificate;
                certificate.signed_message = format!("altered-msg-{}", context.index_certificate);

                certificate
            })
            .build();

        let signed_message = certificates
            .into_iter()
            .take(total_certificates as usize - 1)
            .map(|certificate| certificate.signed_message)
            .collect::<Vec<_>>();
        assert_eq!(expected_signed_messages, signed_message);
    }
}
