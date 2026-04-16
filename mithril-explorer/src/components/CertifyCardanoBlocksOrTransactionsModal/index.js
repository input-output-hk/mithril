import React, { useEffect, useState } from "react";
import { Alert, Col, Modal, Row, Tab } from "react-bootstrap";
import { useSelector } from "react-redux";
import CertificateModal from "#/CertificateModal";
import CertificateVerifier, {
  certificateValidationSteps,
} from "#/VerifyCertificate/CertificateVerifier";
import CertificationBreadcrumb from "./CertificationBreadcrumb";
import CertificationResult from "./CertificationResult";
import FetchingProofPane from "./FetchingProofPane";
import ValidatingProofPane from "./ValidatingProofPane";

export const validationSteps = {
  ready: 1,
  fetchingProof: 2,
  validatingProof: 3,
  validatingCertificateChain: 4,
  done: 5,
};

export const certifiedMessageTypes = {
  block: {
    name: "block",
    pluralName: "blocks",
  },
  transaction: {
    name: "transaction",
    pluralName: "transactions",
  },
};

function getTabForStep(step) {
  switch (step) {
    case validationSteps.validatingCertificateChain:
    case validationSteps.validatingProof:
    case validationSteps.done:
      return `#step-${step}`;
    case validationSteps.ready:
    case validationSteps.fetchingProof:
    default:
      return `#step-${validationSteps.fetchingProof}`;
  }
}

function certifiedItemsFromProof(proof, certifiedMessageType) {
  return certifiedMessageType === certifiedMessageTypes.block
    ? (proof?.certified_blocks ?? [])
    : certifiedMessageType === certifiedMessageTypes.transaction
      ? (proof?.certified_transactions ?? [])
      : [];
}

function nonCertifiedItemsFromProof(proof, certifiedMessageType) {
  return certifiedMessageType === certifiedMessageTypes.block
    ? (proof?.non_certified_blocks ?? [])
    : certifiedMessageType === certifiedMessageTypes.transaction
      ? (proof?.non_certified_transactions ?? [])
      : [];
}

async function getProofAndCertificate(client, itemHashes, certifiedMessageType) {
  let proof;

  if (certifiedMessageType === certifiedMessageTypes.block) {
    proof = await client.get_cardano_block_proof(itemHashes);
  } else if (certifiedMessageType === certifiedMessageTypes.transaction) {
    proof = await client.get_cardano_transaction_v2_proof(itemHashes);
  } else {
    throw new Error(`Unsupported certified message type: ${certifiedMessageType.name}`);
  }

  const certificate = await client.get_mithril_certificate(proof.certificate_hash);
  return {
    proof: proof,
    certificate: certificate,
  };
}

async function verifyProofAgainstCertificate(client, proof, certificate, certifiedMessageType) {
  let protocolMessage;

  if (certifiedMessageType === certifiedMessageTypes.block) {
    protocolMessage = await client.verify_cardano_block_proof_then_compute_message(
      proof,
      certificate,
    );
  } else if (certifiedMessageType === certifiedMessageTypes.transaction) {
    protocolMessage = await client.verify_cardano_transaction_v2_proof_then_compute_message(
      proof,
      certificate,
    );
  } else {
    throw new Error(`Unsupported certified message type: ${certifiedMessageType.name}`);
  }

  const isProofValid =
    (await client.verify_message_match_certificate(protocolMessage, certificate)) === true;

  return isProofValid;
}

export default function CertifyCardanoBlocksOrTransactionsModal({
  hashes,
  certifiedMessageType,
  onHashesChange = (hash) => {},
}) {
  const currentAggregator = useSelector((state) => state.settings.selectedAggregator);
  const [client, setClient] = useState(undefined);
  const [certificate, setCertificate] = useState(undefined);
  const [selectedCertificateHash, setSelectedCertificateHash] = useState(undefined);
  const [certificateVerifierStep, setCertificateVerifierStep] = useState(
    certificateValidationSteps.ready,
  );
  const [proof, setProof] = useState({});
  const certifiedItems = certifiedItemsFromProof(proof, certifiedMessageType);
  const nonCertifiedItems = nonCertifiedItemsFromProof(proof, certifiedMessageType);

  const [showLoadingWarning, setShowLoadingWarning] = useState(false);
  const [isProofValid, setIsProofValid] = useState(false);
  const [isCertificateChainValid, setIsCertificateChainValid] = useState(true);
  const [currentStep, setCurrentStep] = useState(validationSteps.ready);
  const [currentTab, setCurrentTab] = useState(getTabForStep(validationSteps.ready));
  const [currentError, setCurrentError] = useState(undefined);

  function handleError(error) {
    console.error(`Cardano blocks/transactions certification error:`, error);
    setCurrentError(error);
    setCurrentStep(validationSteps.done);
  }

  function handleStepClick(step) {
    // Only allow navigation when the work is done
    if (currentStep === validationSteps.done) {
      setCurrentTab(getTabForStep(step));
    }
  }

  function handleCertificateClick(hash) {
    // Only allow to open the submodal when the work is done
    if (currentStep === validationSteps.done) {
      setSelectedCertificateHash(hash);
    }
  }

  function closeIfNotRunning() {
    if (currentStep !== validationSteps.done) {
      setShowLoadingWarning(true);
    } else {
      onHashesChange([]);
    }
  }

  useEffect(() => {
    setShowLoadingWarning(false);
    setIsProofValid(false);
    setIsCertificateChainValid(true);
    setCertificate(undefined);
    setCurrentStep(validationSteps.ready);
    setCurrentError(undefined);

    if (hashes?.length > 0) {
      const {
        fetchGenesisVerificationKey,
        newMithrilWasmClient,
      } = require("@/wasm-client-helpers");

      fetchGenesisVerificationKey(currentAggregator)
        .then((genesisKey) => newMithrilWasmClient(currentAggregator, genesisKey))
        .then((client) => setClient(client))
        .then(() => setCurrentStep(validationSteps.fetchingProof))
        .catch((err) => handleError(err, certifiedMessageType));
    }
  }, [currentAggregator, hashes, certifiedMessageType]);

  useEffect(() => {
    if (certificateVerifierStep === certificateValidationSteps.done) {
      setCurrentStep(validationSteps.done);
      setShowLoadingWarning(false);
    }
  }, [certificateVerifierStep]);

  useEffect(() => {
    setCurrentTab(getTabForStep(currentStep));
  }, [currentStep]);

  useEffect(() => {
    if (currentStep === validationSteps.fetchingProof) {
      getProofAndCertificate(client, hashes, certifiedMessageType)
        .then((res) => {
          setProof(res.proof);
          setCertificate(res.certificate);

          // Artificial wait to give the user a feel of the workload under-hood
          return setTimeout(() => {
            const haveCertifiedItems =
              certifiedItemsFromProof(res.proof, certifiedMessageType).length > 0;

            if (haveCertifiedItems) {
              setCurrentStep(validationSteps.validatingProof);
            } else {
              setIsProofValid(false);
              setCurrentStep(validationSteps.done);
            }
          }, 350);
        })
        .catch((err) => handleError(err));
    }
  }, [client, currentStep, hashes, certifiedMessageType]);

  useEffect(() => {
    if (currentStep === validationSteps.validatingProof && certificate !== undefined) {
      verifyProofAgainstCertificate(client, proof, certificate, certifiedMessageType)
        .then((proofValid) => {
          setIsProofValid(proofValid);

          if (proofValid) {
            // Artificial wait to give the user a feel of the workload under-hood
            return setTimeout(() => {
              setCurrentStep(validationSteps.validatingCertificateChain);
            }, 250);
          } else {
            setCurrentStep(validationSteps.done);
          }
        })
        .catch((err) => handleError(err));
    }
  }, [client, currentStep, proof, certificate, certifiedMessageType]);

  return (
    <Modal
      show={hashes !== undefined && hashes.length > 0}
      onHide={closeIfNotRunning}
      size="xl"
      aria-labelledby="contained-modal-title-vcenter"
      centered>
      <Modal.Header className="text-break" closeButton>
        <Modal.Title id="contained-modal-title-vcenter">
          Cardano <span className="text-capitalize">{certifiedMessageType.pluralName}</span>{" "}
          Certification
        </Modal.Title>
      </Modal.Header>

      <Modal.Body>
        {currentStep > validationSteps.ready && (
          <Tab.Container activeKey={currentTab}>
            <Row className="mb-2">
              <Col>
                <CertificationBreadcrumb
                  certifiedMessageType={certifiedMessageType}
                  currentStep={currentStep}
                  isProofValid={isProofValid}
                  isCertificateChainValid={isCertificateChainValid}
                  onStepClick={handleStepClick}
                />
                {showLoadingWarning && (
                  <Alert variant="warning" className="mt-2">
                    Verification is in progress. Please wait until the process is complete (less
                    than a minute).
                  </Alert>
                )}
              </Col>
            </Row>
            {/*Little hack: hide the modal content if the certificate modal is shown so this modal content*/}
            {/*never overflow behind the certificate modal.*/}
            <Row className={selectedCertificateHash ? "d-none" : ""}>
              <Col>
                <Tab.Content>
                  <Tab.Pane eventKey={getTabForStep(validationSteps.fetchingProof)}>
                    <FetchingProofPane
                      certifiedMessageType={certifiedMessageType}
                      itemHashes={hashes}
                    />
                  </Tab.Pane>
                  <Tab.Pane eventKey={getTabForStep(validationSteps.validatingProof)}>
                    <ValidatingProofPane
                      certifiedMessageType={certifiedMessageType}
                      isProofValid={isProofValid}
                    />
                  </Tab.Pane>
                  <Tab.Pane eventKey={getTabForStep(validationSteps.validatingCertificateChain)}>
                    {currentStep >= validationSteps.validatingCertificateChain && isProofValid && (
                      <CertificateVerifier
                        client={client}
                        certificate={certificate}
                        onStepChange={(step) => setCertificateVerifierStep(step)}
                        onChainValidationError={() => setIsCertificateChainValid(false)}
                        onCertificateClick={handleCertificateClick}
                        showCertificateLinks
                        hideSpinner
                      />
                    )}
                  </Tab.Pane>
                  <Tab.Pane eventKey={getTabForStep(validationSteps.done)}>
                    {currentStep === validationSteps.done && currentError === undefined && (
                      <CertificationResult
                        certifiedMessageType={certifiedMessageType}
                        isSuccess={isProofValid && isCertificateChainValid}
                        certificate={certificate}
                        certifiedItems={certifiedItems}
                        nonCertifiedItems={nonCertifiedItems}
                      />
                    )}
                    {currentStep === validationSteps.done && currentError !== undefined && (
                      <Alert variant="danger" className="mb-2">
                        <Alert.Heading>
                          <i className="text-danger bi bi-shield-slash"></i> Mithril could not
                          certify the {certifiedMessageType.pluralName}
                        </Alert.Heading>
                        <p className="mb-2">
                          An error occurred during the verification process. Please try again.
                        </p>
                        <hr />
                        <pre className="mb-0">
                          <code>{currentError.toString()}</code>
                        </pre>
                      </Alert>
                    )}
                  </Tab.Pane>
                </Tab.Content>
              </Col>
            </Row>
          </Tab.Container>
        )}
      </Modal.Body>
      <Modal.Footer></Modal.Footer>

      <CertificateModal
        hash={selectedCertificateHash}
        onHashChange={(hash) => setSelectedCertificateHash(hash)}
        hideButtons
      />
    </Modal>
  );
}
