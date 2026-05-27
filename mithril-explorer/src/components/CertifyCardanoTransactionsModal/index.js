import React, { useEffect, useState } from "react";
import { Alert, Col, Modal, Row, Tab } from "react-bootstrap";
import { useSelector } from "react-redux";
import CertificateModal from "#/CertificateModal";
import CertificateVerifier, {
  certificateValidationSteps,
} from "#/VerifyCertificate/CertificateVerifier";
import TransactionCertificationBreadcrumb from "./TransactionCertificationBreadcrumb";
import TransactionCertificationResult from "./TransactionCertificationResult";
import FetchingProofPane from "./FetchingProofPane";
import ValidatingProofPane from "./ValidatingProofPane";

export const validationSteps = {
  ready: 1,
  fetchingProof: 2,
  validatingProof: 3,
  validatingCertificateChain: 4,
  done: 5,
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

export default function CertifyCardanoTransactionsModal({
  transactionHashes,
  onHashesChange = (hash) => {},
}) {
  const currentAggregator = useSelector((state) => state.settings.selectedAggregator);
  const [client, setClient] = useState(undefined);
  const [certificate, setCertificate] = useState(undefined);
  const [selectedCertificateHash, setSelectedCertificateHash] = useState(undefined);
  const [transactionsProofs, setTransactionsProofs] = useState({});
  const [showLoadingWarning, setShowLoadingWarning] = useState(false);
  const [isProofValid, setIsProofValid] = useState(false);
  const [isCertificateChainValid, setIsCertificateChainValid] = useState(true);
  const [currentStep, setCurrentStep] = useState(validationSteps.ready);
  const [selectedTab, setSelectedTab] = useState(undefined);
  const currentTab =
    currentStep === validationSteps.done && selectedTab !== undefined
      ? selectedTab
      : getTabForStep(currentStep);
  const [currentError, setCurrentError] = useState(undefined);

  async function getTransactionsProofsAndCertificate(client, transactionHashes) {
    const proofs = await client.get_cardano_transaction_proofs(transactionHashes);
    const certificate = await client.get_mithril_certificate(proofs.certificate_hash);

    return {
      proofs: proofs,
      certificate: certificate,
    };
  }

  async function verifyTransactionProofAgainstCertificate(client, transactionsProofs, certificate) {
    // Verify proof validity if so get its protocol message
    const protocolMessage = await client.verify_cardano_transaction_proof_then_compute_message(
      transactionsProofs,
      certificate,
    );
    const isProofValid =
      (await client.verify_message_match_certificate(protocolMessage, certificate)) === true;

    return isProofValid;
  }

  function handleError(error) {
    console.error("Cardano Transactions Certification Error:", error);
    setCurrentError(error);
    setCurrentStep(validationSteps.done);
  }

  function handleCertificateClick(hash) {
    // Only allow to open the submodal when the work is done
    if (currentStep === validationSteps.done) {
      setSelectedCertificateHash(hash);
    }
  }

  function handleCertificateVerifierDone() {
    setCurrentStep(validationSteps.done);
    setShowLoadingWarning(false);
  }

  function closeIfNotRunning() {
    if (currentStep !== validationSteps.done) {
      setShowLoadingWarning(true);
    } else {
      onHashesChange([]);
    }
  }

  function resetState() {
    setShowLoadingWarning(false);
    setIsProofValid(false);
    setIsCertificateChainValid(true);
    setCertificate(undefined);
    setCurrentStep(validationSteps.ready);
    setCurrentError(undefined);
  }

  useEffect(() => {
    if (transactionHashes?.length > 0) {
      const { newMithrilWasmClient } = require("@/wasm-client-helpers");

      newMithrilWasmClient(currentAggregator.url, currentAggregator.genesisVerificationKey)
        .then((client) => setClient(client))
        .then(() => setCurrentStep(validationSteps.fetchingProof))
        .catch((err) => handleError(err));
    }
  }, [currentAggregator, transactionHashes]);

  useEffect(() => {
    if (currentStep === validationSteps.fetchingProof) {
      getTransactionsProofsAndCertificate(client, transactionHashes)
        .then((res) => {
          setTransactionsProofs(res.proofs);
          setCertificate(res.certificate);

          // Artificial wait to give the user a feel of the workload under-hood
          setTimeout(() => {
            setCurrentStep(validationSteps.validatingProof);
          }, 350);
        })
        .catch((err) => handleError(err));
    }
  }, [client, currentStep, transactionHashes]);

  useEffect(() => {
    if (currentStep === validationSteps.validatingProof && certificate !== undefined) {
      verifyTransactionProofAgainstCertificate(client, transactionsProofs, certificate)
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
  }, [client, currentStep, transactionsProofs, certificate]);

  return (
    <Modal
      show={transactionHashes !== undefined && transactionHashes.length > 0}
      onHide={closeIfNotRunning}
      onShow={resetState}
      size="xl"
      aria-labelledby="contained-modal-title-vcenter"
      centered>
      <Modal.Header className="text-break" closeButton>
        <Modal.Title id="contained-modal-title-vcenter">
          Cardano Transactions Certification
        </Modal.Title>
      </Modal.Header>

      <Modal.Body>
        {currentStep > validationSteps.ready && (
          <Tab.Container activeKey={currentTab}>
            <Row className="mb-2">
              <Col>
                <TransactionCertificationBreadcrumb
                  currentStep={currentStep}
                  isProofValid={isProofValid}
                  isCertificateChainValid={isCertificateChainValid}
                  onStepClick={(step) => setSelectedTab(getTabForStep(step))}
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
                    <FetchingProofPane transactionHashes={transactionHashes} />
                  </Tab.Pane>
                  <Tab.Pane eventKey={getTabForStep(validationSteps.validatingProof)}>
                    <ValidatingProofPane isProofValid={isProofValid} />
                  </Tab.Pane>
                  <Tab.Pane eventKey={getTabForStep(validationSteps.validatingCertificateChain)}>
                    {currentStep >= validationSteps.validatingCertificateChain && isProofValid && (
                      <CertificateVerifier
                        client={client}
                        certificate={certificate}
                        onDone={handleCertificateVerifierDone}
                        onChainValidationError={() => setIsCertificateChainValid(false)}
                        onCertificateClick={handleCertificateClick}
                        showCertificateLinks
                        hideSpinner
                      />
                    )}
                  </Tab.Pane>
                  <Tab.Pane eventKey={getTabForStep(validationSteps.done)}>
                    {currentStep === validationSteps.done && currentError === undefined && (
                      <TransactionCertificationResult
                        isSuccess={isProofValid && isCertificateChainValid}
                        certificate={certificate}
                        certifiedTransactions={transactionsProofs.transactions_hashes}
                        nonCertifiedTransactions={transactionsProofs.non_certified_transactions}
                      />
                    )}
                    {currentStep === validationSteps.done && currentError !== undefined && (
                      <Alert variant="danger" className="mb-2">
                        <Alert.Heading>
                          <i className="text-danger bi bi-shield-slash"></i> Mithril could not
                          certify the transactions
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
