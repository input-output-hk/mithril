import { MithrilClient } from "@mithril-dev/mithril-client-wasm";
import React, { useEffect, useState } from "react";
import { Alert, Col, Modal, Row, Tab } from "react-bootstrap";
import { useSelector } from "react-redux";
import { fetchGenesisVerificationKey } from "../../utils";
import CertificateVerifier, { certificateValidationSteps } from "../VerifyCertificate/verifier";
import { TransactionCertificationBreadcrumb } from "./TransactionCertificationBreadcrumb";
import { TransactionCertificationResult } from "./TransactionCertificationResult";
import { FetchingProofPane } from "./FetchingProofPane";
import { ValidatingProofPane } from "./ValidatingProofPane";

export const validationSteps = {
  ready: 1,
  fetchingProof: 2,
  validatingCertificateChain: 3,
  validatingProof: 4,
  done: 5,
};

export default function CertifyCardanoTransactionsModal({
  transactionHashes,
  onHashesChange = (hash) => {},
}) {
  const currentAggregator = useSelector((state) => state.settings.selectedAggregator);
  const [certificate, setCertificate] = useState(undefined);
  const [certificateVerifierStep, setCertificateVerifierStep] = useState(
    certificateValidationSteps.ready,
  );
  const [transactionsProofs, setTransactionsProofs] = useState({});
  const [showLoadingWarning, setShowLoadingWarning] = useState(false);
  const [isEverythingValid, setIsEverythingValid] = useState(false);
  const [currentStep, setCurrentStep] = useState(validationSteps.ready);
  const [currentTab, setCurrentTab] = useState(getTabForStep(validationSteps.ready));

  useEffect(() => {
    setShowLoadingWarning(false);
    setIsEverythingValid(false);
    setCertificate(undefined);
    setCurrentStep(validationSteps.ready);

    if (transactionHashes?.length > 0) {
      setCurrentStep(validationSteps.fetchingProof);
      getTransactionsProofs(currentAggregator, transactionHashes)
        .then(() =>
          // Artificial wait to give the user a feel of the work load under-hood
          setTimeout(() => {
            setCurrentStep(validationSteps.validatingCertificateChain);
          }, 350),
        )
        .catch((err) => {
          console.error("Cardano Transactions Certification Error:", err);
          setCurrentStep(validationSteps.done);
        });
    }
  }, [currentAggregator, transactionHashes]);

  useEffect(() => {
    if (certificateVerifierStep === certificateValidationSteps.done) {
      setCurrentStep(validationSteps.validatingProof);
      setShowLoadingWarning(false);
    }
  }, [certificateVerifierStep]);

  useEffect(() => {
    setCurrentTab(getTabForStep(currentStep));
  }, [currentStep]);

  useEffect(() => {
    if (currentStep === validationSteps.validatingProof && certificate !== undefined) {
      verifyTransactionProofAgainstCertificate(currentAggregator, transactionsProofs, certificate)
        .then(() =>
          // Artificial wait to give the user a feel of the work load under-hood
          setTimeout(() => {
            setCurrentStep(validationSteps.done);
          }, 250),
        )
        .catch((err) => {
          console.error("Cardano Transactions Certification Error:", err);
          setCurrentStep(validationSteps.done);
        });
    }
  }, [currentAggregator, currentStep, transactionsProofs, certificate]);

  async function getTransactionsProofs(aggregator, transactionHashes) {
    const genesisVerificationKey = await fetchGenesisVerificationKey(aggregator);
    const client = new MithrilClient(aggregator, genesisVerificationKey);
    const proofs = await client.unstable.get_cardano_transaction_proofs(transactionHashes);

    setTransactionsProofs(proofs);
  }

  async function verifyTransactionProofAgainstCertificate(
    aggregator,
    transactionsProofs,
    certificate,
  ) {
    const genesisVerificationKey = await fetchGenesisVerificationKey(aggregator);
    const client = new MithrilClient(aggregator, genesisVerificationKey);
    // Verify proof validity if so get its protocol message
    const protocolMessage =
      await client.unstable.verify_cardano_transaction_proof_then_compute_message(
        transactionsProofs,
        certificate,
      );

    if ((await client.verify_message_match_certificate(protocolMessage, certificate)) === true) {
      setIsEverythingValid(true);
    }
  }

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

  function handleStepClick(step) {
    // Only allow navigation when the work is done
    if (currentStep === validationSteps.done) {
      setCurrentTab(getTabForStep(step));
    }
  }

  function closeIfNotRunning() {
    if (currentStep !== validationSteps.done) {
      setShowLoadingWarning(true);
    } else {
      onHashesChange([]);
    }
  }

  return (
    <Modal
      show={transactionHashes !== undefined && transactionHashes.length > 0}
      onHide={closeIfNotRunning}
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
          <>
            <Tab.Container activeKey={currentTab}>
              <Row className="mb-2">
                <Col>
                  <TransactionCertificationBreadcrumb
                    currentStep={currentStep}
                    isSuccess={isEverythingValid}
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
              <Row>
                <Col>
                  <Tab.Content>
                    <Tab.Pane eventKey={getTabForStep(validationSteps.fetchingProof)}>
                      <FetchingProofPane transactionHashes={transactionHashes} />
                    </Tab.Pane>
                    <Tab.Pane eventKey={getTabForStep(validationSteps.validatingCertificateChain)}>
                      {currentStep >= validationSteps.validatingCertificateChain && (
                        <CertificateVerifier
                          showSpinner={false}
                          onStepChange={(step) => setCertificateVerifierStep(step)}
                          onCertificateChange={(certificate) => setCertificate(certificate)}
                          certificateHash={transactionsProofs.certificate_hash}
                        />
                      )}
                    </Tab.Pane>
                    <Tab.Pane eventKey={getTabForStep(validationSteps.validatingProof)}>
                      <ValidatingProofPane isEverythingValid={isEverythingValid} />
                    </Tab.Pane>
                    <Tab.Pane eventKey={getTabForStep(validationSteps.done)}>
                      {currentStep === validationSteps.done && (
                        <TransactionCertificationResult
                          isSuccess={isEverythingValid}
                          certifiedTransactions={transactionsProofs.transactions_hashes}
                          nonCertifiedTransactions={transactionsProofs.non_certified_transactions}
                        />
                      )}
                    </Tab.Pane>
                  </Tab.Content>
                </Col>
              </Row>
            </Tab.Container>
          </>
        )}
      </Modal.Body>
      <Modal.Footer></Modal.Footer>
    </Modal>
  );
}
