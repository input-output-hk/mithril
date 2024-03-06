import { MithrilClient } from "@mithril-dev/mithril-client-wasm";
import React, { useEffect, useState } from "react";
import { Alert, Col, Modal, Row, Tab, Table } from "react-bootstrap";
import { useSelector } from "react-redux";
import { fetchGenesisVerificationKey } from "../../utils";
import CertificateVerifier, { certificateValidationSteps } from "../VerifyCertificate/verifier";
import { TransactionCertificationBreadcrumb } from "./TransactionCertificationBreadcrumb";
import { TransactionCertificationResult } from "./TransactionCertificationResult";
import CopyableHash from "../CopyableHash";

export const validationSteps = {
  ready: 1,
  fetchingProof: 2,
  validatingCertificateChain: 3,
  validatingProof: 4,
  done: 5,
};

export default function CertifyCardanoTransactionsModal({ transactionHashes, ...props }) {
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
        return `#step-${validationSteps.validatingCertificateChain}`;
      case validationSteps.validatingProof:
        return `#step-${validationSteps.validatingProof}`;
      case validationSteps.done:
        return `#step-${validationSteps.done}`;
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
      props.onHashesChange([]);
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
          Cardano transaction certification
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
                    <Tab.Pane eventKey={`#step-${validationSteps.fetchingProof}`}>
                      <h4>
                        Fetching a cryptographic proof that the transactions are part of the Cardano
                        transactions sets from the aggregator
                      </h4>
                      <Table responsive striped>
                        <thead>
                          <tr>
                            <th>Transaction Hash to certify</th>
                          </tr>
                        </thead>
                        <tbody>
                          {transactionHashes.map((tx) => (
                            <tr key={tx}>
                              <td>
                                <CopyableHash hash={tx} />
                              </td>
                            </tr>
                          ))}
                        </tbody>
                      </Table>
                    </Tab.Pane>
                    <Tab.Pane eventKey={`#step-${validationSteps.validatingCertificateChain}`}>
                      {currentStep >= validationSteps.validatingCertificateChain && (
                        <CertificateVerifier
                          onStepChange={(step) => setCertificateVerifierStep(step)}
                          onCertificateChange={(certificate) => setCertificate(certificate)}
                          certificateHash={transactionsProofs.certificate_hash}
                        />
                      )}
                    </Tab.Pane>
                    <Tab.Pane eventKey={`#step-${validationSteps.validatingProof}`}>
                      <h4>Checking cryptographic proof validity and certificate association</h4>
                      <p>The certificate chain associated to the cryptographic proof is valid.</p>
                      <p>
                        But the proof may not be associated to the certificate it claims to and its
                        cryptographic materials may not be valid.
                      </p>
                    </Tab.Pane>
                    <Tab.Pane eventKey={`#step-${validationSteps.done}`}>
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
