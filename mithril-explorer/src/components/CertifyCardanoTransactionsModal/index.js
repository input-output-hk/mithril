import { MithrilClient } from "@mithril-dev/mithril-client-wasm";
import React, { useEffect, useState } from "react";
import { Container, Modal, Stack } from "react-bootstrap";
import { useSelector } from "react-redux";
import { fetchGenesisVerificationKey } from "../../utils";
import CertificateVerifier, { certificateValidationSteps } from "../VerifyCertificate/verifier";
import { TransactionCertificationBreadcrumb } from "./TransactionCertificationBreadcrumb";

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

  useEffect(() => {
    setShowLoadingWarning(false);
    setIsEverythingValid(false);
    setCertificate(undefined);
    setCurrentStep(validationSteps.ready);

    if (transactionHashes?.length > 0) {
      setCurrentStep(validationSteps.fetchingProof);
      getTransactionsProofs(currentAggregator, transactionHashes).catch((err) => {
        console.error("Cardano Transactions Certification Error:", err);
        setCurrentStep(validationSteps.done);
      });
      setCurrentStep(validationSteps.validatingCertificateChain);
    }
  }, [currentAggregator, transactionHashes]);

  useEffect(() => {
    if (certificateVerifierStep === certificateValidationSteps.done) {
      setCurrentStep(validationSteps.validatingProof);
      setShowLoadingWarning(false);
    }
  }, [certificateVerifierStep]);

  useEffect(() => {
    if (currentStep === validationSteps.validatingProof && certificate !== undefined) {
      verifyTransactionProofAgainstCertificate(currentAggregator, transactionsProofs, certificate)
        .catch((err) => {
          console.error("Cardano Transactions Certification Error:", err);
        })
        .finally(() => {
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

  function closeIfNotRunning() {
    if (certificateVerifierStep === certificateValidationSteps.validationInProgress) {
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
          <Stack gap={1}>
            <TransactionCertificationBreadcrumb
              currentStep={currentStep}
              isSuccess={isEverythingValid}
            />

            {showLoadingWarning && (
              <div className="alert alert-warning mb-0 mt-2" role="alert">
                Verification is in progress. Please wait until the process is complete (less than a
                minute).
              </div>
            )}

            {currentStep >= validationSteps.fetchingProof && (
              <>
                <div>Transactions Certified: {transactionsProofs.transactions_hashes}</div>
                <div>
                  Transactions not certified: {transactionsProofs.non_certified_transactions}
                </div>
                <div>Certificate hash: {transactionsProofs.certificate_hash}</div>
              </>
            )}

            {currentStep >= validationSteps.validatingCertificateChain && (
              <>
                <hr />
                <CertificateVerifier
                  onStepChange={(step) => setCertificateVerifierStep(step)}
                  onCertificateChange={(certificate) => setCertificate(certificate)}
                  certificateHash={transactionsProofs.certificate_hash}
                />
              </>
            )}

            {currentStep === validationSteps.done && isEverythingValid && <>Success</>}
            {currentStep === validationSteps.done && !isEverythingValid && <>Failure</>}
          </Stack>
        )}
        <Container></Container>
      </Modal.Body>
      <Modal.Footer></Modal.Footer>
    </Modal>
  );
}
