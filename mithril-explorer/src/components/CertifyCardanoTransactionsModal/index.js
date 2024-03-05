import { MithrilClient } from "@mithril-dev/mithril-client-wasm";
import React, { useEffect, useState } from "react";
import { Container, Modal } from "react-bootstrap";
import { useSelector } from "react-redux";
import { fetchGenesisVerificationKey } from "../../utils";
import CertificateVerifier from "../VerifyCertificate/verifier";

export default function CertifyCardanoTransactionsModal({ transactionHashes, ...props }) {
  const currentAggregator = useSelector((state) => state.settings.selectedAggregator);
  const [loading, setLoading] = useState(false);
  const [transactionsProofs, setTransactionsProofs] = useState({});
  const [showLoadingWarning, setShowLoadingWarning] = useState(false);

  useEffect(() => {
    setShowLoadingWarning(false);

    if (transactionHashes?.length > 0) {
      getTransactionsProofs(currentAggregator, transactionHashes).catch((err) => {
        console.error("Cardano Transactions Certification Error:", err);
      });
    }
  }, [currentAggregator, transactionHashes]);

  useEffect(() => {
    if (!loading) {
      setShowLoadingWarning(false);
    }
  }, [loading]);

  async function getTransactionsProofs(aggregator, transactionHashes) {
    const genesisVerificationKey = await fetchGenesisVerificationKey(aggregator);
    const client = new MithrilClient(aggregator, genesisVerificationKey);
    const proofs = await client.unstable.get_cardano_transaction_proofs(transactionHashes);

    setTransactionsProofs(proofs);
  }

  function closeIfNotRunning() {
    // Only allow closing if not loading
    if (loading) {
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
        {Object.entries(transactionsProofs).length > 0 && (
          <>
            {showLoadingWarning && (
              <div className="alert alert-warning" role="alert">
                Verification is in progress. Please wait until the process is complete (less than a
                minute).
              </div>
            )}

            <div>Transactions Certified: {transactionsProofs.transactions_hashes}</div>
            <div>Transactions not certified: {transactionsProofs.non_certified_transactions}</div>
            <div>Certificate hash: {transactionsProofs.certificate_hash}</div>

            <CertificateVerifier
              onLoadingChange={(loading) => setLoading(loading)}
              certificateHash={transactionsProofs.certificate_hash}
            />
          </>
        )}
        <Container></Container>
      </Modal.Body>
      <Modal.Footer></Modal.Footer>
    </Modal>
  );
}
