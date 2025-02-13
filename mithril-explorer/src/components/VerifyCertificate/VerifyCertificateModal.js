import { Modal } from "react-bootstrap";
import { useEffect, useState } from "react";
import CertificateVerifier, { certificateValidationSteps } from "./CertificateVerifier";
import { useSelector } from "react-redux";

export default function VerifyCertificateModal({ show, onClose, certificateHash }) {
  const currentAggregator = useSelector((state) => state.settings.selectedAggregator);
  const [loading, setLoading] = useState(false);
  const [showLoadingWarning, setShowLoadingWarning] = useState(false);
  const [client, setClient] = useState(undefined);
  const [certificate, setCertificate] = useState(undefined);

  useEffect(() => {
    if (show) {
      const {
        fetchGenesisVerificationKey,
        newMithrilWasmClient,
      } = require("@/wasm-client-helpers");

      // Reset existing warning when shown
      setShowLoadingWarning(false);

      fetchGenesisVerificationKey(currentAggregator)
        .then((genesisKey) => newMithrilWasmClient(currentAggregator, genesisKey))
        .then((client) => {
          setClient(client);
          return client.get_mithril_certificate(certificateHash);
        })
        .then((certificate) => setCertificate(certificate))
        .catch((err) => console.error("VerifyCertificateModal init error:", err));
    }
  }, [show, currentAggregator, certificateHash]);

  useEffect(() => {
    if (!loading) {
      setShowLoadingWarning(false);
    }
  }, [loading]);

  function handleModalClose() {
    // Only allow closing if not loading
    if (loading) {
      setShowLoadingWarning(true);
    } else {
      onClose();
    }
  }

  return (
    <Modal
      show={show}
      onHide={handleModalClose}
      size="xl"
      aria-labelledby="contained-modal-title-vcenter"
      centered>
      <Modal.Header closeButton>
        <Modal.Title>Verify certificate</Modal.Title>
      </Modal.Header>
      <Modal.Body>
        {show && (
          <>
            {showLoadingWarning && (
              <div className="alert alert-warning" role="alert">
                Verification is in progress. Please wait until the process is complete (less than a
                minute).
              </div>
            )}
            {client && certificate && (
              <CertificateVerifier
                client={client}
                certificate={certificate}
                onStepChange={(step) =>
                  setLoading(step === certificateValidationSteps.validationInProgress)
                }
              />
            )}
          </>
        )}
      </Modal.Body>
      <Modal.Footer></Modal.Footer>
    </Modal>
  );
}
