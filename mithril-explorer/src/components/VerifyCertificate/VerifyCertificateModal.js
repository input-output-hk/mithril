import { Modal } from "react-bootstrap";
import { useState, useEffect } from "react";
import CertificateVerifier from "./CertificateVerifier";
import { useSelector } from "react-redux";

export default function VerifyCertificateModal({ show, onClose, certificateHash }) {
  const currentAggregator = useSelector((state) => state.settings.selectedAggregator);
  const [loading, setLoading] = useState(false);
  const [showLoadingWarning, setShowLoadingWarning] = useState(false);
  const [verificationContext, setVerificationContext] = useState(undefined);

  useEffect(() => {
    if (show) {
      const {
        fetchGenesisVerificationKey,
        newMithrilWasmClient,
      } = require("@/wasm-client-helpers");

      fetchGenesisVerificationKey(currentAggregator)
        .then((genesisKey) => newMithrilWasmClient(currentAggregator, genesisKey))
        .then((client) => {
          client.get_mithril_certificate(certificateHash).then((certificate) => {
            setVerificationContext({
              client: client,
              certificate: certificate,
            });
          });
        })
        .catch((err) => console.error("VerifyCertificateModal init error:", err));
    }
  }, [show, currentAggregator, certificateHash]);

  function handleModalClose() {
    // Only allow closing if not loading
    if (loading) {
      setShowLoadingWarning(true);
    } else {
      setShowLoadingWarning(false);
      setVerificationContext(undefined);
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
            {verificationContext && (
              <CertificateVerifier
                client={verificationContext.client}
                certificate={verificationContext.certificate}
                onStart={() => setLoading(true)}
                onDone={() => setLoading(false)}
              />
            )}
          </>
        )}
      </Modal.Body>
      <Modal.Footer></Modal.Footer>
    </Modal>
  );
}
