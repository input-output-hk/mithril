import { Modal } from "react-bootstrap";
import { useEffect, useState } from "react";
import CertificateVerifier, { certificateValidationSteps } from "./CertificateVerifier";

export default function VerifyCertificateModal({ show, onClose, certificateHash }) {
  const [loading, setLoading] = useState(false);
  const [showLoadingWarning, setShowLoadingWarning] = useState(false);

  useEffect(() => {
    if (show) {
      // Force hide warning when shown
      setShowLoadingWarning(false);
    }
  }, [show]);

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
            <CertificateVerifier
              onStepChange={(step) =>
                setLoading(step === certificateValidationSteps.validationInProgress)
              }
              certificateHash={certificateHash}
            />
          </>
        )}
      </Modal.Body>
      <Modal.Footer></Modal.Footer>
    </Modal>
  );
}
