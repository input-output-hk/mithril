import { Modal } from "react-bootstrap";
import { useEffect, useRef, useState } from "react";
import CertificateVerifier from "./verifier";

export default function VerifyCertificateModal({ show, onClose, certificateHash }) {
  const verifier = useRef(null);
  const [showWarning, setShowWarning] = useState(false);

  useEffect(() => {
    if (show) {
      // Force hide warning when shown
      setShowWarning(false);
    }
  }, [show]);

  const handleModalClose = () => {
    // Only allow closing if not loading
    if (verifier.current.loading) {
      setShowWarning(true);
    } else {
      onClose();
    }
  };

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
          <CertificateVerifier
            stateRef={verifier}
            certificateHash={certificateHash}
            showLoadingWarning={showWarning}
          />
        )}
      </Modal.Body>
      <Modal.Footer></Modal.Footer>
    </Modal>
  );
}
