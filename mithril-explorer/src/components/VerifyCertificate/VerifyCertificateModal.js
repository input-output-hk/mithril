import { Alert, Modal } from "react-bootstrap";
import { useState, useEffect } from "react";
import CertificateVerifier from "./CertificateVerifier";
import { useSelector } from "react-redux";

import styles from "./styles.module.css";

export default function VerifyCertificateModal({ show, onClose, certificateHash }) {
  const currentAggregator = useSelector((state) => state.settings.selectedAggregator);
  const [loading, setLoading] = useState(false);
  const [showLoadingWarning, setShowLoadingWarning] = useState(false);
  const [verificationContext, setVerificationContext] = useState(undefined);
  const [initError, setInitError] = useState(undefined);

  useEffect(() => {
    if (show) {
      const { newMithrilWasmClient } = require("@/wasm-client-helpers");

      newMithrilWasmClient(currentAggregator.url, currentAggregator.genesisVerificationKey)
        .then((client) => {
          client.get_mithril_certificate(certificateHash).then((certificate) => {
            setVerificationContext({
              client: client,
              certificate: certificate,
            });
          });
        })
        .catch((err) => {
          console.error("VerifyCertificateModal init error:", err);
          setInitError(err);
        });
    }
  }, [show, currentAggregator, certificateHash]);

  function handleModalClose() {
    // Only allow closing if not loading
    if (loading) {
      setShowLoadingWarning(true);
    } else {
      setShowLoadingWarning(false);
      setVerificationContext(undefined);
      setInitError(undefined);
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
            {initError !== undefined && (
              <Alert variant="danger" className="mt-2">
                <Alert.Heading>
                  <i className="text-danger bi bi-shield-slash"></i> Certificate chain verification
                  failed
                </Alert.Heading>
                <div className="my-2">
                  Initialization failed. Check that the genesis verification key is correct.
                </div>
                <div className={styles.error}>{initError.toString()}</div>
              </Alert>
            )}
          </>
        )}
      </Modal.Body>
      <Modal.Footer></Modal.Footer>
    </Modal>
  );
}
