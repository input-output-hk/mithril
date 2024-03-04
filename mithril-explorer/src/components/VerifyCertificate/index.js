import { Modal, Spinner } from "react-bootstrap";
import initMithrilClient, { MithrilClient } from "@mithril-dev/mithril-client-wasm";
import { useEffect, useState } from "react";
import { useSelector } from "react-redux";
import LocalDateTime from "../LocalDateTime";
import { fetchGenesisVerificationKey, formatProcessDuration } from "../../utils";

import styles from "./styles.module.css";

let nextVerifyEventId = 0;

export default function VerifyCertificate({ show, onClose, certificateHash }) {
  const currentAggregator = useSelector((state) => state.settings.selectedAggregator);
  const [loading, setLoading] = useState(false);
  const [certificateData, setCertificateData] = useState({});
  const [verificationDuration, setVerificationDuration] = useState(null);
  const [showWarning, setShowWarning] = useState(false);
  const [verificationEvents, setVerificationEvents] = useState([]);

  useEffect(() => {
    async function buildClientAndVerifyChain() {
      try {
        const client = await initializeClient();
        if (certificateHash !== null && certificateHash !== undefined) {
          let startTime = performance.now();
          const certificate = await client.get_mithril_certificate(certificateHash);
          setCertificateData(certificate);
          await client.verify_certificate_chain(certificateHash);
          setVerificationDuration(formatProcessDuration(startTime));
        }
      } catch (error) {
        console.error("Error:", error);
      } finally {
        setLoading(false);
      }
    }

    const broadcast_channel = new BroadcastChannel("mithril-client");
    broadcast_channel.addEventListener("message", clientEventListener);

    if (certificateHash && show) {
      setLoading(true);
      buildClientAndVerifyChain().catch((err) =>
        console.error("Certificate Chain verification error", err),
      );
    } else {
      // Reset state when the modal is closed
      setCertificateData({});
      setLoading(false);
      setVerificationEvents([]);
    }

    return () => broadcast_channel.close();
  }, [show]); // eslint-disable-line react-hooks/exhaustive-deps

  async function initializeClient() {
    await initMithrilClient();
    const genesisVerificationKey = await fetchGenesisVerificationKey(currentAggregator);
    return new MithrilClient(currentAggregator, genesisVerificationKey);
  }

  function clientEventListener(e) {
    const event = e.data;
    let message = <>{event}</>;

    if (event.type === "CertificateChainValidationStarted") {
      message = <>The certificate chain validation has started...</>;
    } else if (event.type === "CertificateValidated") {
      message = (
        <>
          A certificate has been validated, hash: <strong>{event.payload.certificate_hash}</strong>
        </>
      );
    } else if (event.type === "CertificateChainValidated") {
      message = <>The certificate chain is valid ✅</>;
    }

    setVerificationEvents((old_evts) => [
      ...old_evts,
      { id: nextVerifyEventId++, message: message },
    ]);
  }

  const handleModalClose = () => {
    // Only allow closing if not loading
    if (loading) {
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
        {showWarning && loading && (
          <div className="alert alert-warning" role="alert">
            Verification is in progress. Please wait until the process is complete (less than a
            minute).
          </div>
        )}
        {Object.entries(certificateData).length > 0 && (
          <>
            <h4>Certificate Details</h4>
            <div>Certificate hash: {certificateData.hash}</div>
            <div className="d-flex justify-content-between">
              <div>
                Sealed at: <LocalDateTime datetime={certificateData.metadata.sealed_at} />
              </div>
              {loading ? (
                <div className="d-flex align-items-center">
                  <div className="ms-1 pe-1">Verifying the certificate chain...</div>
                  <Spinner animation="border" variant="primary" />
                </div>
              ) : (
                <div className="ms-1 pe-1">Verification duration: {verificationDuration}</div>
              )}
            </div>
            <hr />
            <div className={styles.events}>
              {/*don't remove: this span is needed for a css trick to ensure scroll start at top */}
              <span />
              <div>
                {verificationEvents.map((evt) => (
                  <div key={evt.id}>{evt.message}</div>
                ))}
              </div>
            </div>
          </>
        )}
      </Modal.Body>
      <Modal.Footer></Modal.Footer>
    </Modal>
  );
}
