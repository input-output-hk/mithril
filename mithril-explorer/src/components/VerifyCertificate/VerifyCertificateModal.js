import { MithrilClient } from "@mithril-dev/mithril-client-wasm";
import { Modal } from "react-bootstrap";
import { useEffect, useState } from "react";
import CertificateVerifier, { certificateValidationSteps } from "./CertificateVerifier";
import { fetchGenesisVerificationKey } from "@/utils";
import { useSelector } from "react-redux";

export default function VerifyCertificateModal({ show, onClose, certificateHash }) {
  const currentAggregator = useSelector((state) => state.settings.selectedAggregator);
  const [loading, setLoading] = useState(false);
  const [showLoadingWarning, setShowLoadingWarning] = useState(false);
  const [client, setClient] = useState(undefined);
  const [certificate, setCertificate] = useState(undefined);
  const [isCacheEnabled, setIsCacheEnabled] = useState(false);

  useEffect(() => {
    if (show) {
      // Reset existing warning when shown
      setShowLoadingWarning(false);
      init(currentAggregator, certificateHash).catch((err) =>
        console.error("VerifyCertificateModal init error:", err),
      );
    }
  }, [show, currentAggregator, certificateHash]);

  useEffect(() => {
    if (!loading) {
      setShowLoadingWarning(false);
    }
  }, [loading]);

  async function init(aggregator, certificateHash) {
    const genesisVerificationKey = await fetchGenesisVerificationKey(aggregator);
    const isCacheEnabled = process.env.UNSTABLE === true;
    const client_options = process.env.UNSTABLE
      ? {
          unstable: true,
          enable_certificate_chain_verification_cache: isCacheEnabled,
        }
      : {};
    const client = new MithrilClient(aggregator, genesisVerificationKey, client_options);
    const certificate = await client.get_mithril_certificate(certificateHash);

    setClient(client);
    setCertificate(certificate);
    setIsCacheEnabled(isCacheEnabled);
  }

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
                isCacheEnabled={isCacheEnabled}
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
