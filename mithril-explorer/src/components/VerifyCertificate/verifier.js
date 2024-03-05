import { MithrilClient } from "@mithril-dev/mithril-client-wasm";
import { useEffect, useState } from "react";
import LocalDateTime from "../LocalDateTime";
import { Spinner } from "react-bootstrap";
import { useSelector } from "react-redux";
import { fetchGenesisVerificationKey, formatProcessDuration } from "../../utils";

import styles from "./styles.module.css";

let nextVerifyEventId = 0;

export const certificateValidationSteps = {
  ready: 1,
  validationInProgress: 2,
  done: 3,
};

const certificateChainValidationEvents = {
  started: "CertificateChainValidationStarted",
  certificateValidated: "CertificateValidated",
  done: "CertificateChainValidated",
};

export default function CertificateVerifier({
  certificateHash,
  onStepChange = (step) => {},
  onCertificateChange = (certificate) => {},
}) {
  const currentAggregator = useSelector((state) => state.settings.selectedAggregator);
  const [currentStep, setCurrentStep] = useState(certificateValidationSteps.ready);
  const [certificate, setCertificate] = useState({});
  const [verificationDuration, setVerificationDuration] = useState(null);
  const [verificationEvents, setVerificationEvents] = useState([]);

  useEffect(() => {
    const broadcast_channel = new BroadcastChannel("mithril-client");
    broadcast_channel.addEventListener("message", clientEventListener);

    return () => broadcast_channel.close();
  }, []);

  useEffect(() => {
    onCertificateChange(certificate);
  }, [certificate, onCertificateChange]);

  useEffect(() => {
    onStepChange(currentStep);
  }, [currentStep, onStepChange]);

  useEffect(() => {
    // Reset state if any
    setCertificate({});
    setVerificationEvents([]);
    setCurrentStep(certificateValidationSteps.ready);

    if (certificateHash) {
      setCurrentStep(certificateValidationSteps.validationInProgress);

      verifyCertificateChain(currentAggregator, certificateHash)
        .catch((err) => console.error("Certificate Chain verification error", err))
        .finally(() => {
          setCurrentStep(certificateValidationSteps.done);
        });
    }
  }, [currentAggregator, certificateHash]);

  async function verifyCertificateChain(aggregator, certificate_hash) {
    const genesisVerificationKey = await fetchGenesisVerificationKey(aggregator);
    const client = new MithrilClient(aggregator, genesisVerificationKey);

    if (certificate_hash !== null && certificate_hash !== undefined) {
      let startTime = performance.now();
      const certificate = await client.get_mithril_certificate(certificate_hash);
      setCertificate(certificate);
      await client.verify_certificate_chain(certificate_hash);
      setVerificationDuration(formatProcessDuration(startTime));
    }
  }

  function clientEventListener(e) {
    const event = e.data;
    let message = <></>;

    switch (event.type) {
      case certificateChainValidationEvents.started:
        message = <>The certificate chain validation has started...</>;
        break;
      case certificateChainValidationEvents.certificateValidated:
        message = (
          <>
            A certificate has been validated, hash:{" "}
            <strong>{event.payload.certificate_hash}</strong>
          </>
        );
        break;
      case certificateChainValidationEvents.done:
        message = <>The certificate chain is valid ✅</>;
        break;
      default:
        message = <>{event}</>;
        break;
    }

    setVerificationEvents((existingEvents) => [
      ...existingEvents,
      { id: nextVerifyEventId++, message: message },
    ]);
  }

  return (
    <>
      {Object.entries(certificate).length > 0 && (
        <div>
          <h4>Certificate Details</h4>
          <div>Certificate hash: {certificate.hash}</div>
          <div className="d-flex justify-content-between">
            <div>
              Sealed at: <LocalDateTime datetime={certificate.metadata.sealed_at} />
            </div>
            {currentStep === certificateValidationSteps.validationInProgress && (
              <div className="d-flex align-items-center">
                <div className="ms-1 pe-1">Verifying the certificate chain...</div>
                <Spinner animation="border" variant="primary" />
              </div>
            )}
            {currentStep === certificateValidationSteps.done && (
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
        </div>
      )}
    </>
  );
}
