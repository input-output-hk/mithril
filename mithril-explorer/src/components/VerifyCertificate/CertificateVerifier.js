import { useEffect, useState } from "react";
import LocalDateTime from "../LocalDateTime";
import { Spinner } from "react-bootstrap";
import { formatProcessDuration } from "../../utils";

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
  client,
  certificate,
  showSpinner = true,
  onStepChange = (step) => {},
}) {
  const [currentStep, setCurrentStep] = useState(certificateValidationSteps.ready);
  const [verificationDuration, setVerificationDuration] = useState(null);
  const [verificationEvents, setVerificationEvents] = useState([]);

  useEffect(() => {
    const broadcast_channel = new BroadcastChannel("mithril-client");
    broadcast_channel.addEventListener("message", clientEventListener);

    return () => broadcast_channel.close();
  }, []);

  useEffect(() => {
    onStepChange(currentStep);
  }, [currentStep, onStepChange]);

  useEffect(() => {
    switch (currentStep) {
      case certificateValidationSteps.ready:
        setVerificationEvents([]);
        if (client && certificate) {
          setCurrentStep(certificateValidationSteps.validationInProgress);

          verifyCertificateChain(client, certificate.hash)
            .catch((err) => {
              console.error("Certificate Chain verification error", err);
            })
            .finally(() => {
              setCurrentStep(certificateValidationSteps.done);
            });
        }
        break;
      case certificateValidationSteps.validationInProgress:
      case certificateValidationSteps.done:
      default:
        break;
    }
  }, [currentStep, client, certificate]);

  async function verifyCertificateChain(client, certificateHash) {
    let startTime = performance.now();
    await client.verify_certificate_chain(certificateHash);
    setVerificationDuration(formatProcessDuration(startTime));
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
          <div>Epoch: {certificate.beacon.epoch}</div>
          <div className="d-flex justify-content-between">
            <div>
              Sealed at: <LocalDateTime datetime={certificate.metadata.sealed_at} />
            </div>
            {currentStep === certificateValidationSteps.validationInProgress && (
              <div className="d-flex align-items-center">
                <div className="ms-1 pe-1">Verifying the certificate chain...</div>
                {showSpinner && <Spinner animation="border" variant="primary" />}
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
