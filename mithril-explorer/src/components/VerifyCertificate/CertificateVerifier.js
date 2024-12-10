import React, { useEffect, useState } from "react";
import { Alert, Spinner, Table } from "react-bootstrap";
import { formatProcessDuration } from "@/utils";
import CopyableHash from "#/CopyableHash";
import CopyButton from "#/CopyButton";
import IconBadge from "#/IconBadge";
import LocalDateTime from "#/LocalDateTime";

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
  certificateValidatedFromCache: "CertificateValidatedFromCache",
  done: "CertificateChainValidated",
};

const eventPosition = {
  beforeTable: 1,
  inTable: 2,
  afterTable: 3,
};

function CertificateHash({ hash, onClick, showLink, linkVariant = "dark" }) {
  function clicked(event) {
    event.preventDefault();
    onClick(hash);
  }

  return showLink ? (
    <>
      <a href="#" target="_blank" className={`link-${linkVariant}`} onClick={clicked}>
        {hash}
      </a>{" "}
      <CopyButton textToCopy={hash} />
    </>
  ) : (
    <CopyableHash hash={hash} />
  );
}

export default function CertificateVerifier({
  client,
  certificate,
  hideSpinner = false,
  showCertificateLinks = false,
  onStepChange = (step) => {},
  onChainValidationError = (error) => {},
  onCertificateClick = (hash) => {},
}) {
  const [currentStep, setCurrentStep] = useState(certificateValidationSteps.ready);
  const [validationError, setValidationError] = useState(undefined);
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
    if (validationError) {
      onChainValidationError(validationError);
    }
  }, [validationError, onChainValidationError]);

  useEffect(() => {
    if (currentStep === certificateValidationSteps.ready) {
      setVerificationEvents([]);
      setValidationError(undefined);

      if (client && certificate) {
        setCurrentStep(certificateValidationSteps.validationInProgress);

        verifyCertificateChain(client, certificate.hash)
          .catch((err) => {
            console.error("Certificate Chain verification error:\n", err);
            setValidationError(err);
          })
          .finally(() => setCurrentStep(certificateValidationSteps.done));
      }
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
    let position = eventPosition.afterTable;

    switch (event.type) {
      case certificateChainValidationEvents.started:
        position = eventPosition.beforeTable;
        message = <>The certificate chain validation has started...</>;
        break;
      case certificateChainValidationEvents.certificateValidated:
        position = eventPosition.inTable;
        message = { certificateHash: event.payload.certificate_hash, cached: false };
        break;
      case certificateChainValidationEvents.certificateValidatedFromCache:
        position = eventPosition.inTable;
        message = { certificateHash: event.payload.certificate_hash, cached: true };
        break;
      case certificateChainValidationEvents.done:
        message = (
          <>
            The certificate chain is valid <i className="text-success bi bi-check-circle-fill"></i>
          </>
        );
        break;
      default:
        message = <>{event}</>;
        break;
    }

    setVerificationEvents((existingEvents) => [
      ...existingEvents,
      { id: nextVerifyEventId++, position: position, message: message },
    ]);
  }

  return (
    <>
      {Object.entries(certificate).length > 0 && (
        <div>
          <h4>Certificate Details</h4>
          <div>
            Certificate hash:{" "}
            <CertificateHash
              hash={certificate.hash}
              onClick={() => onCertificateClick(certificate.hash)}
              showLink={showCertificateLinks}
              linkVariant="primary"
            />
          </div>
          <div>Epoch: {certificate.epoch}</div>
          <div className="d-flex justify-content-between">
            <div>
              Sealed at: <LocalDateTime datetime={certificate.metadata.sealed_at} />
            </div>
            {currentStep === certificateValidationSteps.validationInProgress && (
              <div className="d-flex align-items-center">
                <div className="ms-1 pe-1">Verifying the certificate chain...</div>
                {!hideSpinner && <Spinner animation="border" variant="primary" />}
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
              {verificationEvents
                .filter((evt) => evt.position === eventPosition.beforeTable)
                .map((evt) => (
                  <div key={evt.id}>{evt.message}</div>
                ))}
              <Table className="my-2" responsive striped>
                <thead>
                  <tr>
                    <th>Certificate hash</th>
                    <th>Checked</th>
                  </tr>
                </thead>
                <tbody>
                  {verificationEvents
                    .filter((evt) => evt.position === eventPosition.inTable)
                    .map((evt) => (
                      <tr key={evt.id}>
                        <td>
                          <CertificateHash
                            hash={evt.message.certificateHash}
                            onClick={() => onCertificateClick(evt.message.certificateHash)}
                            showLink={showCertificateLinks}
                          />
                        </td>
                        <td>
                          {evt.message.cached ? (
                            <IconBadge tooltip="cached" variant="warning" icon="clock-fill" />
                          ) : (
                            <IconBadge tooltip="yes" variant="success" icon="check-circle-fill" />
                          )}
                        </td>
                      </tr>
                    ))}
                </tbody>
              </Table>
              {verificationEvents
                .filter((evt) => evt.position === eventPosition.afterTable)
                .map((evt) => (
                  <div key={evt.id}>{evt.message}</div>
                ))}
              {validationError !== undefined && (
                <Alert variant="danger" className="mt-2">
                  <Alert.Heading>
                    <i className="text-danger bi bi-shield-slash"></i> Invalid certificate chain
                  </Alert.Heading>
                  <div className={styles.error}>{validationError.toString()}</div>
                </Alert>
              )}
            </div>
          </div>
        </div>
      )}
    </>
  );
}
