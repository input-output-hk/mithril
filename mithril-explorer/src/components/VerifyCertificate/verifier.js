import { MithrilClient } from "@mithril-dev/mithril-client-wasm";
import { useEffect, useState } from "react";
import LocalDateTime from "../LocalDateTime";
import { Spinner } from "react-bootstrap";
import { useSelector } from "react-redux";
import { fetchGenesisVerificationKey, formatProcessDuration } from "../../utils";

import styles from "./styles.module.css";

let nextVerifyEventId = 0;

export default function CertificateVerifier({ certificateHash, ...props }) {
  const currentAggregator = useSelector((state) => state.settings.selectedAggregator);
  const [loading, setLoading] = useState(false);
  const [certificate, setCertificate] = useState({});
  const [verificationDuration, setVerificationDuration] = useState(null);
  const [verificationEvents, setVerificationEvents] = useState([]);

  useEffect(() => {
    const broadcast_channel = new BroadcastChannel("mithril-client");
    broadcast_channel.addEventListener("message", clientEventListener);

    return () => broadcast_channel.close();
  }, []);

  useEffect(() => {
    props.onLoadingChange(loading);
  }, [loading, props]);

  useEffect(() => {
    // Reset state if any
    setCertificate({});
    setVerificationEvents([]);

    if (certificateHash && !loading) {
      setLoading(true);

      buildClientAndVerifyChain(certificateHash).catch((err) =>
        console.error("Certificate Chain verification error", err),
      );
    }
  }, [certificateHash]); // eslint-disable-line react-hooks/exhaustive-deps

  async function buildClientAndVerifyChain(certificate_hash) {
    try {
      const client = await initializeClient();

      if (certificate_hash !== null && certificate_hash !== undefined) {
        let startTime = performance.now();
        const certificate = await client.get_mithril_certificate(certificate_hash);
        setCertificate(certificate);
        await client.verify_certificate_chain(certificate_hash);
        setVerificationDuration(formatProcessDuration(startTime));
      }
    } catch (error) {
      console.error("Error:", error);
    } finally {
      setLoading(false);
    }
  }

  async function initializeClient() {
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

  return (
    <>
      {Object.entries(certificate).length > 0 && (
        <>
          <h4>Certificate Details</h4>
          <div>Certificate hash: {certificate.hash}</div>
          <div className="d-flex justify-content-between">
            <div>
              Sealed at: <LocalDateTime datetime={certificate.metadata.sealed_at} />
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
    </>
  );
}
