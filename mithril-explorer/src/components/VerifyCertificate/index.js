import { Modal, Spinner } from "react-bootstrap";
import initMithrilClient, { MithrilClient } from "@mithril-dev/mithril-client-wasm";
import { useEffect, useRef, useState } from "react";
import { useSelector } from "react-redux";
import LocalDateTime from "../LocalDateTime";
import { formatProcessDuration, computeAggregatorNetworkFromUrl } from "../../utils";

export default function VerifyCertificate({ show, onClose, certificateHash }) {
  const currentAggregator = useSelector((state) => state.settings.selectedAggregator);
  const [loading, setLoading] = useState(false);
  const [certificateData, setCertificateData] = useState(null);
  const [verificationDuration, setVerificationDuration] = useState(null);
  const [showWarning, setShowWarning] = useState(false);

  // const controller = new AbortController();
  // const signal = controller.signal;
  const controller = useRef(new AbortController());

  useEffect(() => {
    async function buildClientAndVerifyChain() {
      controller.current = new AbortController();
      const signal = controller.current.signal;
      try {
        const client = await initializeClient();
        if (certificateHash !== null && certificateHash !== undefined) {
          let startTime = performance.now();
          const certificate = await client.get_mithril_certificate(certificateHash);
          setCertificateData(certificate);

          let verify_certificate_chain_promise = new Promise((resolve, reject) => {
            const abortHandler = () => {
              console.log("Abort handler called");
              reject(new DOMException("Aborted", "AbortError"));
            };
            signal.addEventListener("abort", abortHandler);

            client
              .verify_certificate_chain(certificateHash)
              .then(resolve)
              .catch(reject)
              .finally(() => {
                signal.removeEventListener("abort", abortHandler);
              });
          });

          // Execute promise with abort capability
          verify_certificate_chain_promise
            .then((result) => {
              console.log("'verify_certificate_chain_promise' completed:", result);
            })
            .catch((error) => {
              if (error.name === "AbortError") {
                console.log("'verify_certificate_chain_promise' was aborted.");
              } else {
                console.error("An error occurred in 'verify_certificate_chain_promise':", error);
              }
            });

          setVerificationDuration(formatProcessDuration(startTime));
        }
      } catch (error) {
        console.error("Error:", error);
      } finally {
        setLoading(false);
      }
    }

    const broadcast_channel = new BroadcastChannel("mithril-client");
    broadcast_channel.onmessage = eventListener;

    if (certificateHash && show) {
      setLoading(true);
      buildClientAndVerifyChain();
    } else {
      // Reset state when the modal is closed
      setCertificateData(null);
      setLoading(false);
    }

    return () => {
      // Cleanup: remove the previous event listener
      broadcast_channel.onmessage = null;
    };
  }, [show]); // eslint-disable-line react-hooks/exhaustive-deps

  async function initializeClient() {
    await initMithrilClient();
    const genesisVerificationKey = await fetchGenesisVerificationKey();
    return new MithrilClient(currentAggregator, genesisVerificationKey);
  }

  async function fetchGenesisVerificationKey() {
    try {
      const network = computeAggregatorNetworkFromUrl(currentAggregator);
      const response = await fetch(
        "https://raw.githubusercontent.com/input-output-hk/mithril/main/mithril-infra/configuration/" +
          network +
          "/genesis.vkey",
      );

      if (!response.ok) {
        throw new Error(`Failed to fetch the genesis verification key. Status: ${response.status}`);
      }

      const genesisVerificationKey = await response.text();
      return genesisVerificationKey;
    } catch (error) {
      console.error("Error fetching genesis verification key:", error.message);
      throw error;
    }
  }

  function eventListener(e) {
    let event = e.data;
    if (event.type === "CertificateChainValidationStarted") {
      displayEventInDOM("The certificate chain validation has started...");
    } else if (event.type === "CertificateValidated") {
      displayEventInDOM(
        "A certificate has been validated, hash: <strong>" +
          event.payload.certificate_hash +
          "<strong>",
      );
    } else if (event.type === "CertificateChainValidated") {
      displayEventInDOM("<strong>The certificate chain is valid ✅<strong>");
    } else {
      displayEventInDOM(event);
    }
  }

  function displayEventInDOM(message) {
    let eventDiv = document.createElement("div");
    eventDiv.innerHTML = message;
    let mithrilEventsDiv = document.getElementById("mithril-events");
    mithrilEventsDiv &&
      (mithrilEventsDiv.appendChild(eventDiv),
      (mithrilEventsDiv.scrollTop = mithrilEventsDiv.scrollHeight));
  }

  const handleModalClose = () => {
    console.log("handleModalClose called");
    controller.current.abort();
    onClose();
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
        {certificateData && (
          <>
            <h4>Certificate Details</h4>
            <div>Certificate hash: {certificateData.hash}</div>
            <div style={{ display: "flex", justifyContent: "space-between" }}>
              <div>
                Sealed at: <LocalDateTime datetime={certificateData.metadata.sealed_at} />
              </div>
              {loading ? (
                <div style={{ display: "flex", alignItems: "center" }}>
                  <div style={{ marginLeft: "10px", paddingRight: "10px" }}>
                    Verifying the certificate chain...
                  </div>
                  <Spinner animation="border" variant="primary" />
                </div>
              ) : (
                <div style={{ marginLeft: "10px", paddingRight: "10px" }}>
                  Verification duration: {verificationDuration}
                </div>
              )}
            </div>
            <hr />
            <div>
              <div
                id="mithril-events"
                style={{
                  height: "400px",
                  maxHeight: "400px",
                  overflowY: "auto",
                  marginTop: "10px",
                }}></div>
            </div>
          </>
        )}
      </Modal.Body>
      <Modal.Footer></Modal.Footer>
    </Modal>
  );
}
