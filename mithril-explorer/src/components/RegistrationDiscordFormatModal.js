import React, { useEffect, useState } from "react";
import { Card, Modal } from "react-bootstrap";
import CopyButton from "#/CopyButton";
import { useSelector } from "react-redux";
import { getSelectedAggregatorPools } from "@/store/poolsSlice";

/**
 * Modal to show a list of registrations in a discord formatted code block
 *
 * @param registrations List of registrations movements
 * @param onClose Callback to call when the user ask to close the modal
 * @param mode 'in', 'out' or 'undefined': if undefined, the modal will not show
 */
export default function RegistrationDiscordFormatModal({ registrations, onClose, mode }) {
  const allPools = useSelector((state) => getSelectedAggregatorPools(state));
  const [textToCopy, setTextToCopy] = useState(undefined);

  useEffect(() => {
    if (mode === undefined) {
      return;
    }
    let text = "";

    for (const [epoch, movements] of Object.entries(registrations).reverse()) {
      const signers = mode === "in" ? movements.in : movements.out;
      if (signers.length === 0) {
        continue;
      }

      text += `Since epoch **#${epoch}**:\n`;
      for (const signer of signers
        .map((s) => ({
          party_id: s.party_id,
          pool_ticker: allPools?.find((p) => p.party_id === s.party_id)?.pool_ticker ?? "",
        }))
        .sort(compareSigners)) {
        text += `* ${signer.party_id}`;

        if (signer.pool_ticker !== "") {
          text += ` **${signer.pool_ticker}**`;
        }

        text += `\n`;
      }
    }

    setTextToCopy(text);
  }, [registrations, mode, allPools]);

  function compareSigners(left, right) {
    // Sort by pool_ticker then party_id
    return (
      left.pool_ticker.localeCompare(right.pool_ticker) ||
      left.party_id.localeCompare(right.party_id)
    );
  }

  function handleModalClose() {
    onClose();
  }

  return (
    <Modal
      show={mode !== undefined}
      onHide={handleModalClose}
      size="xl"
      aria-labelledby="contained-modal-title-vcenter"
      centered>
      <Modal.Header closeButton>
        <Modal.Title>
          <i className="bi bi-discord"></i> Discord formatted table of{" "}
          {mode === "out" ? "missing" : "new"} registrations
        </Modal.Title>
      </Modal.Header>
      <Modal.Body>
        {registrations !== undefined && (
          <Card>
            <Card.Body>
              <pre>
                <code>{textToCopy}</code>
              </pre>
            </Card.Body>
          </Card>
        )}
      </Modal.Body>
      <Modal.Footer>
        <CopyButton text="Copy to clipboard" variant="primary" textToCopy={textToCopy} />
      </Modal.Footer>
    </Modal>
  );
}
