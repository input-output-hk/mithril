import React, { useEffect, useState } from "react";
import { Card, Modal } from "react-bootstrap";
import { useSelector } from "react-redux";
import { getSelectedAggregatorPools } from "@/store/poolsSlice";
import CopyButton from "#/CopyButton";

function compareSigners(left, right) {
  // Sort by pool_ticker then party_id
  return (
    left.pool_ticker.localeCompare(right.pool_ticker) || left.party_id.localeCompare(right.party_id)
  );
}

function formatMarkdown(registrations, mode, aggregatorPools) {
  if (mode === undefined) {
    return;
  }
  let markdown = "";

  for (const [epoch, movements] of Object.entries(registrations).reverse()) {
    const signers = mode === "in" ? movements.in : movements.out;
    if (signers.length === 0) {
      continue;
    }

    markdown += `Since epoch **#${epoch}**:\n`;
    for (const signer of signers
      .map((s) => ({
        party_id: s.party_id,
        pool_ticker:
          aggregatorPools?.pools?.find((p) => p.party_id === s.party_id)?.pool_ticker ?? "",
      }))
      .sort(compareSigners)) {
      markdown += `* ${signer.party_id}`;

      if (signer.pool_ticker !== "") {
        markdown += ` **${signer.pool_ticker}**`;
      }

      markdown += `\n`;
    }
  }

  return markdown;
}

/**
 * Modal to show a list of registrations in a Markdown formatted code block
 *
 * @param registrations List of registrations movements
 * @param onClose Callback to call when the user ask to close the modal
 * @param mode 'in', 'out' or 'undefined': if undefined, the modal will not show
 */
export default function RegistrationMarkdownFormatModal({ registrations, onClose, mode }) {
  const aggregatorPools = useSelector((state) => getSelectedAggregatorPools(state));
  const variant = mode === "out" ? "danger" : "success";
  const markdown = formatMarkdown(registrations, mode, aggregatorPools);

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
          <i className={`bi bi-markdown text-${variant}`}></i> Markdown formatted message of{" "}
          {mode === "out" ? "de-registered" : "newly registered"} signers
        </Modal.Title>
      </Modal.Header>
      <Modal.Body>
        {registrations !== undefined && (
          <Card bg="light" border={variant}>
            <Card.Body>
              <pre className="mb-0">
                <code>{markdown}</code>
              </pre>
            </Card.Body>
          </Card>
        )}
      </Modal.Body>
      <Modal.Footer>
        <CopyButton text="Copy to clipboard" variant="primary" textToCopy={markdown} />
      </Modal.Footer>
    </Modal>
  );
}
