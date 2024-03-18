import React from "react";
import { Button, OverlayTrigger, Tooltip } from "react-bootstrap";

export default function CopyButton({ textToCopy }) {
  function copyToClipboard() {
    if (window.isSecureContext && textToCopy) {
      navigator.clipboard.writeText(textToCopy).then(() => {});
    }
  }

  return (
    <OverlayTrigger overlay={<Tooltip>Copy</Tooltip>}>
      <Button variant="link" onClick={copyToClipboard} size="md" className="p-0">
        <i className="bi bi-copy" style={{ color: "black" }}></i>
      </Button>
    </OverlayTrigger>
  );
}
