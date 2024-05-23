import React from "react";
import { Button, OverlayTrigger, Tooltip } from "react-bootstrap";

export default function CopyButton({ textToCopy, variant = "link", text = "" }) {
  function copyToClipboard() {
    if (window.isSecureContext && textToCopy) {
      navigator.clipboard.writeText(textToCopy).then(() => {});
    }
  }

  return (
    <OverlayTrigger overlay={<Tooltip>Copy</Tooltip>}>
      {variant === "link" ? (
        <Button variant="link" onClick={copyToClipboard} size="md" className="p-0">
          {text} <i className="bi bi-copy" style={{ color: "black" }}></i>
        </Button>
      ) : (
        <Button variant={variant} onClick={copyToClipboard} size="md">
          {text} <i className="bi bi-copy"></i>
        </Button>
      )}
    </OverlayTrigger>
  );
}
