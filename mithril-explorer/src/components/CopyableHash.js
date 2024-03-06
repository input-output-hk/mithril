import React from "react";
import { Button, OverlayTrigger, Tooltip } from "react-bootstrap";

export default function CopyableHash({ hash }) {
  function copyToClipboard() {
    if (window.isSecureContext && hash) {
      navigator.clipboard.writeText(hash).then(() => {});
    }
  }

  return (
    <span className="text-break">
      {hash}
      <> </>
      <OverlayTrigger overlay={<Tooltip>Copy</Tooltip>}>
        <Button variant="link" onClick={copyToClipboard} size="md" className="p-0">
          <i className="bi bi-copy" style={{ color: "black" }}></i>
        </Button>
      </OverlayTrigger>
    </span>
  );
}
