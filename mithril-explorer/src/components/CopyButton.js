import React, { useState, useEffect } from "react";
import { Button, OverlayTrigger, Tooltip } from "react-bootstrap";

export default function CopyButton({ textToCopy, variant = "link", text = "" }) {
  const [isCopying, setIsCopying] = useState(false);
  const [icon, setIcon] = useState("copy");

  useEffect(() => {
    setIcon(isCopying ? "check-circle" : "copy");
  }, [isCopying]);

  function copyToClipboard() {
    if (window.isSecureContext && textToCopy) {
      setIsCopying(true);
      navigator.clipboard
        .writeText(textToCopy)
        .finally(() => setTimeout(() => setIsCopying(false), 400));
    }
  }

  return (
    <OverlayTrigger overlay={<Tooltip>Copy</Tooltip>}>
      {variant === "link" ? (
        <Button variant="link" onClick={copyToClipboard} size="md" className="p-0">
          {text} <i className={`text-black bi bi-${icon}`}></i>
        </Button>
      ) : (
        <Button variant={variant} onClick={copyToClipboard} size="md">
          {text} <i className={`bi bi-${icon}`}></i>
        </Button>
      )}
    </OverlayTrigger>
  );
}
