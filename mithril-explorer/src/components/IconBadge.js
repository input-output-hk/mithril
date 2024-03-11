import React from "react";
import { OverlayTrigger, Tooltip } from "react-bootstrap";

function MithrilIcon({ tooltip, variant }) {
  return (
    <OverlayTrigger overlay={<Tooltip>{tooltip}</Tooltip>}>
      <span className={`badge bg-${variant}`}>
        <i className={`mi mi-logo`}></i>
      </span>
    </OverlayTrigger>
  );
}

function BootstrapIcon({ icon, tooltip, variant }) {
  return (
    <OverlayTrigger overlay={<Tooltip>{tooltip}</Tooltip>}>
      <span className={`badge bg-${variant}`}>
        <i className={`bi bi-${icon}`}></i>
      </span>
    </OverlayTrigger>
  );
}

export default function IconBadge({ icon, tooltip, variant = "success" }) {
  if (icon === "mithril") {
    return <MithrilIcon tooltip={tooltip} variant={variant} />;
  } else {
    return <BootstrapIcon icon={icon} tooltip={tooltip} variant={variant} />;
  }
}
