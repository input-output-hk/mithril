import React from "react";
import { OverlayTrigger, Tooltip } from "react-bootstrap";

export default function IconBadge({ tooltip, icon, variant = "success" }) {
  return (
    <OverlayTrigger overlay={<Tooltip>{tooltip}</Tooltip>}>
      <span className={`badge bg-${variant}`}>
        <i className={`bi bi-${icon}`}></i>
      </span>
    </OverlayTrigger>
  );
}
