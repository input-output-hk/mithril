import React from "react";
import { OverlayTrigger, Tooltip } from "react-bootstrap";

function MithrilIcon({ tooltip, variant }) {
  return (
    <OverlayTrigger overlay={<Tooltip>{tooltip}</Tooltip>}>
      <span className={`badge bg-${variant} mithril-icon`}>
        <style jsx>
          {`
            .mithril-icon {
              padding: 0.15rem 0.35rem 0.05rem 0.35rem !important;
            }

            .mithril-icon i::before {
              content: url("/explorer/logo.svg");
              display: inline-block;
              width: 16px;
              height: auto;
              filter: invert(100%);
            }
          `}
        </style>
        <i></i>
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
