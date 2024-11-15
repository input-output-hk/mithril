import React from "react";
import { Button, OverlayTrigger, Tooltip } from "react-bootstrap";

export default function RawJsonButton({ tooltip = "Raw JSON", color = "black", ...props }) {
  return (
    <OverlayTrigger overlay={<Tooltip>{tooltip}</Tooltip>}>
      <Button variant="outline-secondary" target="_blank" {...props}>
        <i className={`bi bi-filetype-json text-${color}`}></i>
      </Button>
    </OverlayTrigger>
  );
}
