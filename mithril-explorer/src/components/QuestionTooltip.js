import React from "react";
import { OverlayTrigger, Tooltip } from "react-bootstrap";

export default function QuestionTooltip({ tooltip, ...props }) {
  return (
    <OverlayTrigger overlay={<Tooltip>{tooltip}</Tooltip>}>
      <span {...props}>
        <i className="bi bi-question-circle text-secondary"></i>
      </span>
    </OverlayTrigger>
  );
}
