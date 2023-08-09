import React from 'react';
import {OverlayTrigger, Tooltip} from "react-bootstrap";

export default function VerifiedBadge(props) {
  return (
    <OverlayTrigger overlay={<Tooltip>{props.tooltip}</Tooltip>}>
      <a href="#" className="badge bg-success">
        <i className="bi bi-shield-lock"></i>
      </a>
    </OverlayTrigger>
  );
}