import React from 'react';
import {Button, OverlayTrigger, Tooltip} from "react-bootstrap";

export default function RawJsonButton(props) {
  return (
    <OverlayTrigger overlay={<Tooltip>Raw JSON</Tooltip>}>
      <Button variant="outline-secondary" target="_blank" {...props}>
        <i className="bi bi-filetype-json" style={{color: 'black'}}></i>
      </Button>
    </OverlayTrigger>
  );
}