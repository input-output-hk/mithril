import React from 'react';
import {Button} from "react-bootstrap";

export default function RawJsonButton(props) {
  return (
      <Button variant="outline-secondary" target="_blank" {...props}>
        <img src="/explorer/filetype-json.svg" alt="raw json" /> 
      </Button>
  );
}