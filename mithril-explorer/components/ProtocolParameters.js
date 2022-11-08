import React from 'react';
import {ListGroup} from "react-bootstrap";

export default function ProtocolParameters({protocolParameters}) {
  return (
    <ListGroup horizontal>
      <ListGroup.Item>k: {protocolParameters?.k}</ListGroup.Item>
      <ListGroup.Item>m: {protocolParameters?.m}</ListGroup.Item>
      <ListGroup.Item>f: {protocolParameters?.phi_f}</ListGroup.Item>
    </ListGroup>
  );
}
