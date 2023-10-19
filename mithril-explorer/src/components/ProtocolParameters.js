import React from "react";
import { ListGroup } from "react-bootstrap";

export default function ProtocolParameters({ protocolParameters, ...props }) {
  return (
    <ListGroup horizontal {...props}>
      <ListGroup.Item>
        <em>k:</em> {protocolParameters?.k}
      </ListGroup.Item>
      <ListGroup.Item>
        <em>m:</em> {protocolParameters?.m}
      </ListGroup.Item>
      <ListGroup.Item>
        <em>f:</em> {protocolParameters?.phi_f}
      </ListGroup.Item>
    </ListGroup>
  );
}
