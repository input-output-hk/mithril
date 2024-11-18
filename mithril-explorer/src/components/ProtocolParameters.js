import React from "react";
import { ListGroup } from "react-bootstrap";

export default function ProtocolParameters({ protocolParameters, padding, ...props }) {
  const padding_class = `p-${padding ?? "auto"}`;

  return (
    <ListGroup horizontal {...props}>
      <ListGroup.Item className={`${padding_class}`}>
        <em>k:</em> {protocolParameters?.k}
      </ListGroup.Item>
      <ListGroup.Item className={`${padding_class}`}>
        <em>m:</em> {protocolParameters?.m}
      </ListGroup.Item>
      <ListGroup.Item className={`${padding_class}`}>
        <em>f:</em> {protocolParameters?.phi_f}
      </ListGroup.Item>
    </ListGroup>
  );
}
