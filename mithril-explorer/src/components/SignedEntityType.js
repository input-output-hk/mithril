import React from "react";
import { ListGroup, Table } from "react-bootstrap";
import { parseSignedEntity } from "@/utils";

export default function SignedEntityType({ signedEntityType, table = false }) {
  let beacon = parseSignedEntity(signedEntityType);

  return table ? (
    <Table className="mb-3">
      <tbody>
        <tr>
          <td colSpan={2}>
            <h6>{beacon.name}</h6>
          </td>
        </tr>
        {Object.entries(beacon.fields).map(([key, value]) => (
          <tr key={key}>
            <td>
              <em>{key}:</em>
            </td>
            <td>{value}</td>
          </tr>
        ))}
      </tbody>
    </Table>
  ) : (
    <ListGroup>
      <ListGroup.Item>
        <h6>{beacon.name}</h6>
      </ListGroup.Item>
      <ListGroup.Item>
        <ListGroup horizontal="xxl">
          {Object.entries(beacon.fields).map(([key, value]) => (
            <ListGroup.Item key={key}>
              {key}: {value}
            </ListGroup.Item>
          ))}
        </ListGroup>
      </ListGroup.Item>
    </ListGroup>
  );
}
