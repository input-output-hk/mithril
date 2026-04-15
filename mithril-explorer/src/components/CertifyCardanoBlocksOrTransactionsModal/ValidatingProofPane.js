import React from "react";
import { Table } from "react-bootstrap";
import IconBadge from "#/IconBadge";

export default function ValidatingProofPane({ certifiedMessageType, isProofValid }) {
  return (
    <>
      <h4>Checking {certifiedMessageType.pluralName} proof validity and certificate matching</h4>
      <Table responsive striped>
        <thead>
          <tr>
            <th>Verification</th>
            <th>Verified</th>
          </tr>
        </thead>
        <tbody>
          <tr>
            <td>
              <span className="text-capitalize">{certifiedMessageType.pluralName}</span> proof
              validity
            </td>
            <td>
              {isProofValid ? (
                <IconBadge variant="success" tooltip="yes" icon="check-circle-fill" />
              ) : (
                <IconBadge variant="danger" tooltip="no" icon="x-circle" />
              )}
            </td>
          </tr>
          <tr>
            <td>Certificate matches {certifiedMessageType.pluralName} proof</td>
            <td>
              {isProofValid ? (
                <IconBadge variant="success" tooltip="yes" icon="check-circle-fill" />
              ) : (
                <IconBadge variant="danger" tooltip="no" icon="x-circle" />
              )}
            </td>
          </tr>
        </tbody>
      </Table>
    </>
  );
}
