import React from "react";
import { Table } from "react-bootstrap";
import IconBadge from "../IconBadge";

export default function ValidatingProofPane({ isProofValid }) {
  return (
    <>
      <h4>Checking transactions proof validity and certificate matching</h4>
      <Table responsive striped>
        <thead>
          <tr>
            <th>Verification</th>
            <th>Verified</th>
          </tr>
        </thead>
        <tbody>
          {["Transactions proof validity", "Certificate matches transactions proof"].map(
            (verification) => (
              <tr key={verification}>
                <td>{verification}</td>
                <td>
                  {isProofValid ? (
                    <IconBadge variant="success" tooltip="yes" icon="check-circle-fill" />
                  ) : (
                    <IconBadge variant="danger" tooltip="no" icon="x-circle" />
                  )}
                </td>
              </tr>
            ),
          )}
        </tbody>
      </Table>
    </>
  );
}
