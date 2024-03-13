import React from "react";
import { Table } from "react-bootstrap";
import CopyableHash from "#/CopyableHash";

export default function FetchingProofPane({ transactionHashes }) {
  return (
    <>
      <h4>Fetching a transactions proof of membership from the aggregator</h4>
      <Table responsive striped>
        <thead>
          <tr>
            <th>Transaction Hash to certify</th>
          </tr>
        </thead>
        <tbody>
          {transactionHashes.map((tx) => (
            <tr key={tx}>
              <td>
                <CopyableHash hash={tx} />
              </td>
            </tr>
          ))}
        </tbody>
      </Table>
    </>
  );
}
