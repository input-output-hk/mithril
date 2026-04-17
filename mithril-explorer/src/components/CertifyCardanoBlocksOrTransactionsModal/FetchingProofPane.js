import React from "react";
import { Table } from "react-bootstrap";
import CopyableHash from "#/CopyableHash";

export default function FetchingProofPane({ certifiedMessageType, itemHashes }) {
  return (
    <>
      <h4>Fetching a {certifiedMessageType.pluralName} proof of membership from the aggregator</h4>
      <Table responsive striped>
        <thead>
          <tr>
            <th>
              <span className="text-capitalize">{certifiedMessageType.name}</span> hash to certify
            </th>
          </tr>
        </thead>
        <tbody>
          {itemHashes.map((item) => (
            <tr key={item}>
              <td>
                <CopyableHash hash={item} />
              </td>
            </tr>
          ))}
        </tbody>
      </Table>
    </>
  );
}
