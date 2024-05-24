import React from "react";
import { Table } from "react-bootstrap";
import CopyableHash from "#/CopyableHash";
import VerifiedBadge from "#/VerifiedBadge";
import PoolTicker from "#/PoolTicker";
import Stake from "#/Stake";

// Display a table of signers (they must have a party_id and a stake property)
export default function SignerTable({ signers, displayIndexes, ...props }) {
  return (
    <Table responsive striped {...props}>
      <thead>
        <tr>
          {displayIndexes !== undefined && <th>#</th>}
          <th>Party id</th>
          <th>Pool Ticker</th>
          <th className="text-end">Stake</th>
        </tr>
      </thead>
      <tbody>
        {signers.map((signer, index) => (
          <tr key={signer.party_id}>
            {displayIndexes === true && <td>{index}</td>}
            <td>
              <VerifiedBadge /> <CopyableHash hash={signer.party_id} />
            </td>
            <td>
              <PoolTicker partyId={signer.party_id} />
            </td>
            <td className="text-end">
              <Stake lovelace={signer.stake} />
            </td>
          </tr>
        ))}
      </tbody>
    </Table>
  );
}
