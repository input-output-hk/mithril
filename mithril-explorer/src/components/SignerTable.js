import React from "react";
import { Table } from "react-bootstrap";
import PartyId from "./PartyId";
import VerifiedBadge from "./VerifiedBadge";
import PoolTicker from "./PoolTicker";
import Stake from "./Stake";

// Display a table of signers (they must have a party_id and a stake property)
export default function SignerTable({ signers, aggregator, displayIndexes, ...props }) {
  return (
    <Table responsive striped {...props}>
      <thead>
        <tr>
          {displayIndexes !== undefined && <th>#</th>}
          <th>Party id</th>
          <th>Pool Ticker</th>
          <th style={{ textAlign: "end" }}>Stake</th>
        </tr>
      </thead>
      <tbody>
        {signers.map((signer, index) => (
          <tr key={signer.party_id}>
            {displayIndexes === true && <td>{index}</td>}
            <td>
              <VerifiedBadge /> <PartyId partyId={signer.party_id} />
            </td>
            <td>
              <PoolTicker aggregator={aggregator} partyId={signer.party_id} />
            </td>
            <td style={{ textAlign: "end" }}>
              <Stake lovelace={signer.stake} />
            </td>
          </tr>
        ))}
      </tbody>
    </Table>
  );
}
