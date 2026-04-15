import React, { useEffect, useState } from "react";
import { Alert, ListGroup, Table } from "react-bootstrap";
import BlockHash from "#/BlockHash";
import IconBadge from "#/IconBadge";
import TransactionHash from "#/TransactionHash";
import { certifiedMessageTypes } from "#/CertifyCardanoBlocksOrTransactionsModal/index";

function CertifiedDataBeacon({ certificate }) {
  return (
    <>
      <p className="mb-2 px-2 fw-light">
        The point of the Cardano chain used during the verification process.
      </p>
      <ListGroup horizontal>
        <ListGroup.Item>
          <span className="fst-italic">Epoch: </span>
          {certificate.epoch}
        </ListGroup.Item>
        <ListGroup.Item>
          <span className="fst-italic">Block number signed: </span>
          {certificate.signed_entity_type.CardanoBlocksTransactions[1]}
        </ListGroup.Item>
        <ListGroup.Item>
          <span className="fst-italic">Offset from the tip: </span>
          {certificate.signed_entity_type.CardanoBlocksTransactions[2]}
        </ListGroup.Item>
      </ListGroup>
    </>
  );
}

function CertificationStatusBadge({ certified }) {
  return certified ? (
    <IconBadge tooltip="Certified by Mithril" variant="success" icon="mithril" />
  ) : (
    <IconBadge tooltip="Not certified" variant="danger" icon="shield-slash-fill" />
  );
}

function BlocksTable({ blocks }) {
  return (
    <Table responsive striped>
      <thead>
        <tr>
          <th>Block Hash</th>
          <th>Block Number</th>
          <th>Slot Number</th>
          <th>Certified</th>
        </tr>
      </thead>
      <tbody>
        {blocks.map((block) => (
          <tr key={block.hash}>
            <td>
              <BlockHash hash={block.hash} />
            </td>
            <td>{block.block_number}</td>
            <td>{block.slot_number}</td>
            <td>
              <CertificationStatusBadge certified={block.certified} />
            </td>
          </tr>
        ))}
      </tbody>
    </Table>
  );
}

function TransactionsTable({ transactions }) {
  return (
    <Table responsive striped>
      <thead>
        <tr>
          <th>Transaction Hash</th>
          <th>Block Hash</th>
          <th>Block Number</th>
          <th>Slot Number</th>
          <th>Certified</th>
        </tr>
      </thead>
      <tbody>
        {transactions.map((tx) => (
          <tr key={tx.hash}>
            <td>
              <TransactionHash hash={tx.hash} />
            </td>
            <td>
              <BlockHash hash={tx.block_hash} />
            </td>
            <td>{tx.block_number}</td>
            <td>{tx.slot_number}</td>
            <td>
              <CertificationStatusBadge certified={tx.certified} />
            </td>
          </tr>
        ))}
      </tbody>
    </Table>
  );
}

function mapSortBlocks(certifiedBlocks, nonCertifiedBlocks, isSuccess) {
  const blocksList = certifiedBlocks
    .map((block) => ({
      hash: block.block_hash,
      block_number: block.block_number,
      slot_number: block.slot_number,
      // if the proof verification failed then even blocks that the proof
      // said are certified must be shown as not certified
      certified: isSuccess,
    }))
    .concat(nonCertifiedBlocks.map((block) => ({ hash: block, certified: false })));

  blocksList.sort((block1, block2) => {
    if (block1.hash < block2.hash) {
      return -1;
    } else if (block1.hash > block2.hash) {
      return 1;
    }
    return 0;
  });

  console.log("Blocs", blocksList);
  return blocksList;
}

function mapSortTransactions(certifiedTransactions, nonCertifiedTransactions, isSuccess) {
  const transactionsList = certifiedTransactions
    .map((tx) => ({
      hash: tx.transaction_hash,
      block_hash: tx.block_hash,
      block_number: tx.block_number,
      slot_number: tx.slot_number,
      // if the proof verification failed then even transactions that the proof
      // said are certified must be shown as not certified
      certified: isSuccess,
    }))
    .concat(nonCertifiedTransactions.map((tx) => ({ hash: tx, certified: false })));

  transactionsList.sort((tx1, tx2) => {
    if (tx1.hash < tx2.hash) {
      return -1;
    } else if (tx1.hash > tx2.hash) {
      return 1;
    }
    return 0;
  });

  console.log("Transactions:", transactionsList);
  return transactionsList;
}

export default function CertificationResult({
  certifiedMessageType,
  certificate,
  isSuccess,
  certifiedItems,
  nonCertifiedItems,
}) {
  const blocks =
    certifiedMessageType === certifiedMessageTypes.block
      ? mapSortBlocks(certifiedItems, nonCertifiedItems, isSuccess)
      : [];
  const transactions =
    certifiedMessageType === certifiedMessageTypes.transaction
      ? mapSortTransactions(certifiedItems, nonCertifiedItems, isSuccess)
      : [];

  return (
    <>
      {isSuccess ? (
        <Alert variant="success" className="mb-2">
          <Alert.Heading>
            <i className="mi mi-logo"></i> Mithril certified the {certifiedMessageType.pluralName}
          </Alert.Heading>
        </Alert>
      ) : (
        <Alert variant="danger" className="mb-2">
          <Alert.Heading>
            <i className="text-danger bi bi-shield-slash"></i> Mithril could not certify the
            {certifiedMessageType.pluralName}
          </Alert.Heading>
          <p className="mb-1">
            Either the {certifiedMessageType.pluralName} proof is invalid or none of the{" "}
            {certifiedMessageType.pluralName} is certified.
          </p>
          <p className="mb-0 fst-italic">
            <i className="bi bi-info-circle"></i> Mithril may still have to certify those
            {certifiedMessageType.pluralName}.
          </p>
        </Alert>
      )}
      <h5>Beacon</h5>
      <CertifiedDataBeacon certificate={certificate} />
      {blocks.length > 0 && (
        <>
          <h5 className="mt-2">Blocks</h5>
          <BlocksTable blocks={blocks} />
        </>
      )}
      {transactions.length > 0 && (
        <>
          <h5 className="mt-2">Transactions</h5>
          <TransactionsTable transactions={transactions} />
        </>
      )}
      {isSuccess && nonCertifiedItems.length > 0 && (
        <p className="mb-0 fst-italic">
          <i className="bi bi-info-circle"></i> Some {certifiedMessageType.pluralName} could not be
          certified, Mithril may still have to certify those {certifiedMessageType.pluralName} or
          they don&apos;t exist in the blockchain.
        </p>
      )}
    </>
  );
}
