import React, { useEffect, useState } from "react";
import { Alert, ListGroup, Table } from "react-bootstrap";
import IconBadge from "#/IconBadge";
import TransactionHash from "#/TransactionHash";

function CertifiedDataBeacon({ certificate }) {
  return (
    <>
      <p className="mb-2 px-2 fw-light">
        The point of the Cardano chain used during the verification process.
      </p>
      <ListGroup horizontal>
        <ListGroup.Item>
          <span className="fst-italic">Epoch: </span>
          {certificate.beacon.epoch}
        </ListGroup.Item>
        <ListGroup.Item>
          <span className="fst-italic">Immutable File Number: </span>
          {certificate.beacon.immutable_file_number}
        </ListGroup.Item>
      </ListGroup>
    </>
  );
}

function TransactionsTable({ transactions }) {
  return (
    <Table responsive striped>
      <thead>
        <tr>
          <th>Transaction Hash</th>
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
              {tx.certified ? (
                <IconBadge tooltip="Certified by Mithril" variant="success" icon="mithril" />
              ) : (
                <IconBadge tooltip="Not certified" variant="danger" icon="shield-slash-fill" />
              )}
            </td>
          </tr>
        ))}
      </tbody>
    </Table>
  );
}

export default function TransactionCertificationResult({
  certificate,
  isSuccess,
  certifiedTransactions,
  nonCertifiedTransactions,
}) {
  const [transactions, setTransactions] = useState([]);

  useEffect(() => {
    const transactionsList = certifiedTransactions
      .map((tx) => ({
        hash: tx,
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

    setTransactions(transactionsList);
  }, [isSuccess, certifiedTransactions, nonCertifiedTransactions]);

  return (
    <>
      {isSuccess ? (
        <Alert variant="success" className="mb-2">
          <Alert.Heading>Mithril certified the transactions</Alert.Heading>
        </Alert>
      ) : (
        <Alert variant="danger" className="mb-2">
          <Alert.Heading>
            <i className="text-danger bi bi-shield-slash"></i> Mithril could not certify the
            transactions
          </Alert.Heading>
          <p className="mb-1">
            Either the transactions proof is invalid or all the transactions are not certified.
          </p>
          <p className="mb-0 fst-italic">
            <i className="bi bi-info-circle"></i> Mithril may still have to certify those
            transactions.
          </p>
        </Alert>
      )}
      <h5>Beacon</h5>
      <CertifiedDataBeacon certificate={certificate} />
      <h5 className="mt-2">Transactions</h5>
      <TransactionsTable transactions={transactions} />
      {isSuccess && nonCertifiedTransactions.length > 0 && (
        <p className="mb-0 fst-italic">
          <i className="bi bi-info-circle"></i> Some transactions could not be certified, Mithril
          may still have to certify those transactions or they don&apos;t exist in the blockchain.
        </p>
      )}
    </>
  );
}
