import React from "react";
import { Alert, Table } from "react-bootstrap";
import IconBadge from "../IconBadge";
import CopyableHash from "../CopyableHash";

export function TransactionCertificationResult({
  isSuccess,
  certifiedTransactions,
  nonCertifiedTransactions,
}) {
  return (
    <>
      {isSuccess ? (
        <Alert variant="success" className="mb-0">
          <Alert.Heading>Mithril certified the transactions</Alert.Heading>
        </Alert>
      ) : (
        <Alert variant="danger" className="mb-0">
          <Alert.Heading>Mithril could not certify the transactions</Alert.Heading>
          <p className="mb-1">
            Either the transactions proof is invalid or all the transactions are not certified.
          </p>
          <p className="mb-0 fst-italic">
            <i className="bi bi-info-circle"></i> Mithril may still have to certify those
            transactions.
          </p>
        </Alert>
      )}
      <Table responsive striped>
        <thead>
          <tr>
            <th>Transaction Hash</th>
            <th>Certified</th>
          </tr>
        </thead>
        <tbody>
          {certifiedTransactions.map((tx) => (
            <tr key={tx}>
              <td>
                <CopyableHash hash={tx} />
              </td>
              <td>
                <IconBadge tooltip="Certified by Mithril" variant="success" icon="shield-lock" />
              </td>
            </tr>
          ))}
          {nonCertifiedTransactions.map((tx) => (
            <tr key={tx}>
              <td>
                <CopyableHash hash={tx} />
              </td>
              <td>
                <IconBadge tooltip="Not certified" variant="danger" icon="shield-slash" />
              </td>
            </tr>
          ))}
        </tbody>
      </Table>
      {isSuccess && nonCertifiedTransactions.length > 0 && (
        <p className="mb-0 fst-italic">
          <i className="bi bi-info-circle"></i> Some transactions could not be certified, Mithril
          may still have to certify those transactions or they don&apos;t exist in the blockchain.
        </p>
      )}
    </>
  );
}
