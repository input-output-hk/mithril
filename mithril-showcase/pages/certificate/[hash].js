import React, { useState, useEffect } from 'react';
import {Card, CardGroup, ListGroup} from "react-bootstrap";
import {useRouter} from "next/router";
import Link from "next/link";

export default function Certificate(props) {
  const router = useRouter();
  const  { aggregator:aggregator, hash: hash } = router.query;
  const [certificate, setCertificate] = useState({});

  useEffect(() => {
    let fetchSnapshot = () => {
      fetch(`${aggregator}/certificate/${hash}`)
        .then(response => response.status === 200 ? response.json() : {})
        .then(data => setCertificate(data))
        .catch(error => {
          setCertificate({});
          console.error("Fetch certificate error:", error);
        });
    };

    fetchSnapshot();
  }, [aggregator, hash]);

  return (
    <div>
      <h2>Certificate</h2>

      {Object.entries(certificate).length === 0
        ? <p>Not found</p>
        :
        <CardGroup>
          <Card>
            <Card.Header>Certificate</Card.Header>
            <Card.Body>
              <div>Hash: {certificate.hash}</div>
              <div>Previous hash: 
                <Link href={"/certificate/" + certificate.previous_hash + "?aggregator=" + aggregator }>
                  {certificate.previous_hash}
                </Link>
              </div>
            </Card.Body>
          </Card>
          <Card>
            <Card.Body>
              <Card.Title>Beacon</Card.Title>
              <div>Network: {certificate.beacon.network}</div>
              <div>Epoch: {certificate.beacon.epoch}</div>
              <div>Immutable File Number: {certificate.beacon.immutable_file_number}</div>
            </Card.Body>
          </Card>
          <Card>
            <Card.Body>
              <Card.Title>Protocol Parameters</Card.Title>
              <div>K: {certificate.metadata.parameters.k}</div>
              <div>M: {certificate.metadata.parameters.m}</div>
              <div>Phi: {certificate.metadata.parameters.phi_f}</div>
            </Card.Body>
          </Card>
          <Card>
            <Card.Body>
              <Card.Title>Signers</Card.Title>
              {certificate.metadata.signers.length === 0
                ?
                <div>
                  No Signers for this certificate, something went wrong either with the data retrieval or the signing process
                </div>
                :
                <ListGroup>{certificate.metadata.signers.map(signer =>
                  <ListGroup.Item key={signer.party_id}>Party id: {signer.party_id} - stake: {signer.stake}</ListGroup.Item>
                )}
                </ListGroup>
              }
            </Card.Body>
          </Card>
        </CardGroup>
      }
    </div>
  );
}
