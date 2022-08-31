import React, { useState, useEffect } from 'react';
import {Card, CardGroup, ListGroup} from "react-bootstrap";

export default function PendingCertificate(props) {
  const [pendingCertificate, setPendingCertificate] = useState({});

  useEffect(() => {
    if (!props.autoUpdate) {
      return;
    }
    
    let fetchPendingCertificate = () => {
      fetch(`${props.aggregator}/certificate-pending`)
        .then(response => response.status === 200 ? response.json() : {})
        .then(data => setPendingCertificate(data))
        .catch(error => {
          setPendingCertificate({});
          console.error("Fetch certificate-pending error:", error);
        });
    };
    
    // Fetch it once without waiting
    fetchPendingCertificate();
    
    const interval = setInterval(fetchPendingCertificate, props.updateInterval);
    return () => clearInterval(interval);
  }, [props.aggregator, props.updateInterval, props.autoUpdate]);

  return (
    <div className={props.className}>
      <h2>Pending Certificate</h2>
      
      {Object.entries(pendingCertificate).length === 0
        ? <p>No pending certificate available</p>
        :
        <CardGroup>
          <Card>
            <Card.Body>
              <Card.Title>Beacon</Card.Title>
              <ListGroup className="margin-bottom--md" variant="flush">
                <ListGroup.Item>Network: {pendingCertificate.beacon.network}</ListGroup.Item>
                <ListGroup.Item>Epoch: {pendingCertificate.beacon.epoch}</ListGroup.Item>
                <ListGroup.Item>Immutable File Number: {pendingCertificate.beacon.immutable_file_number}</ListGroup.Item>
              </ListGroup>
              <Card.Title>Protocol Parameters</Card.Title>
              <ListGroup horizontal>
                <ListGroup.Item>K: {pendingCertificate.protocol.k}</ListGroup.Item>
                <ListGroup.Item>M: {pendingCertificate.protocol.m}</ListGroup.Item>
                <ListGroup.Item>Phi: {pendingCertificate.protocol.phi_f}</ListGroup.Item>
              </ListGroup>
            </Card.Body>
          </Card>
          <Card>
            <Card.Body>
              <Card.Title>Signers</Card.Title>
              {pendingCertificate.signers.length === 0
                ? <div>No Signers registered</div>
                : <>
                  <ListGroup variant="flush">
                    <ListGroup.Item><b>Party id</b></ListGroup.Item>
                    {pendingCertificate.signers.map(signer =>
                      <ListGroup.Item key={signer.party_id}>{signer.party_id}</ListGroup.Item>
                    )}
                  </ListGroup>
                </>
              }
            </Card.Body>
          </Card>
          <Card>
            <Card.Body>
              <Card.Title>Next Signers</Card.Title>
              {pendingCertificate.next_signers.length === 0
                ? <div>No Signers registered for next epoch</div>
                : <>
                  <ListGroup variant="flush">
                    <ListGroup.Item><b>Party id</b></ListGroup.Item>
                    {pendingCertificate.next_signers.map(signer =>
                      <ListGroup.Item key={signer.party_id}>{signer.party_id}</ListGroup.Item>
                    )}
                  </ListGroup>
                </>
              }
            </Card.Body>
          </Card>
        </CardGroup>
      }
    </div>
  );
}
