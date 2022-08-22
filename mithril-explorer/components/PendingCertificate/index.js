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
              <div>Network: {pendingCertificate.beacon.network}</div>
              <div>Epoch: {pendingCertificate.beacon.epoch}</div>
              <div>Immutable File Number: {pendingCertificate.beacon.immutable_file_number}</div>
            </Card.Body>
          </Card>
          <Card>
            <Card.Body>
              <Card.Title>Protocol Parameters</Card.Title>
              <div>K: {pendingCertificate.protocol.k}</div>
              <div>M: {pendingCertificate.protocol.m}</div>
              <div>Phi: {pendingCertificate.protocol.phi_f}</div>
            </Card.Body>
          </Card>
          <Card>
            <Card.Body>
              <Card.Title>Signers</Card.Title>
              {pendingCertificate.signers.length === 0
                ? <div>No Signers registered</div>
                : <ListGroup>{pendingCertificate.signers.map(signer =>
                    <ListGroup.Item key={signer.party_id}>Party id: {signer.party_id}</ListGroup.Item>
                )}</ListGroup> 
              }
              <Card.Title>Next Signers</Card.Title>
              {pendingCertificate.next_signers.length === 0
                ? <div>No Signers registered</div>
                : <ListGroup>{pendingCertificate.next_signers.map(signer =>
                  <ListGroup.Item key={signer.party_id}>Party id: {signer.party_id}</ListGroup.Item>
                )}</ListGroup>
              }
            </Card.Body>
          </Card>
        </CardGroup>
      }
    </div>
  );
}
