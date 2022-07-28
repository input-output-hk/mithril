import React, { useState, useEffect } from 'react';
import {Badge, Button, Card, CardGroup, ListGroup, Modal} from "react-bootstrap";

export default function CertificateModal(props) {
  const [certificate, setCertificate] = useState({});

  useEffect(() => {
    if (!props.hash) {
      return;
    }
    
    fetch(`${props.aggregator}/certificate/${props.hash}`)
      .then(response => response.status === 200 ? response.json() : {})
      .then(data => setCertificate(data))
      .catch(error => {
        setCertificate({});
        console.error("Fetch certificate error:", error);
      });
  }, [props.aggregator, props.hash]);

  function showPrevious() {
    props.onHashChange(certificate.previous_hash);
  }
  
  function close() {
    props.onHashChange(undefined);
  }

  return (
    <Modal
      show={props.hash !== undefined}
      size="xl"
      aria-labelledby="contained-modal-title-vcenter"
      centered>
      <Modal.Header onHide={close} closeButton>
        <Modal.Title id="contained-modal-title-vcenter">
          Certificate {certificate.hash}
        </Modal.Title>
      </Modal.Header>

      <Modal.Body>
        {Object.entries(certificate).length === 0
          ? <p>Not found</p>
          :
          <CardGroup>
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
      </Modal.Body>
      <Modal.Footer>
        {certificate.previous_hash === ""
          ? <Badge bg="warning">Genesis</Badge>
          : <>
            <Button size="sm" onClick={showPrevious}>Previous hash: {certificate.previous_hash}</Button>
          </>
        }
      </Modal.Footer>
    </Modal>
  );
}
