import React, { useState, useEffect } from 'react';
import { Badge, Button, Container, Col, ListGroup, Modal, OverlayTrigger, Row, Table, Tooltip } from "react-bootstrap";
import {useSelector} from "react-redux";
import RawJsonButton from "../RawJsonButton";
import VerifiedBadge from '../VerifiedBadge';

export default function CertificateModal(props) {
  const [certificate, setCertificate] = useState({});
  const aggregator = useSelector((state) => state.settings.selectedAggregator);

  useEffect(() => {
    if (!props.hash) {
      return;
    }

    fetch(`${aggregator}/certificate/${props.hash}`)
      .then(response => response.status === 200 ? response.json() : {})
      .then(data => setCertificate(data))
      .catch(error => {
        setCertificate({});
        console.error("Fetch certificate error:", error);
      });
  }, [aggregator, props.hash]);

  function showPrevious() {
    props.onHashChange(certificate.previous_hash);
  }

  function close() {
    props.onHashChange(undefined);
  }

  return (
    <Modal
      show={props.hash !== undefined}
      onHide={close}
      size="xl"
      aria-labelledby="contained-modal-title-vcenter"
      centered>
      <Modal.Header className="text-break" closeButton>
        <Modal.Title id="contained-modal-title-vcenter">
          Certificate {certificate.hash}
        </Modal.Title>
      </Modal.Header>

      <Modal.Body>
        {Object.entries(certificate).length === 0
          ? <p>Not found</p>
          :
          <Container>
            <Row md={1} xl="auto">
              <Col xl={4}>
                <h4>Beacon</h4>
                <ListGroup className="margin-bottom--md" variant="flush">
                  <ListGroup.Item>Network: {certificate.beacon.network}</ListGroup.Item>
                  <ListGroup.Item>Epoch: {certificate.beacon.epoch}</ListGroup.Item>
                  <ListGroup.Item>Immutable File Number: {certificate.beacon.immutable_file_number}</ListGroup.Item>
                </ListGroup>
                <h4>Protocol Parameters</h4>
                <ListGroup horizontal>
                  <ListGroup.Item>K: {certificate.metadata.parameters.k}</ListGroup.Item>
                  <ListGroup.Item>M: {certificate.metadata.parameters.m}</ListGroup.Item>
                  <ListGroup.Item>Phi: {certificate.metadata.parameters.phi_f}</ListGroup.Item>
                </ListGroup>
              </Col>
              <Col xl={8}>
                <h4>Signers</h4>
                {certificate.genesis_signature !== ""
                  ?
                  <div>
                    This is the chain Genesis Certificate, since it&aops;s manually created it doesn&apos;t contain any Signers.
                  </div>
                  : certificate.metadata.signers.length === 0
                    ?
                    <div>
                      No Signers for this certificate, something went wrong either with the data retrieval or the signing process
                    </div>
                    : <>
                      <Table responsive>
                        <thead>
                          <tr>
                            <th></th>
                            <th>Party id</th>
                            <th>Stake</th>
                          </tr>
                        </thead>
                        <tbody>
                          {certificate.metadata.signers.map(signer =>
                            <tr key={signer.party_id}>
                              <td>
                                {signer.verification_key_signature &&
                                  <VerifiedBadge tooltip="Verified Signer" />
                                }
                              </td>
                              <td>{signer.party_id}</td>
                              <td>{signer.stake}</td>
                            </tr>
                          )}
                        </tbody>
                      </Table>
                    </>
                }
              </Col>
            </Row>
          </Container>
        }
      </Modal.Body>
      <Modal.Footer>
        {certificate.genesis_signature !== ""
          ? <Badge bg="warning">Genesis</Badge>
          : <>
            <Button size="sm" onClick={showPrevious} className="text-break">Previous hash: {certificate.previous_hash}</Button>
          </>
        }
        <RawJsonButton href={`${aggregator}/certificate/${props.hash}`} size="sm" />
      </Modal.Footer>
    </Modal>
  );
}
