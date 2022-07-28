import React, { useState, useEffect } from 'react';
import CertificateModal from '../CertificateModal';
import {Badge, Row, Col, Card, Container, Button} from "react-bootstrap";

/*
 * Code from: https://stackoverflow.com/a/18650828
 */
function formatBytes(bytes, decimals = 2) {
  if (bytes === 0) return '0 Bytes';

  const k = 1024;
  const dm = decimals < 0 ? 0 : decimals;
  const sizes = ['Bytes', 'KB', 'MB', 'GB', 'TB', 'PB', 'EB', 'ZB', 'YB'];

  const i = Math.floor(Math.log(bytes) / Math.log(k));

  return parseFloat((bytes / Math.pow(k, i)).toFixed(dm)) + ' ' + sizes[i];
}

export default function SnapshotsList(props) {
  const [snapshots, setSnapshots] = useState([]);
  const [selectedCertificateHash, setSelectedCertificateHash] = useState(undefined);

  useEffect(() => {
    if (!props.autoUpdate) {
      return;
    }
    
    let fetchSnapshots = () => {
      fetch(`${props.aggregator}/snapshots`)
        .then(response => response.json())
        .then(data => setSnapshots(data))
        .catch(error => {
          setSnapshots([]);
          console.error("Fetch snapshots error:", error);
        });
    };
    
    // Fetch them once without waiting
    fetchSnapshots(); 
    
    const interval = setInterval(fetchSnapshots, props.updateInterval);
    return () => clearInterval(interval);
  }, [props.aggregator, props.updateInterval, props.autoUpdate]);
  
  function handleCertificateHashChange(hash) {
    setSelectedCertificateHash(hash);
  }
  
  function showCertificate(hash) {
    setSelectedCertificateHash(hash);
  }

  return (
    <>
      <CertificateModal
        aggregator={props.aggregator}
        hash={selectedCertificateHash}
        onHashChange={handleCertificateHashChange} />
      
      <div className={props.className}>
        <h2>Snapshots</h2>
        {Object.entries(snapshots).length === 0
          ? <p>No snapshot available</p>
          :
          <Container fluid>
            <Row xs={1} md={2} lg={3} xl={4}>
              {snapshots.map((snapshot, index) =>
                <Col key={snapshot.digest} className="mb-2">
                  <Card border={index === 0 ? "primary" : ""}>
                    <Card.Body>
                      <Card.Title>{snapshot.digest}</Card.Title>
                      <div>
                        <div>Epoch: {snapshot.beacon.epoch}</div>
                        <div>Immutable File Number: {snapshot.beacon.immutable_file_number}</div>
                        <div>Certificate hash: <br/>
                          {snapshot.certificate_hash}{' '}
                          <Button size="sm" onClick={() => showCertificate(snapshot.certificate_hash)}>Show</Button>
                        </div>
                        <div>Created at: <br/> {new Date(snapshot.created_at).toLocaleString()}</div>
                        <div>Size: {formatBytes(snapshot.size)}</div>
                      </div>
                      <Card.Text>
                      </Card.Text>
                    </Card.Body>
                    <Card.Footer>
                      {index === 0 &&
                        <><Badge bg="primary">Latest</Badge>{' '}</>
                      }
                      <Badge bg="secondary">{snapshot.beacon.network}</Badge>
                    </Card.Footer>
                  </Card>
                </Col>
              )}
            </Row>
          </Container>
        }
      </div>
    </>
  );
}
