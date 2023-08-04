import React, {useEffect, useState} from 'react';
import {Badge, Button, Card, Col, Container, ListGroup, Row, Stack} from "react-bootstrap";
import CertificateModal from '../../CertificateModal';
import RawJsonButton from "../../RawJsonButton";
import {useSelector} from "react-redux";
import {selectedAggregator} from "../../../store/settingsSlice";

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
  const aggregator = useSelector(selectedAggregator);
  const artifactsEndpoint = useSelector((state) => `${selectedAggregator(state)}/artifact/snapshots`);
  const autoUpdate = useSelector((state) => state.settings.autoUpdate);
  const updateInterval = useSelector((state) => state.settings.updateInterval);

  useEffect(() => {
    if (!autoUpdate) {
      return;
    }

    let fetchSnapshots = () => {
      fetch(artifactsEndpoint)
        .then(response => response.json())
        .then(data => setSnapshots(data))
        .catch(error => {
          setSnapshots([]);
          console.error("Fetch snapshots error:", error);
        });
    };

    // Fetch them once without waiting
    fetchSnapshots();

    const interval = setInterval(fetchSnapshots, updateInterval);
    return () => clearInterval(interval);
  }, [artifactsEndpoint, updateInterval, autoUpdate]);

  function handleCertificateHashChange(hash) {
    setSelectedCertificateHash(hash);
  }

  function showCertificate(hash) {
    setSelectedCertificateHash(hash);
  }

  return (
    <>
      <CertificateModal
        hash={selectedCertificateHash}
        onHashChange={handleCertificateHashChange}/>

      <div className={props.className}>
        <h2>Snapshots <RawJsonButton href={artifactsEndpoint} variant="outline-light" size="sm"/></h2>
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
                      <ListGroup variant="flush" className="data-list-group">
                        <ListGroup.Item>Epoch: {snapshot.beacon.epoch}</ListGroup.Item>
                        <ListGroup.Item>Immutable File Number: {snapshot.beacon.immutable_file_number}</ListGroup.Item>
                        <ListGroup.Item>Certificate hash: <br/>
                          {snapshot.certificate_hash}{' '}
                          <Button size="sm" onClick={() => showCertificate(snapshot.certificate_hash)}>Show</Button>
                        </ListGroup.Item>
                        <ListGroup.Item>Created at: <br/> {new Date(snapshot.created_at).toLocaleString()}
                        </ListGroup.Item>
                        <ListGroup.Item>Size: {formatBytes(snapshot.size)}</ListGroup.Item>
                      </ListGroup>
                    </Card.Body>
                    <Card.Footer>
                      <Stack direction="horizontal" gap={1}>
                        {index === 0 &&
                          <><Badge bg="primary">Latest</Badge>{' '}</>
                        }
                        <Badge bg="secondary">{snapshot.beacon.network}</Badge>

                        <RawJsonButton href={`${aggregator}/snapshot/${snapshot.digest}`} size="sm"
                                       className="ms-auto"/>
                      </Stack>
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
