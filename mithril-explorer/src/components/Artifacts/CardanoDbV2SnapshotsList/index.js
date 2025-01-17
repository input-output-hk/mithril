import React, { useEffect, useState } from "react";
import { useSelector } from "react-redux";
import { Button, Card, Col, Container, ListGroup, Row, Stack } from "react-bootstrap";
import LatestBadge from "#/Artifacts/LatestBadge";
import CertificateModal from "#/CertificateModal";
import RawJsonButton from "#/RawJsonButton";
import LocalDateTime from "#/LocalDateTime";
import { selectedAggregator } from "@/store/settingsSlice";
import { formatBytes } from "@/utils";

export default function CardanoDbV2SnapshotsList(props) {
  const [cardanoDbSnapshots, setCardanoDbSnapshots] = useState([]);
  const [selectedCertificateHash, setSelectedCertificateHash] = useState(undefined);
  const artifactsEndpoint = useSelector(
    (state) => `${selectedAggregator(state)}/artifact/cardano-database`,
  );
  const refreshSeed = useSelector((state) => state.settings.refreshSeed);
  const updateInterval = useSelector((state) => state.settings.updateInterval);

  useEffect(() => {
    let fetchSnapshots = () => {
      fetch(artifactsEndpoint)
        .then((response) => response.json())
        .then((data) => setCardanoDbSnapshots(data))
        .catch((error) => {
          setCardanoDbSnapshots([]);
          console.error("Fetch Cardano Db Snapshots error:", error);
        });
    };

    // Fetch them once without waiting
    fetchSnapshots();

    if (updateInterval) {
      const interval = setInterval(fetchSnapshots, updateInterval);
      return () => clearInterval(interval);
    }
  }, [artifactsEndpoint, updateInterval, refreshSeed]);

  function handleCertificateHashChange(hash) {
    setSelectedCertificateHash(hash);
  }

  function showCertificate(hash) {
    setSelectedCertificateHash(hash);
  }

  return (
    <>
      <CertificateModal hash={selectedCertificateHash} onHashChange={handleCertificateHashChange} />

      <div className={props.className}>
        <h2>
          Cardano Db Snapshots v2{" "}
          <RawJsonButton href={artifactsEndpoint} variant="outline-light" size="sm" />
        </h2>
        {Object.entries(cardanoDbSnapshots).length === 0 ? (
          <p>No cardano database snapshot available</p>
        ) : (
          <Container fluid>
            <Row xs={1} md={2} lg={3} xl={4}>
              {cardanoDbSnapshots.map((cdb_snapshot, index) => (
                <Col key={cdb_snapshot.hash} className="mb-2">
                  <Card border={index === 0 ? "primary" : ""}>
                    <Card.Body>
                      <Card.Title>{cdb_snapshot.hash}</Card.Title>
                      <ListGroup variant="flush" className="data-list-group">
                        <ListGroup.Item>Epoch: {cdb_snapshot.beacon.epoch}</ListGroup.Item>
                        <ListGroup.Item>
                          Immutable file number: {cdb_snapshot.beacon.immutable_file_number}
                        </ListGroup.Item>
                        <ListGroup.Item>Merkle Root: {cdb_snapshot.merkle_root}</ListGroup.Item>
                        <ListGroup.Item>
                          Cardano node: {cdb_snapshot.cardano_node_version}
                        </ListGroup.Item>
                        <ListGroup.Item>
                          Compression: {cdb_snapshot.compression_algorithm}
                        </ListGroup.Item>
                        <ListGroup.Item>
                          Certificate hash: <br />
                          {cdb_snapshot.certificate_hash}{" "}
                          <Button
                            size="sm"
                            onClick={() => showCertificate(cdb_snapshot.certificate_hash)}>
                            Show
                          </Button>
                        </ListGroup.Item>
                        <ListGroup.Item>
                          Created: <LocalDateTime datetime={cdb_snapshot.created_at} />
                        </ListGroup.Item>
                        <ListGroup.Item>
                          Uncompressed DB size:{" "}
                          {formatBytes(cdb_snapshot.total_db_size_uncompressed)}
                        </ListGroup.Item>
                      </ListGroup>
                    </Card.Body>
                    <Card.Footer>
                      <Stack direction="horizontal" gap={1}>
                        <LatestBadge show={index === 0} />
                        <RawJsonButton
                          href={`${artifactsEndpoint}/${cdb_snapshot.hash}`}
                          size="sm"
                          className="ms-auto"
                        />
                      </Stack>
                    </Card.Footer>
                  </Card>
                </Col>
              ))}
            </Row>
          </Container>
        )}
      </div>
    </>
  );
}
