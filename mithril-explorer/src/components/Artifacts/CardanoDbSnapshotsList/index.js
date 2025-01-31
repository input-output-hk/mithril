import React, { useEffect, useState } from "react";
import { useSelector } from "react-redux";
import { Badge, Button, Card, Col, Container, Row, Stack } from "react-bootstrap";
import ArtifactTitle from "#/Artifacts/ArtifactTitle";
import ArtifactCol from "#/Artifacts/ArtifactCol";
import LatestBadge from "#/Artifacts/LatestBadge";
import CertificateModal from "#/CertificateModal";
import RawJsonButton from "#/RawJsonButton";
import LocalDateTime from "#/LocalDateTime";
import { selectedAggregator } from "@/store/settingsSlice";
import { formatBytes } from "@/utils";

export default function CardanoDbSnapshotsList(props) {
  const [cardanoDbSnapshots, setCardanoDbSnapshots] = useState([]);
  const [selectedCertificateHash, setSelectedCertificateHash] = useState(undefined);
  const aggregator = useSelector(selectedAggregator);
  const artifactsEndpoint = useSelector(
    (state) => `${selectedAggregator(state)}/artifact/snapshots`,
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
          Cardano Db Snapshots{" "}
          <RawJsonButton href={artifactsEndpoint} variant="outline-light" size="sm" />
        </h2>
        <Container fluid>
          <Row>
            {Object.entries(cardanoDbSnapshots).length === 0 ? (
              <p>No snapshot available</p>
            ) : (
              cardanoDbSnapshots.map((snapshot, index) => (
                <Col key={snapshot.digest} className="mb-2">
                  <Card border={index === 0 ? "primary" : ""}>
                    <Card.Body className="pt-2 pb-1">
                      <ArtifactTitle hash={snapshot.digest} index={index} />
                      <Container fluid>
                        <Row>
                          <ArtifactCol label="Epoch">{snapshot.beacon.epoch}</ArtifactCol>
                          <ArtifactCol label="Immutable file number">
                            {snapshot.beacon.immutable_file_number}
                          </ArtifactCol>
                          <ArtifactCol label="Created">
                            <LocalDateTime datetime={snapshot.created_at} />
                          </ArtifactCol>
                          <ArtifactCol label="Archive size">
                            {formatBytes(snapshot.size)}
                          </ArtifactCol>
                          <ArtifactCol label="Cardano node">
                            {snapshot.cardano_node_version}
                          </ArtifactCol>
                          <ArtifactCol label="Compression">
                            {snapshot.compression_algorithm}
                          </ArtifactCol>
                          <ArtifactCol label="Certificate hash">
                            {snapshot.certificate_hash}
                          </ArtifactCol>
                        </Row>
                      </Container>
                    </Card.Body>
                    <Card.Footer>
                      <Stack direction="horizontal" gap={1}>
                        <LatestBadge show={index === 0} />
                        <Badge bg="secondary">{snapshot.network}</Badge>
                        <Button
                          size="sm"
                          className="ms-auto"
                          onClick={() => showCertificate(snapshot.certificate_hash)}>
                          Show Certificate
                        </Button>
                        <RawJsonButton
                          href={`${aggregator}/artifact/snapshot/${snapshot.digest}`}
                          size="sm"
                        />
                      </Stack>
                    </Card.Footer>
                  </Card>
                </Col>
              ))
            )}
          </Row>
        </Container>
      </div>
    </>
  );
}
