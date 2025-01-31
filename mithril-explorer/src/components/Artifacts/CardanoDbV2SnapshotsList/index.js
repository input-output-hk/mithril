import React, { useEffect, useState } from "react";
import { useSelector } from "react-redux";
import { Button, Card, Container, Row, Stack } from "react-bootstrap";
import ArtifactCol from "#/Artifacts/ArtifactCol";
import ArtifactTitle from "#/Artifacts/ArtifactTitle";
import LatestBadge from "#/Artifacts/LatestBadge";
import CertificateModal from "#/CertificateModal";
import LocalDateTime from "#/LocalDateTime";
import QuestionTooltip from "#/QuestionTooltip";
import RawJsonButton from "#/RawJsonButton";
import { formatBytes } from "@/utils";
import DownloadButton from "#/Artifacts/CardanoDbV2SnapshotsList/DownloadButton";
import { selectedAggregator } from "@/store/settingsSlice";

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
          <Stack className="mx-2" gap={2}>
            {cardanoDbSnapshots.map((cdb_snapshot, index) => (
              <Card border={index === 0 ? "primary" : ""} key={cdb_snapshot.hash}>
                <Card.Body className="pt-2 pb-1">
                  <ArtifactTitle hash={cdb_snapshot.hash} index={index} />
                  <Container fluid>
                    <Row>
                      <ArtifactCol label="Epoch">{cdb_snapshot.beacon.epoch}</ArtifactCol>
                      <ArtifactCol label="Immutable file number">
                        {cdb_snapshot.beacon.immutable_file_number}
                      </ArtifactCol>
                      <ArtifactCol label="Created">
                        <LocalDateTime datetime={cdb_snapshot.created_at} />
                      </ArtifactCol>
                      <ArtifactCol label="DB size">
                        {formatBytes(cdb_snapshot.total_db_size_uncompressed)}{" "}
                        <QuestionTooltip tooltip="Total uncompressed database size" />
                      </ArtifactCol>
                      <ArtifactCol label="Cardano node">
                        {cdb_snapshot.cardano_node_version}
                      </ArtifactCol>
                      <ArtifactCol label="Compression">
                        {cdb_snapshot.compression_algorithm}
                      </ArtifactCol>
                      <ArtifactCol label="Merkle Root">{cdb_snapshot.merkle_root}</ArtifactCol>
                      <ArtifactCol label="Certificate hash">
                        {cdb_snapshot.certificate_hash}
                      </ArtifactCol>
                    </Row>
                  </Container>
                </Card.Body>
                <Card.Footer>
                  <Stack direction="horizontal" gap={1}>
                    <LatestBadge show={index === 0} />
                    <Button
                      size="sm"
                      className="ms-auto"
                      onClick={() => showCertificate(cdb_snapshot.certificate_hash)}>
                      Show Certificate
                    </Button>
                    <DownloadButton
                      size="sm"
                      artifactUrl={`${artifactsEndpoint}/${cdb_snapshot.hash}`}
                    />
                    <RawJsonButton href={`${artifactsEndpoint}/${cdb_snapshot.hash}`} size="sm" />
                  </Stack>
                </Card.Footer>
              </Card>
            ))}
          </Stack>
        )}
      </div>
    </>
  );
}
