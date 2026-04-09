import React, { useEffect, useState } from "react";
import { useSelector } from "react-redux";
import { Button, Card, Col, Container, Row, Stack } from "react-bootstrap";
import ArtifactTitle from "#/Artifacts/ArtifactTitle";
import ArtifactCol from "#/Artifacts/ArtifactCol";
import LatestBadge from "#/Artifacts/LatestBadge";
import RawJsonButton from "#/RawJsonButton";
import LocalDateTime from "#/LocalDateTime";
import CertificateModal from "#/CertificateModal";
import { selectedAggregator } from "@/store/settingsSlice";
import { fetchAggregator } from "@/aggregator-api";

export default function CardanoBlocksTransactionsSnapshotsList(props) {
  const [cardanoBlocksTransactionsSnapshots, setCardanoBlocksTransactionsSnapshots] = useState([]);
  const [selectedCertificateHash, setSelectedCertificateHash] = useState(undefined);
  const aggregator = useSelector(selectedAggregator);
  const artifactsEndpoint = useSelector(
    (state) => `${selectedAggregator(state)}/artifact/cardano-blocks-transactions`,
  );
  const refreshSeed = useSelector((state) => state.settings.refreshSeed);
  const updateInterval = useSelector((state) => state.settings.updateInterval);

  useEffect(() => {
    let fetchSnapshots = () => {
      fetchAggregator(artifactsEndpoint)
        .then((response) => response.json())
        .then((data) => setCardanoBlocksTransactionsSnapshots(data))
        .catch((error) => {
          setCardanoBlocksTransactionsSnapshots([]);
          console.error("Fetch cardano blocks & transactions snapshots error:", error);
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
          Cardano Blocks & Transactions Snapshots{" "}
          <RawJsonButton href={artifactsEndpoint} variant="outline-light" size="sm" />
        </h2>
        <Container fluid>
          <Row>
            {Object.entries(cardanoBlocksTransactionsSnapshots).length === 0 ? (
              <p>No Cardano Blocks & Transactions Snapshot available</p>
            ) : (
              cardanoBlocksTransactionsSnapshots.map((cardanoBlocksTransactionsSnapshot, index) => (
                <Col key={cardanoBlocksTransactionsSnapshot.hash} className="mb-2">
                  <Card border={index === 0 ? "primary" : ""}>
                    <Card.Body className="pt-2 pb-1">
                      <ArtifactTitle hash={cardanoBlocksTransactionsSnapshot.hash} index={index} />
                      <Container fluid>
                        <Row>
                          <ArtifactCol label="Epoch">
                            {cardanoBlocksTransactionsSnapshot.epoch}
                          </ArtifactCol>
                          <ArtifactCol label="Block Number Signed">
                            {cardanoBlocksTransactionsSnapshot.block_number_signed}
                          </ArtifactCol>
                          <ArtifactCol label="Block Number Tip">
                            {cardanoBlocksTransactionsSnapshot.block_number_tip}
                          </ArtifactCol>
                          <ArtifactCol label="Created">
                            <LocalDateTime
                              datetime={cardanoBlocksTransactionsSnapshot.created_at}
                            />
                          </ArtifactCol>
                          <ArtifactCol label="Merkle Root">
                            {cardanoBlocksTransactionsSnapshot.merkle_root}
                          </ArtifactCol>
                          <ArtifactCol label="Certificate hash">
                            {cardanoBlocksTransactionsSnapshot.certificate_hash}
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
                          onClick={() =>
                            showCertificate(cardanoBlocksTransactionsSnapshot.certificate_hash)
                          }>
                          Show Certificate
                        </Button>
                        <RawJsonButton
                          href={`${aggregator}/artifact/cardano-blocks-transaction/${cardanoBlocksTransactionsSnapshot.hash}`}
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
