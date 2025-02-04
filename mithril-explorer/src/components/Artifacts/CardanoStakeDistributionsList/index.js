import React, { useEffect, useState } from "react";
import { useSelector } from "react-redux";
import { Button, Card, Col, Container, Row, Stack } from "react-bootstrap";
import ArtifactTitle from "#/Artifacts/ArtifactTitle";
import ArtifactCol from "#/Artifacts/ArtifactCol";
import LatestBadge from "#/Artifacts/LatestBadge";
import CertificateModal from "#/CertificateModal";
import RawJsonButton from "#/RawJsonButton";
import LocalDateTime from "#/LocalDateTime";
import { selectedAggregator } from "@/store/settingsSlice";

export default function CardanoStakeDistributionsList(props) {
  const [cardanoStakeDistributions, setCardanoStakeDistributions] = useState([]);
  const [selectedCertificateHash, setSelectedCertificateHash] = useState(undefined);
  const aggregator = useSelector(selectedAggregator);
  const artifactsEndpoint = useSelector(
    (state) => `${selectedAggregator(state)}/artifact/cardano-stake-distributions`,
  );
  const refreshSeed = useSelector((state) => state.settings.refreshSeed);
  const updateInterval = useSelector((state) => state.settings.updateInterval);

  useEffect(() => {
    let fetchCardanoStakeDistribution = () => {
      fetch(artifactsEndpoint)
        .then((response) => response.json())
        .then((data) => setCardanoStakeDistributions(data))
        .catch((error) => {
          setCardanoStakeDistributions([]);
          console.error("Fetch cardanoStakeDistributions error:", error);
        });
    };

    // Fetch them once without waiting
    fetchCardanoStakeDistribution();

    if (updateInterval) {
      const interval = setInterval(fetchCardanoStakeDistribution, updateInterval);
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
      <CertificateModal
        aggregator={aggregator}
        hash={selectedCertificateHash}
        onHashChange={handleCertificateHashChange}
      />

      <div className={props.className}>
        <h2>
          Cardano Stake Distribution{" "}
          <RawJsonButton href={artifactsEndpoint} variant="outline-light" size="sm" />
        </h2>
        <Container fluid>
          <Row>
            {Object.entries(cardanoStakeDistributions).length === 0 ? (
              <p>No cardano stake distribution available</p>
            ) : (
              cardanoStakeDistributions.map((cardanoStakeDistribution, index) => (
                <Col key={cardanoStakeDistribution.hash} className="mb-2">
                  <Card border={index === 0 ? "primary" : ""}>
                    <Card.Body className="pt-2 pb-1">
                      <ArtifactTitle hash={cardanoStakeDistribution.hash} index={index} />
                      <Container fluid>
                        <Row>
                          <ArtifactCol label="Epoch">{cardanoStakeDistribution.epoch}</ArtifactCol>
                          <ArtifactCol label="Created">
                            <LocalDateTime datetime={cardanoStakeDistribution.created_at} />
                          </ArtifactCol>
                          <ArtifactCol label="Certificate hash">
                            {cardanoStakeDistribution.certificate_hash}
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
                            showCertificate(cardanoStakeDistribution.certificate_hash)
                          }>
                          Show Certificate
                        </Button>
                        <RawJsonButton
                          href={`${aggregator}/artifact/cardano-stake-distribution/${cardanoStakeDistribution.hash}`}
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
