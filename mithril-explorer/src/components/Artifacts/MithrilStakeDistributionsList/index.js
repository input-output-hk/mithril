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

export default function MithrilStakeDistributionsList(props) {
  const [mithrilStakeDistributions, setMithrilStakeDistributions] = useState([]);
  const [selectedCertificateHash, setSelectedCertificateHash] = useState(undefined);
  const aggregator = useSelector(selectedAggregator);
  const artifactsEndpoint = useSelector(
    (state) => `${selectedAggregator(state)}/artifact/mithril-stake-distributions`,
  );
  const refreshSeed = useSelector((state) => state.settings.refreshSeed);
  const updateInterval = useSelector((state) => state.settings.updateInterval);

  useEffect(() => {
    let fetchMithrilStakeDistribution = () => {
      fetch(artifactsEndpoint)
        .then((response) => response.json())
        .then((data) => setMithrilStakeDistributions(data))
        .catch((error) => {
          setMithrilStakeDistributions([]);
          console.error("Fetch mithrilStakeDistributions error:", error);
        });
    };

    // Fetch them once without waiting
    fetchMithrilStakeDistribution();

    if (updateInterval) {
      const interval = setInterval(fetchMithrilStakeDistribution, updateInterval);
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
          Mithril Stake Distribution{" "}
          <RawJsonButton href={artifactsEndpoint} variant="outline-light" size="sm" />
        </h2>
        <Container fluid>
          <Row>
            {Object.entries(mithrilStakeDistributions).length === 0 ? (
              <p>No mithril stake distribution available</p>
            ) : (
              mithrilStakeDistributions.map((mithrilStakeDistribution, index) => (
                <Col key={mithrilStakeDistribution.hash} className="mb-2">
                  <Card border={index === 0 ? "primary" : ""}>
                    <Card.Body className="pt-2 pb-1">
                      <ArtifactTitle hash={mithrilStakeDistribution.hash} index={index} />
                      <Container fluid>
                        <Row>
                          <ArtifactCol label="Epoch">{mithrilStakeDistribution.epoch}</ArtifactCol>
                          <ArtifactCol label="Created">
                            <LocalDateTime datetime={mithrilStakeDistribution.created_at} />
                          </ArtifactCol>
                          <ArtifactCol label="Certificate hash">
                            {mithrilStakeDistribution.certificate_hash}
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
                            showCertificate(mithrilStakeDistribution.certificate_hash)
                          }>
                          Show Certificate
                        </Button>
                        <RawJsonButton
                          href={`${aggregator}/artifact/mithril-stake-distribution/${mithrilStakeDistribution.hash}`}
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
