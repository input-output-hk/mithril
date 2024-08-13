import React, { useEffect, useState } from "react";
import { useSelector } from "react-redux";
import { Badge, Button, Card, Col, Container, ListGroup, Row, Stack } from "react-bootstrap";
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
  const autoUpdate = useSelector((state) => state.settings.autoUpdate);
  const updateInterval = useSelector((state) => state.settings.updateInterval);

  useEffect(() => {
    if (!autoUpdate) {
      return;
    }

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

    const interval = setInterval(fetchCardanoStakeDistribution, updateInterval);
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
        aggregator={aggregator}
        hash={selectedCertificateHash}
        onHashChange={handleCertificateHashChange}
      />

      <div className={props.className}>
        <h2>
          Cardano Stake Distribution{" "}
          <RawJsonButton href={artifactsEndpoint} variant="outline-light" size="sm" />
        </h2>
        {Object.entries(cardanoStakeDistributions).length === 0 ? (
          <p>No cardano stake distribution available</p>
        ) : (
          <Container fluid>
            <Row xs={1} md={2} lg={3} xl={4}>
              {cardanoStakeDistributions.map((cardanoStakeDistribution, index) => (
                <Col key={cardanoStakeDistribution.hash} className="mb-2">
                  <Card border={index === 0 ? "primary" : ""}>
                    <Card.Body>
                      <Card.Title>{cardanoStakeDistribution.hash}</Card.Title>
                      <ListGroup variant="flush" className="data-list-group">
                        <ListGroup.Item>Epoch: {cardanoStakeDistribution.epoch}</ListGroup.Item>
                        {cardanoStakeDistribution.created_at && (
                          <ListGroup.Item>
                            Created:{" "}
                            <LocalDateTime datetime={cardanoStakeDistribution.created_at} />
                          </ListGroup.Item>
                        )}
                        <ListGroup.Item>
                          Certificate hash: <br />
                          {cardanoStakeDistribution.certificate_hash}{" "}
                          <Button
                            size="sm"
                            onClick={() =>
                              showCertificate(cardanoStakeDistribution.certificate_hash)
                            }>
                            Show
                          </Button>
                        </ListGroup.Item>
                      </ListGroup>
                    </Card.Body>
                    <Card.Footer>
                      <Stack direction="horizontal" gap={1}>
                        {index === 0 && (
                          <>
                            <Badge bg="primary">Latest</Badge>{" "}
                          </>
                        )}

                        <RawJsonButton
                          href={`${aggregator}/artifact/cardano-stake-distribution/${cardanoStakeDistribution.hash}`}
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
