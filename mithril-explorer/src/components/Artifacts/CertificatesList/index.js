import React, { useEffect, useState } from "react";
import { useSelector } from "react-redux";
import { Badge, Button, Card, Col, Container, ListGroup, Row, Stack } from "react-bootstrap";
import CertificateModal from "#/CertificateModal";
import RawJsonButton from "#/RawJsonButton";
import LocalDateTime from "#/LocalDateTime";
import { selectedAggregator } from "@/store/settingsSlice";
import SignedEntityType from "#/SignedEntityType";

export default function CertificatesList(props) {
  const [certificates, setCertificates] = useState([]);
  const [selectedCertificateHash, setSelectedCertificateHash] = useState(undefined);
  const aggregator = useSelector(selectedAggregator);
  const certificatesEndpoint = useSelector((state) => `${selectedAggregator(state)}/certificates`);
  const autoUpdate = useSelector((state) => state.settings.autoUpdate);
  const updateInterval = useSelector((state) => state.settings.updateInterval);

  useEffect(() => {
    if (!autoUpdate) {
      return;
    }

    let fetchCertificates = () => {
      fetch(certificatesEndpoint)
        .then((response) => response.json())
        .then((data) => setCertificates(data))
        .catch((error) => {
          setCertificates([]);
          console.error("Fetch certificates error:", error);
        });
    };

    // Fetch them once without waiting
    fetchCertificates();

    const interval = setInterval(fetchCertificates, updateInterval);
    return () => clearInterval(interval);
  }, [certificatesEndpoint, updateInterval, autoUpdate]);

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
          Certificates{" "}
          <RawJsonButton href={certificatesEndpoint} variant="outline-light" size="sm" />
        </h2>
        {Object.entries(certificates).length === 0 ? (
          <p>No certificate available</p>
        ) : (
          <Container fluid>
            <Row xs={1} md={2} lg={3} xl={4}>
              {certificates.map((certificate, index) => (
                <Col key={certificate.hash} className="mb-2">
                  <Card border={index === 0 ? "primary" : ""}>
                    <Card.Body>
                      <Card.Title>
                        {certificate.hash}{" "}
                        <Button size="sm" onClick={() => showCertificate(certificate.hash)}>
                          Details
                        </Button>
                      </Card.Title>
                      <ListGroup variant="flush" className="data-list-group">
                        <ListGroup.Item>
                          Parent hash: {certificate.previous_hash}{" "}
                          <Button
                            size="sm"
                            onClick={() => showCertificate(certificate.previous_hash)}>
                            Show
                          </Button>
                        </ListGroup.Item>
                        <ListGroup.Item>Epoch: {certificate.beacon.epoch}</ListGroup.Item>
                        <ListGroup.Item>
                          Number of signers: {certificate.metadata.total_signers}
                        </ListGroup.Item>
                        <ListGroup.Item>
                          Signed entity:{" "}
                          <SignedEntityType
                            signedEntityType={certificate.signed_entity_type}
                            table
                          />
                        </ListGroup.Item>
                        <ListGroup.Item>
                          Initiated at:{" "}
                          <LocalDateTime datetime={certificate.metadata.initiated_at} />
                        </ListGroup.Item>
                        <ListGroup.Item>
                          Sealed at: <LocalDateTime datetime={certificate.metadata.sealed_at} />
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
                        <Badge bg="secondary">{certificate.beacon.network}</Badge>

                        <RawJsonButton
                          href={`${aggregator}/certificate/${certificate.hash}`}
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
