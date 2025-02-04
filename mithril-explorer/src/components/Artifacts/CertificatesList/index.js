import React, { useEffect, useState } from "react";
import { useSelector } from "react-redux";
import { Badge, Button, Card, Col, Container, Row, Stack } from "react-bootstrap";
import ArtifactTitle from "#/Artifacts/ArtifactTitle";
import ArtifactCol from "#/Artifacts/ArtifactCol";
import LatestBadge from "#/Artifacts/LatestBadge";
import CertificateModal from "#/CertificateModal";
import LocalDateTime from "#/LocalDateTime";
import RawJsonButton from "#/RawJsonButton";
import { selectedAggregator } from "@/store/settingsSlice";
import { parseSignedEntity } from "@/utils";

function BeaconColumns({ signedEntity }) {
  const beacon = parseSignedEntity(signedEntity ?? {});
  const formatLabel = (label) => label.replace(/_/g, " ");

  return (
    <>
      <ArtifactCol label="Type">
        <h6>{beacon.name}</h6>
      </ArtifactCol>
      {Object.entries(beacon.fields).map(([key, value]) => (
        <ArtifactCol key={key} label={formatLabel(key)}>
          {value}
        </ArtifactCol>
      ))}
    </>
  );
}

export default function CertificatesList(props) {
  const [certificates, setCertificates] = useState([]);
  const [selectedCertificateHash, setSelectedCertificateHash] = useState(undefined);
  const aggregator = useSelector(selectedAggregator);
  const certificatesEndpoint = useSelector((state) => `${selectedAggregator(state)}/certificates`);
  const refreshSeed = useSelector((state) => state.settings.refreshSeed);
  const updateInterval = useSelector((state) => state.settings.updateInterval);

  useEffect(() => {
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

    if (updateInterval) {
      const interval = setInterval(fetchCertificates, updateInterval);
      return () => clearInterval(interval);
    }
  }, [certificatesEndpoint, updateInterval, refreshSeed]);

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
        <Container fluid>
          <Row>
            {Object.entries(certificates).length === 0 ? (
              <p>No certificate available</p>
            ) : (
              certificates.map((certificate, index) => (
                <Col key={certificate.hash} className="mb-2">
                  <Card border={index === 0 ? "primary" : ""}>
                    <Card.Body>
                      <ArtifactTitle hash={certificate.hash} index={index} />
                      <Container fluid>
                        <Row>
                          <BeaconColumns signedEntity={certificate.signed_entity_type} />
                          <ArtifactCol label="Number of signers">
                            {certificate.metadata.total_signers}
                          </ArtifactCol>
                          <ArtifactCol label="Initiated at">
                            <LocalDateTime datetime={certificate.metadata.initiated_at} />
                          </ArtifactCol>
                          <ArtifactCol label="Sealed at">
                            <LocalDateTime datetime={certificate.metadata.sealed_at} />
                          </ArtifactCol>
                          <ArtifactCol label="Parent hash">
                            {certificate.previous_hash}{" "}
                          </ArtifactCol>
                        </Row>
                      </Container>
                    </Card.Body>
                    <Card.Footer>
                      <Stack direction="horizontal" gap={1}>
                        <LatestBadge show={index === 0} />
                        <Badge bg="secondary">{certificate.metadata.network}</Badge>

                        <Button
                          size="sm"
                          onClick={() => showCertificate(certificate.hash)}
                          className="ms-auto">
                          Show Details
                        </Button>
                        <Button
                          size="sm"
                          onClick={() => showCertificate(certificate.previous_hash)}>
                          Show Parent
                        </Button>
                        <RawJsonButton
                          href={`${aggregator}/certificate/${certificate.hash}`}
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
