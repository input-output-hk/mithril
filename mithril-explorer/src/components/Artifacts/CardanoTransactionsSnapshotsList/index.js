import React, { useEffect, useState } from "react";
import { useSelector } from "react-redux";
import { Button, Card, Col, Container, Form, Row, Stack } from "react-bootstrap";
import ArtifactTitle from "#/Artifacts/ArtifactTitle";
import ArtifactCol from "#/Artifacts/ArtifactCol";
import LatestBadge from "#/Artifacts/LatestBadge";
import RawJsonButton from "#/RawJsonButton";
import LocalDateTime from "#/LocalDateTime";
import CardanoTransactionsFormInput from "#/CardanoTransactionsFormInput";
import CertificateModal from "#/CertificateModal";
import CertifyCardanoTransactionsModal from "#/CertifyCardanoTransactionsModal";
import { defaultAggregatorCapabilities } from "@/constants";
import { selectedAggregator, selectedAggregatorCapabilities } from "@/store/settingsSlice";

export default function CardanoTransactionsSnapshotsList(props) {
  const [cardanoTransactionsSnapshots, setCardanoTransactionsSnapshots] = useState([]);
  const [selectedCertificateHash, setSelectedCertificateHash] = useState(undefined);
  const [showCertificationFormValidation, setShowCertificationFormValidation] = useState(false);
  const [transactionHashesToCertify, setTransactionHashesToCertify] = useState([]);
  const aggregator = useSelector(selectedAggregator);
  const artifactsEndpoint = useSelector(
    (state) => `${selectedAggregator(state)}/artifact/cardano-transactions`,
  );
  const refreshSeed = useSelector((state) => state.settings.refreshSeed);
  const updateInterval = useSelector((state) => state.settings.updateInterval);
  const currentAggregatorCapabilities = useSelector((state) =>
    selectedAggregatorCapabilities(state),
  );

  useEffect(() => {
    let fetchSnapshots = () => {
      fetch(artifactsEndpoint)
        .then((response) => response.json())
        .then((data) => setCardanoTransactionsSnapshots(data))
        .catch((error) => {
          setCardanoTransactionsSnapshots([]);
          console.error("Fetch cardano transactions snapshots error:", error);
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

  function handleTransactionHashesToCertifyChange(hashes) {
    setTransactionHashesToCertify(hashes);
  }

  function showCertificate(hash) {
    setSelectedCertificateHash(hash);
  }

  function handleCtxCertificationSubmit(event) {
    // Prevent page refresh
    event.preventDefault();

    const form = event.target;

    if (form.checkValidity() === true) {
      const formData = new FormData(form);
      const formJson = Object.fromEntries(formData.entries());
      const hashes = (formJson?.txHashes ?? "")
        .split(",")
        .map((hash) => hash.trim())
        .filter((hash) => hash.length > 0);
      hashes.sort();

      setTransactionHashesToCertify(hashes);
      setShowCertificationFormValidation(false);
    } else {
      setShowCertificationFormValidation(true);
    }
  }

  return (
    <>
      <CertificateModal hash={selectedCertificateHash} onHashChange={handleCertificateHashChange} />
      <CertifyCardanoTransactionsModal
        transactionHashes={transactionHashesToCertify}
        onHashesChange={handleTransactionHashesToCertifyChange}
      />

      <div className={props.className}>
        <h2>
          Cardano Transactions Snapshots{" "}
          <RawJsonButton href={artifactsEndpoint} variant="outline-light" size="sm" />
        </h2>
        <Container fluid>
          <Row className="mb-2">
            <Form
              onSubmit={handleCtxCertificationSubmit}
              noValidate
              validated={showCertificationFormValidation}>
              <Row>
                <CardanoTransactionsFormInput
                  maxAllowedHashesByRequest={
                    currentAggregatorCapabilities?.cardano_transactions_prover
                      ?.max_hashes_allowed_by_request ??
                    defaultAggregatorCapabilities.cardano_transactions_prover
                      .max_hashes_allowed_by_request
                  }
                />
              </Row>
            </Form>
          </Row>
          <Row>
            {Object.entries(cardanoTransactionsSnapshots).length === 0 ? (
              <p>No Cardano Transactions Snapshot available</p>
            ) : (
              cardanoTransactionsSnapshots.map((cardanoTransactionsSnapshot, index) => (
                <Col key={cardanoTransactionsSnapshot.hash} className="mb-2">
                  <Card border={index === 0 ? "primary" : ""}>
                    <Card.Body className="pt-2 pb-1">
                      <ArtifactTitle hash={cardanoTransactionsSnapshot.hash} index={index} />
                      <Container fluid>
                        <Row>
                          <ArtifactCol label="Epoch">
                            {cardanoTransactionsSnapshot.epoch}
                          </ArtifactCol>
                          <ArtifactCol label="Block Number">
                            {cardanoTransactionsSnapshot.block_number}
                          </ArtifactCol>
                          <ArtifactCol label="Created">
                            <LocalDateTime datetime={cardanoTransactionsSnapshot.created_at} />
                          </ArtifactCol>
                          <ArtifactCol label="Merkle Root">
                            {cardanoTransactionsSnapshot.merkle_root}
                          </ArtifactCol>
                          <ArtifactCol label="Certificate hash">
                            {cardanoTransactionsSnapshot.certificate_hash}
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
                            showCertificate(cardanoTransactionsSnapshot.certificate_hash)
                          }>
                          Show Certificate
                        </Button>
                        <RawJsonButton
                          href={`${aggregator}/artifact/cardano-transaction/${cardanoTransactionsSnapshot.hash}`}
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
