import React, { useEffect, useState } from "react";
import { useSelector } from "react-redux";
import { Badge, Button, Card, Col, Container, Form, ListGroup, Row, Stack } from "react-bootstrap";
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
  const autoUpdate = useSelector((state) => state.settings.autoUpdate);
  const updateInterval = useSelector((state) => state.settings.updateInterval);
  const currentAggregatorCapabilities = useSelector((state) =>
    selectedAggregatorCapabilities(state),
  );

  useEffect(() => {
    if (!autoUpdate) {
      return;
    }

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

    const interval = setInterval(fetchSnapshots, updateInterval);
    return () => clearInterval(interval);
  }, [artifactsEndpoint, updateInterval, autoUpdate]);

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
          {Object.entries(cardanoTransactionsSnapshots).length === 0 ? (
            <p>No Cardano Transactions Snapshot available</p>
          ) : (
            <Row xs={1} md={2} lg={3} xl={4}>
              {cardanoTransactionsSnapshots.map((cardanoTransactionsSnapshot, index) => (
                <Col key={cardanoTransactionsSnapshot.hash} className="mb-2">
                  <Card border={index === 0 ? "primary" : ""}>
                    <Card.Body>
                      <Card.Title>{cardanoTransactionsSnapshot.hash}</Card.Title>
                      <ListGroup variant="flush" className="data-list-group">
                        <ListGroup.Item>Epoch: {cardanoTransactionsSnapshot.epoch}</ListGroup.Item>
                        <ListGroup.Item>
                          Block Number: {cardanoTransactionsSnapshot.block_number}
                        </ListGroup.Item>
                        <ListGroup.Item>
                          Merkle Root: {cardanoTransactionsSnapshot.merkle_root}
                        </ListGroup.Item>
                        <ListGroup.Item>
                          Certificate hash: <br />
                          {cardanoTransactionsSnapshot.certificate_hash}{" "}
                          <Button
                            size="sm"
                            onClick={() =>
                              showCertificate(cardanoTransactionsSnapshot.certificate_hash)
                            }>
                            Show
                          </Button>
                        </ListGroup.Item>
                        <ListGroup.Item>
                          Created:{" "}
                          <LocalDateTime datetime={cardanoTransactionsSnapshot.created_at} />
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
                          href={`${aggregator}/artifact/cardano-transaction/${cardanoTransactionsSnapshot.hash}`}
                          size="sm"
                          className="ms-auto"
                        />
                      </Stack>
                    </Card.Footer>
                  </Card>
                </Col>
              ))}
            </Row>
          )}
        </Container>
      </div>
    </>
  );
}
