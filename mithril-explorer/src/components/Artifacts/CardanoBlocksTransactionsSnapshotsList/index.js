import React, { useEffect, useState } from "react";
import { useSelector } from "react-redux";
import { Button, Card, Col, Container, Form, Row, Stack } from "react-bootstrap";
import ArtifactTitle from "#/Artifacts/ArtifactTitle";
import ArtifactCol from "#/Artifacts/ArtifactCol";
import LatestBadge from "#/Artifacts/LatestBadge";
import RawJsonButton from "#/RawJsonButton";
import LocalDateTime from "#/LocalDateTime";
import CertificateModal from "#/CertificateModal";
import CertifyCardanoBlocksOrTransactionsModal, {
  certifiedMessageTypes,
} from "#/CertifyCardanoBlocksOrTransactionsModal";
import { selectedAggregator, selectedAggregatorCapabilities } from "@/store/settingsSlice";
import { fetchAggregator } from "@/aggregator-api";
import CardanoTransactionsFormInput from "#/CardanoTransactionsFormInput";
import { defaultAggregatorCapabilities } from "@/constants";

export default function CardanoBlocksTransactionsSnapshotsList(props) {
  const [cardanoBlocksTransactionsSnapshots, setCardanoBlocksTransactionsSnapshots] = useState([]);
  const [selectedCertificateHash, setSelectedCertificateHash] = useState(undefined);
  const [showCertificationFormValidation, setShowCertificationFormValidation] = useState(false);
  const [transactionHashesToCertify, setTransactionHashesToCertify] = useState([]);
  const aggregator = useSelector(selectedAggregator);
  const artifactsEndpoint = useSelector(
    (state) => `${selectedAggregator(state)}/artifact/cardano-blocks-transactions`,
  );
  const refreshSeed = useSelector((state) => state.settings.refreshSeed);
  const updateInterval = useSelector((state) => state.settings.updateInterval);
  const currentAggregatorCapabilities = useSelector((state) =>
    selectedAggregatorCapabilities(state),
  );

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
      <CertifyCardanoBlocksOrTransactionsModal
        certifiedMessageType={certifiedMessageTypes.transaction}
        hashes={transactionHashesToCertify}
        onHashesChange={handleTransactionHashesToCertifyChange}
      />

      <div className={props.className}>
        <h2>
          Cardano Blocks & Transactions Snapshots{" "}
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
