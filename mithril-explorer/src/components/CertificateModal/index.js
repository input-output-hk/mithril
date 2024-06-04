import React, { useEffect, useState } from "react";
import { Accordion, Badge, Button, Col, Container, Modal, Row, Table } from "react-bootstrap";
import { Bar } from "react-chartjs-2";
import { useSelector } from "react-redux";
import { computeStakeShapesDataset } from "@/charts";
import { selectedAggregator } from "@/store/settingsSlice";
import RawJsonButton from "#/RawJsonButton";
import Stake from "#/Stake";
import ProtocolParameters from "#/ProtocolParameters";
import SignedEntityType from "#/SignedEntityType";
import SignerTable from "#/SignerTable";
import VerifyCertificateModal from "#/VerifyCertificate/VerifyCertificateModal";

export default function CertificateModal({
  hash,
  hideButtons = false,
  onHashChange = (hash) => {},
}) {
  const [certificate, setCertificate] = useState({});
  const [charts, setCharts] = useState({
    stakesBreakdown: {},
  });
  const [showVerifyCertificateModal, setShowVerifyCertificateModal] = useState(false);
  const certificateEndpoint = useSelector(
    (state) => `${selectedAggregator(state)}/certificate/${hash}`,
  );

  useEffect(() => {
    if (!hash) {
      return;
    }

    fetch(certificateEndpoint)
      .then((response) => (response.status === 200 ? response.json() : {}))
      .then((data) => {
        setCertificate(data);
        setCharts({
          stakesBreakdown: computeStakeShapesDataset(data.metadata.signers),
        });
      })
      .catch((error) => {
        setCertificate({});
        console.error("Fetch certificate error:", error);
      });
  }, [certificateEndpoint, hash]);

  function showPrevious() {
    onHashChange(certificate.previous_hash);
  }

  function close() {
    onHashChange(undefined);
  }

  return (
    <Modal
      show={hash !== undefined}
      onHide={close}
      size="xl"
      aria-labelledby="contained-modal-title-vcenter"
      centered>
      <Modal.Header className="text-break" closeButton>
        <Modal.Title id="contained-modal-title-vcenter">Certificate {certificate.hash}</Modal.Title>
      </Modal.Header>

      <Modal.Body>
        {Object.entries(certificate).length === 0 ? (
          <p>Not found</p>
        ) : (
          // Little hack: hide the modal content if the verify modal is shown so this modal content
          // never overflow behind the verify certificate modal.
          <Container className={showVerifyCertificateModal && "d-none"}>
            <Row md={1} xl="auto">
              <Col xl={4} className="mb-3">
                <h4>Beacon</h4>
                <SignedEntityType signedEntityType={certificate.signed_entity_type} table />
                <h4>Protocol Parameters</h4>
                <ProtocolParameters
                  className="mb-3"
                  protocolParameters={certificate.metadata.parameters}
                />
                <h4>Statistics</h4>
                <Table className="mb-0">
                  <tbody>
                    <tr>
                      <td>
                        <em>Number of signers:</em>
                      </td>
                      <td>{certificate?.metadata.signers.length ?? 0}</td>
                    </tr>
                    <tr>
                      <td>
                        <em>Total stakes:</em>
                      </td>
                      <td>
                        <Stake
                          lovelace={
                            certificate?.metadata.signers.reduce((acc, s) => acc + s.stake, 0) ?? 0
                          }
                        />
                      </td>
                    </tr>
                  </tbody>
                </Table>
              </Col>
              <Col xl={8} className="mb-1">
                {certificate.genesis_signature !== "" ? (
                  <div>
                    This is the chain{" "}
                    <em>
                      <strong>Genesis Certificate</strong>
                    </em>
                    , since it&apos;s manually created it doesn&apos;t contain any Signers.
                  </div>
                ) : (
                  <>
                    <h4>Stakes breakdown</h4>
                    <Bar data={charts.stakesBreakdown} />
                  </>
                )}
              </Col>
            </Row>
            <Row>
              {certificate.genesis_signature === "" && (
                <Accordion>
                  <Accordion.Item eventKey="signers">
                    <Accordion.Header>
                      <h4>Signers</h4>
                    </Accordion.Header>
                    <Accordion.Body>
                      {certificate.metadata.signers.length === 0 ? (
                        <div>
                          No Signers for this certificate, something went wrong either with the data
                          retrieval or the signing process
                        </div>
                      ) : (
                        <SignerTable signers={certificate.metadata.signers} />
                      )}
                    </Accordion.Body>
                  </Accordion.Item>
                </Accordion>
              )}
            </Row>
          </Container>
        )}
      </Modal.Body>
      <Modal.Footer>
        {!hideButtons && (
          <Button
            size="sm"
            onClick={() => setShowVerifyCertificateModal(true)}
            className="text-break">
            Verify certificate
          </Button>
        )}
        {certificate.genesis_signature !== "" ? (
          <Badge bg="warning">Genesis</Badge>
        ) : (
          !hideButtons && (
            <Button size="sm" onClick={showPrevious} className="text-break">
              Previous hash: {certificate.previous_hash}
            </Button>
          )
        )}
        <RawJsonButton href={certificateEndpoint} size="sm" />
      </Modal.Footer>
      {!hideButtons && (
        <VerifyCertificateModal
          show={showVerifyCertificateModal}
          onClose={() => setShowVerifyCertificateModal(false)}
          certificateHash={certificate.hash}
        />
      )}
    </Modal>
  );
}
