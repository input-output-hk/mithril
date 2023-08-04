import React, {useEffect, useState} from 'react';
import {Badge, Button, Card, Col, Container, ListGroup, Row, Stack} from "react-bootstrap";
import CertificateModal from '../../CertificateModal';
import RawJsonButton from "../../RawJsonButton";
import {useSelector} from "react-redux";
import {selectedAggregator} from "../../../store/settingsSlice";

export default function MithrilStakeDistributionsList(props) {
  const [mithrilStakeDistributions, setMithrilStakeDistributions] = useState([]);
  const [selectedCertificateHash, setSelectedCertificateHash] = useState(undefined);
  const aggregator = useSelector(selectedAggregator);
  const artifactsEndpoint = useSelector((state) => `${selectedAggregator(state)}/artifact/mithril-stake-distributions`);
  const autoUpdate = useSelector((state) => state.settings.autoUpdate);
  const updateInterval = useSelector((state) => state.settings.updateInterval);

  useEffect(() => {
    if (!autoUpdate) {
      return;
    }

    let fetchMithrilStakeDistribution = () => {
      fetch(artifactsEndpoint)
        .then(response => response.json())
        .then(data => setMithrilStakeDistributions(data))
        .catch(error => {
          setMithrilStakeDistributions([]);
          console.error("Fetch mithrilStakeDistributions error:", error);
        });
    };

    // Fetch them once without waiting
    fetchMithrilStakeDistribution();

    const interval = setInterval(fetchMithrilStakeDistribution, updateInterval);
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
        onHashChange={handleCertificateHashChange}/>

      <div className={props.className}>
        <h2>Mithril Stake Distribution <RawJsonButton href={artifactsEndpoint}
                                                      variant="outline-light" size="sm"/></h2>
        {Object.entries(mithrilStakeDistributions).length === 0
          ? <p>No mithril stake distribution available</p>
          :
          <Container fluid>
            <Row xs={1} md={2} lg={3} xl={4}>
              {mithrilStakeDistributions.map((mithrilStakeDistribution, index) =>
                <Col key={mithrilStakeDistribution.hash} className="mb-2">
                  <Card border={index === 0 ? "primary" : ""}>
                    <Card.Body>
                      <Card.Title>{mithrilStakeDistribution.hash}</Card.Title>
                      <ListGroup variant="flush" className="data-list-group">
                        <ListGroup.Item>Epoch: {mithrilStakeDistribution.epoch}</ListGroup.Item>
                        {mithrilStakeDistribution.created_at &&
                          <ListGroup.Item>Created: {new Date(mithrilStakeDistribution.created_at).toLocaleString()}</ListGroup.Item>
                        }
                        <ListGroup.Item>Certificate hash: <br/>
                          {mithrilStakeDistribution.certificate_hash}{' '}
                          <Button size="sm"
                                  onClick={() => showCertificate(mithrilStakeDistribution.certificate_hash)}>Show</Button>
                        </ListGroup.Item>
                      </ListGroup>
                    </Card.Body>
                    <Card.Footer>
                      <Stack direction="horizontal" gap={1}>
                        {index === 0 &&
                          <><Badge bg="primary">Latest</Badge>{' '}</>
                        }

                        <RawJsonButton
                          href={`${aggregator}/artifact/mithril-stake-distribution/${mithrilStakeDistribution.hash}`}
                          size="sm"
                          className="ms-auto"/>
                      </Stack>
                    </Card.Footer>
                  </Card>
                </Col>
              )}
            </Row>
          </Container>
        }
      </div>
    </>
  );
}
