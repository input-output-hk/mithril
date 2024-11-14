import React, { useEffect, useState } from "react";
import { Card, Row, Col, Container, Table } from "react-bootstrap";
import { useSelector } from "react-redux";
import RawJsonButton from "#/RawJsonButton";
import ProtocolParameters from "#/ProtocolParameters";
import Stake from "#/Stake";
import { selectedAggregator } from "@/store/settingsSlice";
import { checkUrl } from "@/utils";

export default function AggregatorStatus() {
  const [aggregatorStatus, setAggregatorStatus] = useState({});
  const currentAggregator = useSelector((state) => state.settings.selectedAggregator);
  const aggregatorStatusEndpoint = useSelector((state) => `${selectedAggregator(state)}/status`);
  const autoUpdate = useSelector((state) => state.settings.autoUpdate);
  const updateInterval = useSelector((state) => state.settings.updateInterval);

  useEffect(() => {
    let fetchAggregatorStatus = () => {
      fetch(aggregatorStatusEndpoint)
        .then((response) => (response.status === 200 ? response.json() : {}))
        .then((data) => setAggregatorStatus(data))
        .catch((error) => {
          setAggregatorStatus({});
          console.error("Fetch status error:", error);
        });
    };

    // Fetch it once without waiting
    fetchAggregatorStatus();

    if (autoUpdate) {
      const interval = setInterval(() => {
        fetchAggregatorStatus();
      }, updateInterval);
      return () => clearInterval(interval);
    }
  }, [aggregatorStatusEndpoint, updateInterval, autoUpdate]);

  useEffect(() => {
    if (checkUrl(currentAggregator) && Number.isInteger(aggregatorStatus?.epoch)) {
      const params = new URLSearchParams();
      params.set("aggregator", currentAggregator);
      params.set("epoch", aggregatorStatus.epoch);
    }
  }, [currentAggregator, aggregatorStatus]);

  return (
    <Container fluid>
      <div>
        <h2>
          Aggregator Status
          <RawJsonButton href={aggregatorStatusEndpoint} variant="outline-light" size="sm" />
        </h2>

        <Container fluid>
          <Row className="d-flex flex-wrap">
            <Col xs={10} lg={2} className="mb-3">
              <Card className="h-100">
                <Card.Body>
                  <h5>Epoch {aggregatorStatus.epoch}</h5>
                  <Table className="mb-0">
                    <tbody>
                      <tr>
                        <td>
                          <em>Cardano Era:</em>
                        </td>
                        <td>{aggregatorStatus.cardano_era}</td>
                      </tr>
                      <tr>
                        <td>
                          <em>Mithril Era:</em>
                        </td>
                        <td>{aggregatorStatus.mithril_era}</td>
                      </tr>
                    </tbody>
                  </Table>
                </Card.Body>
              </Card>
            </Col>
            <Col xs={10} lg={2} className="mb-3">
              <Card className="h-100">
                <Card.Body>
                  <h5>Protocol Parameters</h5>
                  <em>Current:</em>
                  <ProtocolParameters protocolParameters={aggregatorStatus.protocol} />
                  <em>Next:</em>
                  <ProtocolParameters protocolParameters={aggregatorStatus.next_protocol} />
                </Card.Body>
              </Card>
            </Col>
            <Col xs={10} lg={2} className="mb-3">
              <Card className="h-100">
                <Card.Body>
                  <h5>Nodes Versions</h5>
                  <div className="mb-2 ps-3">
                    Cardano node: {aggregatorStatus.cardano_node_version}
                  </div>
                  <div className="mb-2 ps-3">
                    Aggregator node: {aggregatorStatus.aggregator_node_version}
                  </div>
                </Card.Body>
              </Card>
            </Col>
            <Col xs={10} lg={2} className="mb-3">
              <Card className="h-100">
                <Card.Body>
                  <h5>Participants</h5>
                  <Table className="mb-0">
                    <tbody>
                      <tr>
                        <td>
                          <em>Total SPOs:</em>
                        </td>
                        <td>{aggregatorStatus.total_cardano_spo ?? 0}</td>
                      </tr>
                      <tr>
                        <td>
                          <em>Total Signers:</em>
                        </td>
                        <td>{aggregatorStatus.total_signers ?? 0}</td>
                      </tr>
                      <tr>
                        <td>
                          <em>Total Next Signers:</em>
                        </td>
                        <td>{aggregatorStatus.total_next_signers ?? 0}</td>
                      </tr>
                    </tbody>
                  </Table>
                </Card.Body>
              </Card>
            </Col>
            <Col xs={10} lg={2} className="mb-3">
              <Card className="h-100">
                <Card.Body>
                  <h5>Stakes</h5>
                  <Table className="mb-0">
                    <tbody>
                      <tr>
                        <td>
                          <em>Total SPOs:</em>
                        </td>
                        <td>
                          <Stake lovelace={aggregatorStatus.total_cardano_stake ?? 0} />
                        </td>
                      </tr>
                      <tr>
                        <td>
                          <em>Total Signers:</em>
                        </td>
                        <td>
                          <Stake lovelace={aggregatorStatus.total_stakes_signers ?? 0} />
                        </td>
                      </tr>
                      <tr>
                        <td>
                          <em>Total Next Signers:</em>
                        </td>
                        <td>
                          <Stake lovelace={aggregatorStatus.total_next_stakes_signers ?? 0} />
                        </td>
                      </tr>
                    </tbody>
                  </Table>
                </Card.Body>
              </Card>
            </Col>
          </Row>
        </Container>
      </div>
    </Container>
  );
}
