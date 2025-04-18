import React, { useEffect, useState } from "react";
import { Card, Row, Col, Collapse, Container, Stack } from "react-bootstrap";
import { useSelector } from "react-redux";
import EpochSettings from "#/ControlPanel/EpochSettings";
import LinkButton from "#/LinkButton";
import RawJsonButton from "#/RawJsonButton";
import ProtocolParameters from "#/ProtocolParameters";
import QuestionTooltip from "#/QuestionTooltip";
import Stake from "#/Stake";
import { selectedAggregator } from "@/store/settingsSlice";
import { checkUrl, formatStake, percent } from "@/utils";
import { fetchAggregator } from "@/aggregator-api";

function InfoGroupCard({ children, title, ...props }) {
  return (
    <Col xs={12} sm={6} md={4} lg={3} xl={2} className="mb-3" {...props}>
      <Card className="h-100" {...props}>
        <Card.Body>
          <h5>{title}</h5>
          <div className="ps-1">{children}</div>
        </Card.Body>
      </Card>
    </Col>
  );
}

function InfoRow({ label, children, className = "", ...props }) {
  return (
    <>
      <div className={`d-flex justify-content-between ${className}`} {...props}>
        <div className="me-2 flex-fill">
          <em>{label}:</em>
        </div>
        <div className="text-end">{children}</div>
      </div>
      <hr className="my-2" />
    </>
  );
}

function PercentTooltip({ value, total, ...props }) {
  return <QuestionTooltip tooltip={`${value} out of ${total}`} {...props} />;
}

export default function AggregatorStatus({ showContent = true }) {
  const [aggregatorStatus, setAggregatorStatus] = useState({});
  const [aggregatorVersion, setAggregatorVersion] = useState({});
  const currentAggregator = useSelector((state) => state.settings.selectedAggregator);
  const aggregatorStatusEndpoint = useSelector((state) => `${selectedAggregator(state)}/status`);
  const [registrationPageUrl, setRegistrationPageUrl] = useState(undefined);
  const [inOutRegistrationsPageUrl, setInOutRegistrationsPageUrl] = useState(undefined);
  const refreshSeed = useSelector((state) => state.settings.refreshSeed);
  const updateInterval = useSelector((state) => state.settings.updateInterval);
  const [fallbackToEpochSetting, setFallbackToEpochSetting] = useState(false);

  useEffect(() => {
    let fetchAggregatorStatus = () => {
      fetchAggregator(aggregatorStatusEndpoint)
        .then((response) => (response.status === 200 ? response.json() : {}))
        .then((data) => {
          setAggregatorStatus(data);
          setFallbackToEpochSetting(false);
        })
        .catch((error) => {
          setAggregatorStatus({});
          setFallbackToEpochSetting(true);
          // todo: uncomment when the fallback is removed
          // console.error("Fetch status error:", error);
        });
    };

    // Fetch it once without waiting
    fetchAggregatorStatus();

    if (updateInterval) {
      const interval = setInterval(fetchAggregatorStatus, updateInterval);
      return () => clearInterval(interval);
    }
  }, [aggregatorStatusEndpoint, updateInterval, refreshSeed]);

  useEffect(() => {
    if (!checkUrl(currentAggregator)) {
      return;
    }

    const split_version = aggregatorStatus?.aggregator_node_version?.split("+") ?? [];
    setAggregatorVersion({
      number: split_version[0] ?? "0.0.0",
      sha: split_version[1] ?? undefined,
    });

    const params = new URLSearchParams();
    params.set("aggregator", currentAggregator);
    setInOutRegistrationsPageUrl(`/registrations-in-out?${params.toString()}`);

    if (Number.isInteger(aggregatorStatus?.epoch)) {
      const params = new URLSearchParams();
      params.set("aggregator", currentAggregator);
      params.set("epoch", aggregatorStatus.epoch);

      setRegistrationPageUrl(`/registrations?${params.toString()}`);
    }
  }, [currentAggregator, aggregatorStatus]);

  return fallbackToEpochSetting ? (
    <Stack direction="horizontal">
      <Collapse in={showContent}>
        <div id="contentRow">
          <EpochSettings />
        </div>
      </Collapse>
    </Stack>
  ) : (
    <Container fluid>
      <Collapse in={showContent}>
        <div id="contentRow">
          <Row className="d-flex flex-wrap justify-content-md-center">
            <InfoGroupCard title={`Epoch ${aggregatorStatus.epoch}`}>
              <InfoRow label="Cardano Network" className="text-capitalize">
                {aggregatorStatus.cardano_network}
              </InfoRow>
              <InfoRow label="Cardano Era">{aggregatorStatus.cardano_era}</InfoRow>
              <InfoRow label="Mithril Era" className="text-capitalize">
                {aggregatorStatus.mithril_era}
              </InfoRow>
              <InfoRow label="Aggreg." className="text-capitalize">
                {aggregatorStatus.aggregation_type?.replace("StmAggrSig", "")}
              </InfoRow>
            </InfoGroupCard>

            <InfoGroupCard title="SPOs">
              <InfoRow label="Current Signers">{aggregatorStatus.total_signers ?? 0}</InfoRow>
              <InfoRow label="Next Signers">{aggregatorStatus.total_next_signers ?? 0}</InfoRow>
              <InfoRow label="Cardano Adoption">
                {percent(aggregatorStatus.total_signers, aggregatorStatus.total_cardano_spo)}%{" "}
                <PercentTooltip
                  value={aggregatorStatus.total_signers ?? 0}
                  total={aggregatorStatus.total_cardano_spo ?? 0}
                />
              </InfoRow>
            </InfoGroupCard>

            <InfoGroupCard title="Stakes">
              <InfoRow label="Current Signers">
                <Stake lovelace={aggregatorStatus.total_stakes_signers ?? 0} />
              </InfoRow>
              <InfoRow label="Next Signers">
                <Stake lovelace={aggregatorStatus.total_next_stakes_signers ?? 0} />
              </InfoRow>
              <InfoRow label="Cardano Adoption">
                {percent(
                  aggregatorStatus.total_stakes_signers,
                  aggregatorStatus.total_cardano_stake,
                )}
                %{" "}
                <PercentTooltip
                  value={formatStake(aggregatorStatus.total_stakes_signers) ?? 0}
                  total={formatStake(aggregatorStatus.total_cardano_stake) ?? 0}
                />
              </InfoRow>
            </InfoGroupCard>

            <InfoGroupCard title="Protocol Parameters">
              <em>Current:</em>
              <ProtocolParameters protocolParameters={aggregatorStatus.protocol} padding={2} />
              <em>Next:</em>
              <ProtocolParameters protocolParameters={aggregatorStatus.next_protocol} padding={2} />
            </InfoGroupCard>

            <InfoGroupCard title="Versions">
              <InfoRow label="Aggregator">
                {aggregatorVersion.number}
                {aggregatorVersion.sha && <em> ({aggregatorVersion.sha})</em>}
              </InfoRow>
              <InfoRow label="Cardano">{aggregatorStatus.cardano_node_version}</InfoRow>
            </InfoGroupCard>
          </Row>
        </div>
      </Collapse>
      <Row>
        <Stack
          direction="horizontal"
          gap={1}
          className="align-items-stretch justify-content-sm-center border bg-light rounded p-2">
          <LinkButton
            className="ms-auto"
            href={registrationPageUrl ?? "#"}
            disabled={registrationPageUrl === undefined}>
            <i className="bi bi-pen"></i> Registered Signers
          </LinkButton>
          <LinkButton
            href={inOutRegistrationsPageUrl ?? "#"}
            disabled={inOutRegistrationsPageUrl === undefined}>
            <i className="bi bi-arrow-left-right translate-middle-y"></i> In/Out Registrations
          </LinkButton>
          <RawJsonButton
            href={aggregatorStatusEndpoint}
            variant="outline-secondary"
            tooltip="Aggregator Status Raw JSON"
            className="ms-auto"
          />
        </Stack>
      </Row>
    </Container>
  );
}
