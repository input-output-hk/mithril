import React, { useEffect, useState } from "react";
import { Card, Row, Col, Stack, Container } from "react-bootstrap";
import { useSelector } from "react-redux";
import LinkButton from "#/LinkButton";
import RawJsonButton from "#/RawJsonButton";
import ProtocolParameters from "#/ProtocolParameters";
import { selectedAggregator } from "@/store/settingsSlice";
import { checkUrl } from "@/utils";

export default function EpochSettings() {
  const [epochSettings, setEpochSettings] = useState({});
  const currentAggregator = useSelector((state) => state.settings.selectedAggregator);
  const epochSettingsEndpoint = useSelector(
    (state) => `${selectedAggregator(state)}/epoch-settings`,
  );
  const refreshSeed = useSelector((state) => state.settings.refreshSeed);
  const updateInterval = useSelector((state) => state.settings.updateInterval);
  const [registrationPageUrl, setRegistrationPageUrl] = useState(undefined);
  const [inOutRegistrationsPageUrl, setInOutRegistrationsPageUrl] = useState(undefined);

  useEffect(() => {
    let fetchEpochSettings = () => {
      fetch(epochSettingsEndpoint)
        .then((response) => (response.status === 200 ? response.json() : {}))
        .then((data) => setEpochSettings(data))
        .catch((error) => {
          setEpochSettings({});
          console.error("Fetch epoch-settings error:", error);
        });
    };

    // Fetch it once without waiting
    fetchEpochSettings();

    if (updateInterval) {
      const interval = setInterval(fetchEpochSettings, updateInterval);
      return () => clearInterval(interval);
    }
  }, [epochSettingsEndpoint, updateInterval, refreshSeed]);

  useEffect(() => {
    if (checkUrl(currentAggregator)) {
      const params = new URLSearchParams();
      params.set("aggregator", currentAggregator);

      setInOutRegistrationsPageUrl(`/registrations-in-out?${params.toString()}`);
    }

    if (checkUrl(currentAggregator) && Number.isInteger(epochSettings?.epoch)) {
      const params = new URLSearchParams();
      params.set("aggregator", currentAggregator);
      params.set("epoch", epochSettings.epoch);

      setRegistrationPageUrl(`/registrations?${params.toString()}`);
    }
  }, [currentAggregator, epochSettings]);

  return (
    <Card>
      <Card.Body>
        <Container>
          <Row>
            <Col xs={12} md="auto">
              <h5>Current Epoch</h5>
              <div className="mb-2 ps-3">{epochSettings.epoch}</div>
            </Col>
            <Col xs={12} md="auto">
              <h5>Registration Protocol Parameters</h5>
              <ProtocolParameters
                protocolParameters={
                  epochSettings.signer_registration_protocol ?? epochSettings.next_protocol
                }
              />
            </Col>
          </Row>
        </Container>
      </Card.Body>
      <Card.Footer>
        <Stack
          direction="horizontal"
          gap={1}
          className="justify-content-md-end align-items-stretch justify-content-sm-center">
          <LinkButton
            href={registrationPageUrl ?? "#"}
            disabled={registrationPageUrl === undefined}>
            <i className="bi bi-pen"></i> Registered Signers
          </LinkButton>
          <LinkButton
            href={inOutRegistrationsPageUrl ?? "#"}
            disabled={inOutRegistrationsPageUrl === undefined}>
            <i className="bi bi-arrow-left-right translate-middle-y"></i> In/Out Registrations
          </LinkButton>
          <RawJsonButton href={epochSettingsEndpoint} variant="outline-secondary" />
        </Stack>
      </Card.Footer>
    </Card>
  );
}
