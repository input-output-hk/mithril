import React, { useEffect, useState } from "react";
import { Card, ButtonGroup, DropdownButton } from "react-bootstrap";
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
  const autoUpdate = useSelector((state) => state.settings.autoUpdate);
  const updateInterval = useSelector((state) => state.settings.updateInterval);
  const [registrationPageUrl, setRegistrationPageUrl] = useState(undefined);
  const [inOutRegistrationsPageUrl, setInOutRegistrationsPageUrl] = useState(undefined);

  useEffect(() => {
    if (!autoUpdate) {
      return;
    }

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

    const interval = setInterval(fetchEpochSettings, updateInterval);
    return () => clearInterval(interval);
  }, [epochSettingsEndpoint, updateInterval, autoUpdate]);

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
    <div>
      <h2>
        Epoch Settings
        <RawJsonButton href={epochSettingsEndpoint} variant="outline-light" size="sm" />
      </h2>

      <Card>
        <Card.Body>
          <Card.Title>Current Epoch</Card.Title>
          <div className="mb-2 ps-3">{epochSettings.epoch}</div>
          <Card.Title>Protocol Parameters</Card.Title>
          <ProtocolParameters className="mb-2" protocolParameters={epochSettings.protocol} />
          <Card.Title>Next Protocol Parameters</Card.Title>
          <ProtocolParameters protocolParameters={epochSettings.next_protocol} />
        </Card.Body>
        {registrationPageUrl && inOutRegistrationsPageUrl && (
          <Card.Footer className="text-center">
            <ButtonGroup>
              <LinkButton href={registrationPageUrl}>Registered Signers</LinkButton>
              <DropdownButton as={ButtonGroup}>
                <LinkButton href={inOutRegistrationsPageUrl} variant="light">
                  In/Out Registrations
                </LinkButton>
              </DropdownButton>
            </ButtonGroup>
          </Card.Footer>
        )}
      </Card>
    </div>
  );
}
