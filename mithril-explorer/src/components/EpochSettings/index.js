import React, {useEffect, useState} from 'react';
import {Card, ListGroup} from "react-bootstrap";
import Link from "next/link";
import RawJsonButton from "../RawJsonButton";
import {useSelector} from "react-redux";
import ProtocolParameters from "../ProtocolParameters";
import {selectedAggregator} from "../../store/settingsSlice";
import {checkUrl} from "../../utils";

export default function EpochSettings(props) {
  const [epochSettings, setEpochSettings] = useState({});
  const currentAggregator = useSelector((state) => state.settings.selectedAggregator);
  const epochSettingsEndpoint = useSelector((state) => `${selectedAggregator(state)}/epoch-settings`);
  const autoUpdate = useSelector((state) => state.settings.autoUpdate);
  const updateInterval = useSelector((state) => state.settings.updateInterval);
  const [registrationPageUrl, setRegistrationPageUrl] = useState(undefined);

  useEffect(() => {
    if (!autoUpdate) {
      return;
    }

    let fetchEpochSettings = () => {
      fetch(epochSettingsEndpoint)
        .then(response => response.status === 200 ? response.json() : {})
        .then(data => setEpochSettings(data))
        .catch(error => {
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
      if (checkUrl(currentAggregator) && Number.isInteger(epochSettings?.epoch)) {
        const params = new URLSearchParams();
        params.set("aggregator", currentAggregator);
        params.set("epoch", epochSettings.epoch);

        setRegistrationPageUrl(`/registrations?${params.toString()}`)
      }
    },
    [currentAggregator, epochSettings]
  );

  return (
    <div>
      <h2>
        Epoch Settings
        <RawJsonButton href={epochSettingsEndpoint} variant="outline-light" size="sm"/>
      </h2>

      <Card>
        <Card.Body>
          <Card.Title>Current Epoch</Card.Title>
          <ListGroup variant="flush">
            <ListGroup.Item>
              {epochSettings.epoch}{' '}
              {registrationPageUrl &&
                <Link href={registrationPageUrl}>Registrations</Link>
              }
            </ListGroup.Item>
          </ListGroup>
          <Card.Title>Protocol Parameters</Card.Title>
          <ProtocolParameters protocolParameters={epochSettings.protocol}/>
          <Card.Title>Next Protocol Parameters</Card.Title>
          <ProtocolParameters protocolParameters={epochSettings.next_protocol}/>
        </Card.Body>
      </Card>
    </div>
  );
}