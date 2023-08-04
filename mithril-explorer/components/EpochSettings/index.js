import React, {useEffect, useState} from 'react';
import {Card, ListGroup} from "react-bootstrap";
import RawJsonButton from "../RawJsonButton";
import {useSelector} from "react-redux";
import ProtocolParameters from "../ProtocolParameters";
import {selectedAggregator} from "../../store/settingsSlice";

export default function EpochSettings(props) {
  const [epochSettings, setEpochSettings] = useState({});
  const epochSettingsEndpoint = useSelector((state) => `${selectedAggregator(state)}/epoch-settings`);
  const autoUpdate = useSelector((state) => state.settings.autoUpdate);
  const updateInterval = useSelector((state) => state.settings.updateInterval);

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
            <ListGroup.Item>{epochSettings.epoch}</ListGroup.Item>
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