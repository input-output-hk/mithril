import React, {useEffect, useState} from 'react';
import {Card, ListGroup} from "react-bootstrap";
import RawJsonButton from "../RawJsonButton";
import {useSelector} from "react-redux";
import ProtocolParameters from "../ProtocolParameters";

export default function EpochSettings(props) {
  const [epochSettings, setEpochSettings] = useState({});
  const aggregator = useSelector((state) => state.settings.selectedAggregator);
  const autoUpdate = useSelector((state) => state.settings.autoUpdate);
  const updateInterval = useSelector((state) => state.settings.updateInterval);

  useEffect(() => {
    if (!autoUpdate) {
      return;
    }

    let fetchEpochSettings = () => {
      fetch(`${aggregator}/epoch-settings`)
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
  }, [aggregator, updateInterval, autoUpdate]);

  return (
    <div>
      <h2>
        Epoch Settings
        <RawJsonButton href={`${aggregator}/epoch-settings`} variant="outline-light" size="sm"/>
      </h2>

      <Card>
        <Card.Body>
          <Card.Title>Current Epoch</Card.Title>
          <ListGroup variant="flush">
            <ListGroup.Item>{epochSettings.epoch}</ListGroup.Item>
          </ListGroup>
          <Card.Title>Protocol Parameters</Card.Title>
          <ProtocolParameters protocolParameters={epochSettings.protocol}/>
        </Card.Body>
      </Card>
    </div>
  );
}