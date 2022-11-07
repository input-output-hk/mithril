import React, { useEffect, useState } from 'react';
import {Card, ListGroup} from "react-bootstrap";
import RawJsonButton from "../RawJsonButton";
import {useSelector} from "react-redux";

export default function EpochSettings(props) {
  const [epochSettings, setEpochSettings] = useState({});
  const autoUpdate = useSelector((state) => state.settings.autoUpdate);

  useEffect(() => {
    if (!autoUpdate) {
      return;
    }

    let fetchEpochSettings = () => {
      fetch(`${props.aggregator}/epoch-settings`)
        .then(response => response.status === 200 ? response.json() : {})
        .then(data => setEpochSettings(data))
        .catch(error => {
          setEpochSettings({});
          console.error("Fetch epoch-settings error:", error);
        });
    };

    // Fetch it once without waiting
    fetchEpochSettings();

    const interval = setInterval(fetchEpochSettings, props.updateInterval);
    return () => clearInterval(interval);
  }, [props.aggregator, props.updateInterval, autoUpdate]);
  
  return (
    <div>
      <h2>
        Epoch Settings
        <RawJsonButton href={`${props.aggregator}/epoch-settings`} variant="outline-light" size="sm" />
      </h2>

      <Card>
        <Card.Body>
          <Card.Title>Current Epoch</Card.Title>
          <ListGroup variant="flush">
            <ListGroup.Item>{epochSettings.epoch}</ListGroup.Item>
          </ListGroup>
          <Card.Title>Protocol Parameters</Card.Title>
          <ListGroup horizontal>
            <ListGroup.Item>K: {epochSettings.protocol?.k}</ListGroup.Item>
            <ListGroup.Item>M: {epochSettings.protocol?.m}</ListGroup.Item>
            <ListGroup.Item>Phi: {epochSettings.protocol?.phi_f}</ListGroup.Item>
          </ListGroup>
        </Card.Body>
      </Card>
    </div>
  );
}