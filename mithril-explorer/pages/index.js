import React, { useState  } from 'react';
import PendingCertificate from '../components/PendingCertificate';
import SnapshotsList from '../components/SnapshotsList';
import Head from "next/head";
import Image from "next/image";
import {Form, Stack, Button, Row, Col, InputGroup} from "react-bootstrap";
import styles from "../styles/Home.module.css";

const available_aggregators = [
  "https://aggregator.api.mithril.network/aggregator",
  "http://localhost:8080/aggregator"
];

function IntervalSetter(props) {
  function handleChange(event) {
    props.onIntervalChange(parseInt(event.target.value));
  }
  
  function handleClick() {
    props.onStartStopPress(!props.isStartStopPressed);
  }
  
  return (
    <Form.Group as={Col} className={props.className}>
      <Form.Label>Update Interval:</Form.Label>
      <InputGroup>
        <Button type="button" onClick={handleClick} variant={props.isStartStopPressed ? "primary" : "success"}>
          {props.isStartStopPressed ? "Pause ⏸" : "Resume ▶"}
        </Button>
        <Form.Select value={props.interval} onChange={handleChange}>
          <option value={1000}>1 seconds</option>
          <option value={5000}>5 seconds</option>
          <option value={10000}>10 seconds</option>
        </Form.Select>
      </InputGroup>
    </Form.Group>
  );
}

function AggregatorSetter(props) {
  function handleChange(event) {
    props.onAggregatorChange(event.target.value);
  }

  return (
    <Form.Group as={Col} className={props.className}>
      <Form.Label>Aggregator:</Form.Label>
      <Form.Select value={props.aggregator} onChange={handleChange}>
        {props.availableAggregators.map((aggregator, index) =>
          <option key={"agg-" + index} value={aggregator}>{aggregator}</option>
        )}
      </Form.Select>
    </Form.Group>
  );
}

export default function Explorer() {
  const [aggregator, setAggregator] = useState(available_aggregators[0]);
  const [interval, setInterval] = useState(10000);
  const [autoUpdate, setAutoUpdate] = useState(true);

  function handleApiChange(api) {
    setAggregator(api);
  }
  
  function handleStartStopButtonPress(isPressed) {
    setAutoUpdate(isPressed);
  }

  function handleIntervalChange(interval) {
    setInterval(interval);
  }

  return (
    <>
      <Head>
        <title>Mithril Explorer</title>
        <meta name="description" content="Explore a Mithril Network" />
        <link rel="icon" href="/explorer/favicon.ico" />
      </Head>
      
      <div className={styles.container}>
        <main className={styles.main}>
          <h1 className={styles.title}>Mithril Explorer</h1>
          <Stack gap={3}>
            <Form>
              <Row xs={1} sm={2}>
                <AggregatorSetter aggregator={aggregator} onAggregatorChange={handleApiChange} availableAggregators={available_aggregators}  />
                <IntervalSetter
                  interval={interval}
                  onIntervalChange={handleIntervalChange}
                  isStartStopPressed={autoUpdate}
                  onStartStopPress={handleStartStopButtonPress} />
              </Row>
            </Form>
            <PendingCertificate aggregator={aggregator} updateInterval={interval} autoUpdate={autoUpdate} />
            <SnapshotsList aggregator={aggregator} updateInterval={interval} autoUpdate={autoUpdate} />
          </Stack>
        </main>
      </div>
      
      <footer className={styles.footer}>
        <span className={styles.logo}>
          <Image src="/explorer/logo.png" alt="Mithril Logo" width={32} height={32} />
        </span>{' '}
        <a href="https://mithril.network/doc">
          Go back to mithril documentation
        </a>
      </footer>
    </>
  );
}
