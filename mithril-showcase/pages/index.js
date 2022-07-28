import React, { useState, useEffect } from 'react';
import PendingCertificate from '../components/PendingCertificate';
import SnapshotsList from '../components/SnapshotsList';
import styles from "../styles/Home.module.css";
import Head from "next/head";
import {Form, Stack, Button, Row, Col, InputGroup, Navbar, Container, Nav} from "react-bootstrap";
import Image from "next/image";

const available_aggregators = [
  "http://aggregator.api.mithril.network/aggregator",
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

export default function Showcase() {
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
        <title>Mithril Showcase</title>
        <meta name="description" content="Showcase of a Mithril Network" />
        <link rel="icon" href="/favicon.ico" />
      </Head>

      {/*<Navbar className="border" fixed="bottom" bg="light">*/}
      {/*  <Container style={{margin: "unset"}}>*/}
      {/*    <Navbar.Brand href="#home">*/}
      {/*      <img src="/logo.png" width="32" height="32" alt="Mithril Logo" />{' '}*/}
      {/*    </Navbar.Brand>*/}
      {/*    <Navbar.Collapse className="">*/}
      {/*      <Navbar.Text>*/}
      {/*        <a href="https://mithril.network/doc">Back to mithril documentation</a>*/}
      {/*      </Navbar.Text>*/}
      {/*    </Navbar.Collapse>*/}
      {/*  </Container>*/}
      {/*</Navbar>*/}
      
      <div className={styles.container}>
        <main className={styles.main}>
          <h1 className={styles.title}>Mithril Showcase</h1>
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
          <Image src="/logo.png" alt="Mithril Logo" width={32} height={32} />
        </span>{' '}
        <a href="https://mithril.network/doc">
          Go back to mithril documentation
        </a>
      </footer>
    </>
  );
}
