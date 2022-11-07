import React, {useEffect, useState} from 'react';
import PendingCertificate from '../components/PendingCertificate';
import SnapshotsList from '../components/SnapshotsList';
import Head from "next/head";
import Image from "next/image";
import {Form, Stack, Button, Row, Col, InputGroup, Container} from "react-bootstrap";
import styles from "../styles/Home.module.css";
import AggregatorSetter from "../components/AggregatorSetter";
import available_aggregators from "../aggregators-list";
import {useRouter} from "next/router";
import EpochSettings from "../components/EpochSettings";
import {useDispatch, useSelector} from "react-redux";
import {setUpdateInterval, toggleAutoUpdate} from "../store/settingsSlice";

function IntervalSetter(props) {
  const autoUpdate = useSelector((state) => state.settings.autoUpdate);
  const updateInterval = useSelector((state) => state.settings.updateInterval);
  const dispatch = useDispatch();

  return (
    <Form.Group as={Col} className={props.className}>
      <Form.Label>Update Interval:</Form.Label>
      <InputGroup>
        <Button type="button" onClick={() => dispatch(toggleAutoUpdate())} variant={autoUpdate ? "primary" : "success"}>
          {autoUpdate ? "Pause ⏸" : "Resume ▶"}
        </Button>
        <Form.Select value={updateInterval} onChange={(e) => dispatch(setUpdateInterval(e.target.value))}>
          <option value={1000}>1 seconds</option>
          <option value={5000}>5 seconds</option>
          <option value={10000}>10 seconds</option>
        </Form.Select>
      </InputGroup>
    </Form.Group>
  );
}

export default function Explorer() {
  const router = useRouter();
  const [aggregator, setAggregator] = useState(available_aggregators[0]);
  
  useEffect(() => {
    if (router.query?.aggregator && router.query?.aggregator !== aggregator) {
      // const autoUpdateState = autoUpdate;
      
      // setAutoUpdate(false);
      setAggregator(router.query.aggregator);
      // setAutoUpdate(autoUpdateState);
    }
  }, [router.query]);

  function handleApiChange(api) {
    setAggregator(api);
    router.push({query: {aggregator: api}}).then(() => {});
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
                <AggregatorSetter
                  aggregator={aggregator}
                  onAggregatorChange={handleApiChange}
                  defaultAvailableAggregators={available_aggregators} />
                <IntervalSetter />
              </Row>
            </Form>
            <Row>
              <Col xs={12} sm={4} lg={3} xl={2}>
                <EpochSettings aggregator={aggregator} />
              </Col>
              <Col xs={12} sm={8} lg={9} xl={10}>
                <PendingCertificate aggregator={aggregator} />
              </Col>
            </Row>
            <SnapshotsList aggregator={aggregator} />
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
