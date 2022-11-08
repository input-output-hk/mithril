import React, {useEffect} from 'react';
import PendingCertificate from '../components/PendingCertificate';
import SnapshotsList from '../components/SnapshotsList';
import Head from "next/head";
import Image from "next/image";
import {Col, Form, Row, Stack} from "react-bootstrap";
import styles from "../styles/Home.module.css";
import AggregatorSetter from "../components/AggregatorSetter";
import {useRouter} from "next/router";
import EpochSettings from "../components/EpochSettings";
import {useSelector} from "react-redux";
import IntervalSetter from "../components/IntervalSetter";

export default function Explorer() {
  const router = useRouter();
  const selectedAggregator = useSelector((state) => state.settings.selectedAggregator);

  useEffect(() => {
    router.push({query: {aggregator: selectedAggregator}}).then(() => {
    });
  }, [selectedAggregator]);

  return (
    <>
      <Head>
        <title>Mithril Explorer</title>
        <meta name="description" content="Explore a Mithril Network"/>
        <link rel="icon" href="/explorer/favicon.ico"/>
      </Head>

      <div className={styles.container}>
        <main className={styles.main}>
          <h1 className={styles.title}>Mithril Explorer</h1>
          <Stack gap={3}>
            <Form>
              <Row xs={1} sm={2}>
                <AggregatorSetter/>
                <IntervalSetter/>
              </Row>
            </Form>
            <Row>
              <Col xs={12} sm={4} lg={3} xl={2}>
                <EpochSettings/>
              </Col>
              <Col xs={12} sm={8} lg={9} xl={10}>
                <PendingCertificate/>
              </Col>
            </Row>
            <SnapshotsList/>
          </Stack>
        </main>
      </div>

      <footer className={styles.footer}>
        <span className={styles.logo}>
          <Image src="/explorer/logo.png" alt="Mithril Logo" width={32} height={32}/>
        </span>{' '}
        <a href="https://mithril.network/doc">
          Go back to mithril documentation
        </a>
      </footer>
    </>
  );
}
