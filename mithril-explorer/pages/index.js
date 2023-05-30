import React, {useEffect} from 'react';
import {useRouter} from "next/router";
import {useSelector} from "react-redux";
import Head from "next/head";
import Image from "next/image";
import {Col, Form, Row, Stack, Tab, Tabs} from "react-bootstrap";
import styles from "../styles/Home.module.css";
import AggregatorSetter from "../components/AggregatorSetter";
import EpochSettings from "../components/EpochSettings";
import IntervalSetter from "../components/IntervalSetter";
import PendingCertificate from '../components/PendingCertificate';
import SnapshotsList from '../components/Artifacts/SnapshotsList';
import MithrilStakeDistributionsList from "../components/Artifacts/MithrilStakeDistributionsList";

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
        <link rel="icon" href="/explorer/logo.svg?v=1" type="image/svg+xml"/>
      </Head>

      <div className={styles.container}>
        <main className={styles.main}>
          <h1 className={styles.title}>
            <Image src="/explorer/logo.png" alt="Mithril Logo" width={55} height={55}/> Mithril Explorer
          </h1>
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
            <Tabs defaultActiveKey="snapshots">
              <Tab title="Snapshots" eventKey="snapshots">
                <SnapshotsList/>
              </Tab>
              <Tab title="Mithril Stake Distribution" eventKey="mithrilStakeDistribution">
                <MithrilStakeDistributionsList/>
              </Tab>
            </Tabs>
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
