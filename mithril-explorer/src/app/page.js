"use client";

import React, {useEffect} from 'react';
import {useSelector} from "react-redux";
import {usePathname, useRouter, useSearchParams} from "next/navigation";
import dynamic from "next/dynamic";
import {Col, Form, Row, Stack, Tab, Tabs} from "react-bootstrap";
import EpochSettings from "../components/EpochSettings";
import PendingCertificate from '../components/PendingCertificate';
import SnapshotsList from '../components/Artifacts/SnapshotsList';
import MithrilStakeDistributionsList from "../components/Artifacts/MithrilStakeDistributionsList";
import {selectedAggregator as currentlySelectedAggregator} from "../store/settingsSlice";

// Disable SSR for the following components since they use data from the store that are not
// available server sides (because those data can be read from the local storage).
const AggregatorSetter = dynamic(() => import('../components/AggregatorSetter'), { ssr: false })
const IntervalSetter = dynamic(() => import('../components/IntervalSetter'), { ssr: false })

export default function Explorer() {
  const router = useRouter();
  const pathname = usePathname();
  const searchParams = useSearchParams();
  const selectedAggregator = useSelector(currentlySelectedAggregator);

  useEffect(() => {
    const params = new URLSearchParams(searchParams);
    params.set("aggregator", selectedAggregator);
    let new_path = pathname + "?" + params.toString();

    router.prefetch(new_path);
    router.push(new_path);
  }, [router, searchParams, pathname, selectedAggregator]);

  return (
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
  );
}
