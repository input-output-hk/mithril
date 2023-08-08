"use client";

import React, {useEffect} from 'react';
import {useSelector} from "react-redux";
import {usePathname, useRouter, useSearchParams} from "next/navigation";
import {Col, Form, Row, Stack, Tab, Tabs} from "react-bootstrap";
import AggregatorSetter from "../components/AggregatorSetter";
import EpochSettings from "../components/EpochSettings";
import IntervalSetter from "../components/IntervalSetter";
import PendingCertificate from '../components/PendingCertificate';
import SnapshotsList from '../components/Artifacts/SnapshotsList';
import MithrilStakeDistributionsList from "../components/Artifacts/MithrilStakeDistributionsList";
import {selectedAggregator as currentlySelectedAggregator} from "../store/settingsSlice";

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
