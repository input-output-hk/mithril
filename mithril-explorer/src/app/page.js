"use client";

import React, {useEffect, useState} from 'react';
import {useDispatch, useSelector} from "react-redux";
import {useRouter, useSearchParams} from "next/navigation";
import dynamic from "next/dynamic";
import {Col, Form, Row, Stack, Tab, Tabs} from "react-bootstrap";
import EpochSettings from "../components/EpochSettings";
import PendingCertificate from '../components/PendingCertificate';
import SnapshotsList from '../components/Artifacts/SnapshotsList';
import MithrilStakeDistributionsList from "../components/Artifacts/MithrilStakeDistributionsList";
import {selectAggregator, selectedAggregator as currentlySelectedAggregator} from "../store/settingsSlice";

// Disable SSR for the following components since they use data from the store that are not
// available server sides (because those data can be read from the local storage).
const AggregatorSetter = dynamic(() => import('../components/AggregatorSetter'), {ssr: false})
const IntervalSetter = dynamic(() => import('../components/IntervalSetter'), {ssr: false})

export default function Explorer() {
  const router = useRouter();
  const searchParams = useSearchParams();
  const dispatch = useDispatch();
  
  // Used to avoid infinite loop between the update of the url query and the navigation handling.
  const [isUpdatingAggregatorInUrl, setIsUpdatingAggregatorInUrl] = useState(false);
  const selectedAggregator = useSelector(currentlySelectedAggregator);

  // Update the aggregator in the url query
  useEffect(() => {
    const aggregatorInUrl = searchParams.get('aggregator');

    if (selectedAggregator !== aggregatorInUrl) {
      const params = new URLSearchParams();
      params.set("aggregator", selectedAggregator);
     
      setIsUpdatingAggregatorInUrl(true);
      router.push("?" + params.toString(), undefined, {shallow: true});
    }
  }, [selectedAggregator]);

  // Allow navigation to work (previous, next)
  useEffect(() => {
    if (isUpdatingAggregatorInUrl) {
      setIsUpdatingAggregatorInUrl(false);
    } else {
      const aggregatorInUrl = searchParams.get('aggregator');

      dispatch(selectAggregator(aggregatorInUrl));
    }
  }, [searchParams]);

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
