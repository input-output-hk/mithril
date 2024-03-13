"use client";

import React, { useEffect, useState } from "react";
import { useDispatch, useSelector } from "react-redux";
import { useSearchParams } from "next/navigation";
import dynamic from "next/dynamic";
import { Col, Form, Row, Stack, Tab, Tabs } from "react-bootstrap";
import {
  ArcElement,
  BarElement,
  CategoryScale,
  Chart,
  Legend,
  LinearScale,
  Title,
  Tooltip,
} from "chart.js";
import initMithrilClient from "@mithril-dev/mithril-client-wasm";
import EpochSettings from "#/EpochSettings";
import PendingCertificate from "#/PendingCertificate";
import CardanoDbSnapshotsList from "#/Artifacts/CardanoDbSnapshotsList";
import CardanoTransactionsSnapshotsList from "#/Artifacts/CardanoTransactionsSnapshotsList";
import CertificatesList from "#/Artifacts/CertificatesList";
import MithrilStakeDistributionsList from "#/Artifacts/MithrilStakeDistributionsList";
import { aggregatorSearchParam, signedEntityType } from "@/constants";
import { setChartJsDefaults } from "@/charts";
import {
  selectAggregator,
  selectedAggregator as currentlySelectedAggregator,
  selectedAggregatorSignedEntities as currentAggregatorSignedEntities,
} from "@/store/settingsSlice";
import { updatePoolsForAggregator } from "@/store/poolsSlice";

// Disable SSR for the following components since they use data from the store that are not
// available server sides (because those data can be read from the local storage).
const AggregatorSetter = dynamic(() => import("#/AggregatorSetter"), { ssr: false });
const IntervalSetter = dynamic(() => import("#/IntervalSetter"), {
  ssr: false,
});

Chart.register(ArcElement, CategoryScale, LinearScale, BarElement, Title, Tooltip, Legend);
setChartJsDefaults(Chart);

const defaultTab = signedEntityType.CardanoImmutableFilesFull;

export default function Explorer() {
  const searchParams = useSearchParams();
  const dispatch = useDispatch();

  // Used to avoid infinite loop between the update of the url query and the navigation handling.
  const [isUpdatingAggregatorInUrl, setIsUpdatingAggregatorInUrl] = useState(false);
  const [enableCardanoTransactionTab, setEnableCardanoTransactionTab] = useState(false);
  const [currentTab, setCurrentTab] = useState(defaultTab);
  const selectedAggregator = useSelector(currentlySelectedAggregator);
  const selectedAggregatorSignedEntities = useSelector((state) =>
    currentAggregatorSignedEntities(state),
  );

  useEffect(() => {
    setEnableCardanoTransactionTab(
      selectedAggregatorSignedEntities.includes(signedEntityType.CardanoTransactions),
    );
  }, [selectedAggregatorSignedEntities]);

  useEffect(() => {
    if (!enableCardanoTransactionTab && currentTab === signedEntityType.CardanoTransactions) {
      setCurrentTab(defaultTab);
    }
  }, [currentTab, enableCardanoTransactionTab]);

  // Global mithril client wasm init
  useEffect(() => {
    initMithrilClient().catch((err) => console.error("Mithril-client-wasm init error:", err));
  }, []);

  // Update the aggregator in the url query
  useEffect(() => {
    const aggregatorInUrl = searchParams.get(aggregatorSearchParam);

    if (selectedAggregator !== aggregatorInUrl) {
      const params = new URLSearchParams();
      params.set("aggregator", selectedAggregator);

      setIsUpdatingAggregatorInUrl(true);
      window.history.pushState(null, "", `?${params.toString()}`);
    }

    dispatch(updatePoolsForAggregator(selectedAggregator));
  }, [selectedAggregator]); // eslint-disable-line react-hooks/exhaustive-deps

  // Allow navigation to work (previous, next)
  useEffect(() => {
    function allowNavigation() {
      if (isUpdatingAggregatorInUrl) {
        setIsUpdatingAggregatorInUrl(false);
      } else {
        const aggregatorInUrl = searchParams.get("aggregator");

        dispatch(selectAggregator(aggregatorInUrl));
      }
    }

    allowNavigation();
  }, [searchParams]); // eslint-disable-line react-hooks/exhaustive-deps

  return (
    <Stack gap={3}>
      <Form>
        <Row xs={1} sm={2} className="row-gap-2">
          <AggregatorSetter />
          <IntervalSetter />
        </Row>
      </Form>
      <Row className="row-gap-3">
        <Col xs={12} sm={4} lg={3} xl={2}>
          <EpochSettings />
        </Col>
        <Col xs={12} sm={8} lg={9} xl={10}>
          <PendingCertificate />
        </Col>
      </Row>
      <Tabs activeKey={currentTab} onSelect={(key) => setCurrentTab(key)}>
        <Tab title="Cardano Db" eventKey={signedEntityType.CardanoImmutableFilesFull}>
          <CardanoDbSnapshotsList />
        </Tab>
        {enableCardanoTransactionTab && (
          <Tab title="Cardano Transactions" eventKey={signedEntityType.CardanoTransactions}>
            <CardanoTransactionsSnapshotsList />
          </Tab>
        )}
        <Tab
          title="Mithril Stake Distribution"
          eventKey={signedEntityType.MithrilStakeDistribution}>
          <MithrilStakeDistributionsList />
        </Tab>
        <Tab title="Certificates" eventKey="certificates">
          <CertificatesList />
        </Tab>
      </Tabs>
    </Stack>
  );
}
