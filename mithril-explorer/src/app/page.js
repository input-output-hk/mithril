"use client";

import React, { useEffect, useState } from "react";
import { useDispatch, useSelector } from "react-redux";
import { useSearchParams } from "next/navigation";
import { Stack, Tab, Tabs } from "react-bootstrap";
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
import ControlPanel from "#/ControlPanel";
import CardanoDbSnapshotsList from "#/Artifacts/CardanoDbSnapshotsList";
import CardanoDbV2SnapshotsList from "#/Artifacts/CardanoDbV2SnapshotsList";
import CardanoStakeDistributionsList from "#/Artifacts/CardanoStakeDistributionsList";
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

Chart.register(ArcElement, CategoryScale, LinearScale, BarElement, Title, Tooltip, Legend);
setChartJsDefaults(Chart);

const defaultTab = signedEntityType.CardanoImmutableFilesFull;

export default function Explorer() {
  const searchParams = useSearchParams();
  const dispatch = useDispatch();

  // Used to avoid infinite loop between the update of the url query and the navigation handling.
  const [isUpdatingAggregatorInUrl, setIsUpdatingAggregatorInUrl] = useState(false);
  const [enableCardanoTransactionTab, setEnableCardanoTransactionTab] = useState(false);
  const [enableCardanoStakeDistributionTab, setEnableCardanoStakeDistributionTab] = useState(false);
  const [enableCardanoDbV2Tab, setEnableCardanoDbV2Tab] = useState(false);
  const [currentTab, setCurrentTab] = useState(defaultTab);
  const selectedAggregator = useSelector(currentlySelectedAggregator);
  const selectedAggregatorSignedEntities = useSelector((state) =>
    currentAggregatorSignedEntities(state),
  );

  useEffect(() => {
    setEnableCardanoTransactionTab(
      selectedAggregatorSignedEntities.includes(signedEntityType.CardanoTransactions),
    );
    setEnableCardanoStakeDistributionTab(
      selectedAggregatorSignedEntities.includes(signedEntityType.CardanoStakeDistribution),
    );
    setEnableCardanoDbV2Tab(selectedAggregatorSignedEntities.includes(signedEntityType.CardanoDb));
  }, [selectedAggregatorSignedEntities]);

  useEffect(() => {
    if (!enableCardanoTransactionTab && currentTab === signedEntityType.CardanoTransactions) {
      setCurrentTab(defaultTab);
    }
  }, [currentTab, enableCardanoTransactionTab]);

  // Global mithril client wasm init
  useEffect(() => {
    const initMithrilClient = async () => {
      const wasmClient = await import("@mithril-dev/mithril-client-wasm");
      await wasmClient.default();
    };

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
      <ControlPanel />
      <Tabs activeKey={currentTab} onSelect={(key) => setCurrentTab(key)}>
        <Tab title="Cardano Db" eventKey={signedEntityType.CardanoImmutableFilesFull}>
          <CardanoDbSnapshotsList />
        </Tab>
        {enableCardanoDbV2Tab && (
          <Tab title="Cardano Db v2" eventKey={signedEntityType.CardanoDb}>
            <CardanoDbV2SnapshotsList />
          </Tab>
        )}
        {enableCardanoTransactionTab && (
          <Tab title="Cardano Transactions" eventKey={signedEntityType.CardanoTransactions}>
            <CardanoTransactionsSnapshotsList />
          </Tab>
        )}
        {enableCardanoStakeDistributionTab && (
          <Tab
            title="Cardano Stake Distribution"
            eventKey={signedEntityType.CardanoStakeDistribution}>
            <CardanoStakeDistributionsList />
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
