"use client";

import React, { useEffect, useState, useRef } from "react";
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
import CardanoBlocksTransactionsSnapshotsList from "#/Artifacts/CardanoBlocksTransactionsSnapshotsList";
import CardanoDbSnapshotsList from "#/Artifacts/CardanoDbSnapshotsList";
import CardanoStakeDistributionsList from "#/Artifacts/CardanoStakeDistributionsList";
import CardanoTransactionsSnapshotsList from "#/Artifacts/CardanoTransactionsSnapshotsList";
import CertificatesList from "#/Artifacts/CertificatesList";
import MithrilStakeDistributionsList from "#/Artifacts/MithrilStakeDistributionsList";
import default_available_aggregators from "@/aggregators-list.generated";
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

const certificateTab = "certificates";
const defaultTab = certificateTab;

function isDefaultAggregator(aggregator) {
  return default_available_aggregators.map((d) => d.url).includes(aggregator.url);
}

export default function Explorer() {
  const searchParams = useSearchParams();
  const dispatch = useDispatch();

  // Used to avoid infinite loop between the update of the url query and the navigation handling.
  const isUpdatingAggregatorInUrl = useRef(false);
  const selectedAggregator = useSelector(currentlySelectedAggregator);
  const selectedAggregatorSignedEntities = useSelector((state) =>
    currentAggregatorSignedEntities(state),
  );
  const enableCardanoTransactionTab = useSelector((state) =>
    currentAggregatorSignedEntities(state).includes(signedEntityType.CardanoTransactions),
  );
  const enableCardanoBlocksTransactionTab = useSelector((state) =>
    currentAggregatorSignedEntities(state).includes(signedEntityType.CardanoBlocksTransactions),
  );
  const enableCardanoStakeDistributionTab = useSelector((state) =>
    currentAggregatorSignedEntities(state).includes(signedEntityType.CardanoStakeDistribution),
  );
  const enableCardanoDbTab = useSelector((state) =>
    currentAggregatorSignedEntities(state).includes(signedEntityType.CardanoDb),
  );
  const [currentTab, setCurrentTab] = useState(defaultTab);
  const displayedTab = selectedAggregatorSignedEntities.includes(currentTab)
    ? currentTab
    : defaultTab;

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

    if (selectedAggregator?.url !== aggregatorInUrl) {
      const params = new URLSearchParams();
      params.set("aggregator", selectedAggregator.url);
      if (!isDefaultAggregator(selectedAggregator) && selectedAggregator?.genesisVerificationKey) {
        params.set("genesisVerificationKey", selectedAggregator.genesisVerificationKey);
      }

      isUpdatingAggregatorInUrl.current = true;
      window.history.pushState(null, "", `?${params.toString()}`);
    }

    dispatch(updatePoolsForAggregator(selectedAggregator.url));
  }, [selectedAggregator?.url]); // eslint-disable-line react-hooks/exhaustive-deps

  // Allow navigation to work (previous, next)
  useEffect(() => {
    function allowNavigation() {
      if (isUpdatingAggregatorInUrl.current) {
        isUpdatingAggregatorInUrl.current = false;
      } else {
        const aggregatorInUrl = searchParams.get("aggregator");
        const genesisVerificationKeyInUrl = searchParams.get("genesisVerificationKey");
        let aggregator = {
          url: aggregatorInUrl,
          genesisVerificationKey: genesisVerificationKeyInUrl ?? "",
        };

        dispatch(selectAggregator(aggregator));
      }
    }

    allowNavigation();
  }, [searchParams]); // eslint-disable-line react-hooks/exhaustive-deps

  return (
    <Stack gap={3}>
      <ControlPanel />
      <Tabs activeKey={displayedTab} onSelect={(key) => setCurrentTab(key)}>
        <Tab title="Certificates" eventKey={certificateTab}>
          <CertificatesList />
        </Tab>
        {enableCardanoDbTab && (
          <Tab title="Cardano Db" eventKey={signedEntityType.CardanoDb}>
            <CardanoDbSnapshotsList />
          </Tab>
        )}
        {enableCardanoTransactionTab && (
          <Tab title="Cardano Transactions" eventKey={signedEntityType.CardanoTransactions}>
            <CardanoTransactionsSnapshotsList />
          </Tab>
        )}
        {enableCardanoBlocksTransactionTab && (
          <Tab
            title="Cardano Blocks & Transactions"
            eventKey={signedEntityType.CardanoBlocksTransactions}>
            <CardanoBlocksTransactionsSnapshotsList />
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
      </Tabs>
    </Stack>
  );
}
