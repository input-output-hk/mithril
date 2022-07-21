import React, { useState, useEffect } from 'react';
import PendingCertificate from '@site/src/components/Showcase/PendingCertificate';
import SnapshotsList from '@site/src/components/Showcase/SnapshotsList';
import Layout from '@theme/Layout';

const available_aggregators = [
  "http://localhost:8080/aggregator",
  "http://aggregator.api.mithril.network/aggregator"
];

function IntervalSetter(props) {
  function handleChange(event) {
    props.onIntervalChange(parseInt(event.target.value));
  }
  
  return (
    <label className={props.className}>
      Update Interval:
      <select value={props.interval} onChange={handleChange} className="margin-left--sm">
        <option value={0}>Stop</option>
        <option value={1000}>1 seconds</option>
        <option value={5000}>5 seconds</option>
        <option value={10000}>10 seconds</option>
      </select>
    </label>
  );
}

function AggregatorSetter(props) {
  function handleChange(event) {
    props.onAggregatorChange(event.target.value);
  }

  return (
    <label className={props.className}>
      Aggregator:
      <select value={props.aggregator} onChange={handleChange} className="margin-left--sm">
        {props.availableAggregators.map((aggregator, index) =>
          <option key={"agg-" + index} value={aggregator}>{aggregator}</option>
        )}
      </select>
    </label>
  );
}

export default function Showcase() {
  const [aggregator, setAggregator] = useState(available_aggregators[0]);
  const [interval, setInterval] = useState(10000);

  function handleApiChange(api) {
    setAggregator(api);
  }

  function handleIntervalChange(interval) {
    setInterval(interval);
  }
  
  return (
    <Layout
      title="Mithril Showcase"
      description="Showcase of a Mithril Network">
      <main className="padding--md">
        <h1>Mithril Showcase</h1>
        <form className="margin--md">
          <AggregatorSetter aggregator={aggregator} onAggregatorChange={handleApiChange} availableAggregators={available_aggregators}  />
          <IntervalSetter interval={interval} onIntervalChange={handleIntervalChange} className="margin-left--sm" />
        </form>
        <PendingCertificate aggregator={aggregator} updateInterval={interval} className="margin--md" />
        <SnapshotsList aggregator={aggregator} updateInterval={interval} className="margin--md" />
      </main>
    </Layout>
  );
}
