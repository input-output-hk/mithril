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

function StartStopButton(props) {
  function handleClick() {
    props.onPress(!props.isPressed);
  }
  
  let buttonClasses =
    "margin-left--sm button "
    + props.isPressed ? "button--primary" : "button--success";

  return (
    <label className={props.className}>
      <button
        type="button"
        onClick={handleClick}
        className={"margin-left--sm button button--sm ".concat(props.isPressed ? "button--primary" : "button--success")}
      >
        {props.isPressed ? "Pause ⏸" : "Resume ▶"}
      </button>
    </label>
  );
}

export default function Showcase() {
  const [aggregator, setAggregator] = useState(available_aggregators[0]);
  const [interval, setInterval] = useState(10000);
  const [autoUpdate, setAutoUpdate] = useState(true);

  function handleApiChange(api) {
    setAggregator(api);
  }
  
  function handleStartStopButtonPress(isPressed) {
    setAutoUpdate(isPressed);
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
          <StartStopButton isPressed={autoUpdate} onPress={handleStartStopButtonPress} className="margin-left--sm" />
        </form>
        <PendingCertificate aggregator={aggregator} updateInterval={interval} autoUpdate={autoUpdate} className="margin--md" />
        <SnapshotsList aggregator={aggregator} updateInterval={interval} autoUpdate={autoUpdate} className="margin--md" />
      </main>
    </Layout>
  );
}
