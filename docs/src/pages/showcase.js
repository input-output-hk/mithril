import React, { useState, useEffect } from 'react';
import PendingCertificate from '@site/src/components/Showcase/PendingCertificate';
import SnapshotsList from '@site/src/components/Showcase/SnapshotsList';
import Layout from '@theme/Layout';

const api_path = "http://localhost:8080/aggregator";
// const api_path = "http://aggregator.api.mithril.network/aggregator";

function IntervalSetter(props) {
  function handleChange(event) {
    props.onIntervalChange(parseInt(event.target.value));
  }
  
  return (
    <form>
      <label>
        Update Interval:
        <select value={props.interval} onChange={handleChange}>
          <option value={0}>Stop</option>
          <option value={1000}>1 seconds</option>
          <option value={5000}>5 seconds</option>
          <option value={10000}>10 seconds</option>
        </select>
      </label>
    </form>
  );
}

export default function Showcase() {
  const [interval, setInterval] = useState(10000);
  
  function handleIntervalChange(interval) {
    setInterval(interval);
  }
  
  return (
    <Layout
      title="Mithril Showcase"
      description="Showcase of a Mithril Network">
      <main className="padding--md">
        <h1>Mithril Showcase</h1>
        <div className="margin--md">
          <IntervalSetter interval={interval} onIntervalChange={handleIntervalChange} />
        </div>
        <PendingCertificate apiPath={api_path} updateInterval={interval} className="margin--md" />
        <SnapshotsList apiPath={api_path} updateInterval={interval} className="margin--md" />
      </main>
    </Layout>
  );
}
