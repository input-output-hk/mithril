import React, { useState, useEffect } from 'react';
import PendingCertificate from '@site/src/components/Showcase/PendingCertificate';
import SnapshotsList from '@site/src/components/Showcase/SnapshotsList';
import Layout from '@theme/Layout';

const api_path = "http://localhost:8080/aggregator";
// const api_path = "http://aggregator.api.mithril.network/aggregator";

export default function Showcase() {
  return (
    <Layout
      title="Mithril Showcase"
      description="Showcase of a Mithril Network">
      <main>
        <PendingCertificate apiPath={api_path} updateInterval="10000" />
        <SnapshotsList apiPath={api_path} updateInterval="10000" />
      </main>
    </Layout>
  );
}
