import React, { useState, useEffect } from 'react';

export default function PendingCertificate(props) {
  const [pendingCertificate, setPendingCertificate] = useState({});

  useEffect(() => {
    if (!props.updateInterval) {
      return;
    }
    
    let fetchPendingCertificate = () => {
      fetch(`${props.aggregator}/certificate-pending`)
        .then(response => response.status === 200 ? response.json() : {})
        .then(data => setPendingCertificate(data))
        .catch(error => {
          console.error("Fetch certificate-pending error:", error);
        });
    };
    
    // Fetch it once without waiting
    fetchPendingCertificate();
    
    const interval = setInterval(fetchPendingCertificate, props.updateInterval);
    return () => clearInterval(interval);
  }, [props.aggregator, props.updateInterval]);

  return (
    <div className={props.className}>
      <h2>Pending Certificate</h2>
      
      {Object.entries(pendingCertificate).length === 0
        ? <p>No pending certificate available</p>
        :
        <div className="row">
          <div className="card margin-right--md shadow--md">
            <div className="card__header">
              <h3>Beacon</h3>
            </div>
            <div className="card__body">
              <div>Network: {pendingCertificate.beacon.network}</div>
              <div>Epoch: {pendingCertificate.beacon.epoch}</div>
              <div>Immutable File Number: {pendingCertificate.beacon.immutable_file_number}</div>
            </div>
          </div>
          <div className="card margin-right--md shadow--md">
            <div className="card__header">
              <h3>Protocol Parameters</h3>
            </div>
            <div className="card__body">
              <div>K: {pendingCertificate.protocol.k}</div>
              <div>M: {pendingCertificate.protocol.m}</div>
              <div>Phi: {pendingCertificate.protocol.phi_f}</div>
            </div>
          </div>
          <div className="card margin-right--md shadow--md">
            <div className="card__header">
              <h3>Signers</h3>
            </div>
            <div className="card__body">
              {pendingCertificate.signers.length === 0
                ? <div>No Signers registered</div>
                : pendingCertificate.signers.map(signer =>
                  <div key={signer.party_id}>Party id: {signer.party_id}</div>
                )
              }
            </div>
          </div>
        </div>
      }
    </div>
  );
}
