import React, { useState, useEffect } from 'react';

export default function SnapshotsList(props) {
  const [snapshots, setSnapshots] = useState([]);

  useEffect(() => {
    fetch(`${props.apiPath}/snapshots`)
      .then(response => response.json())
      .then(data => setSnapshots(data))
      .catch(error => {
        console.error(error);
      });
  });

  return (
    <div className="margin--lg">
      <h2>Snapshots</h2>
      {Object.entries(snapshots).length === 0
        ? <p>No snapshot available</p>
        :
        <div className="row">
          {snapshots.map((snapshot, index) =>
            <div key={snapshot.digest} className="card margin-right--md">
              <div className="card__header">
                <h3>
                  Snapshot {snapshot.digest.slice(0, 12)}
                  { index === 0 && <span className="badge badge--primary margin-left--sm">Latest</span> }
                </h3>
              </div>
              <div className="card__body">
                <div>Certificate hash: <br/> {snapshot.certificate_hash.slice(0, 30)}</div>
                <div>Created at: <br/> {new Date(snapshot.created_at).toLocaleString()}</div>
                <div>Size:{snapshot.size}</div>
              </div>
            </div>
          )}
        </div>
      }
    </div>
  );
}
