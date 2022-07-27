import React, { useState, useEffect } from 'react';
import styles from './styles.module.css';

/*
 * Code from: https://stackoverflow.com/a/18650828
 */
function formatBytes(bytes, decimals = 2) {
  if (bytes === 0) return '0 Bytes';

  const k = 1024;
  const dm = decimals < 0 ? 0 : decimals;
  const sizes = ['Bytes', 'KB', 'MB', 'GB', 'TB', 'PB', 'EB', 'ZB', 'YB'];

  const i = Math.floor(Math.log(bytes) / Math.log(k));

  return parseFloat((bytes / Math.pow(k, i)).toFixed(dm)) + ' ' + sizes[i];
}

export default function SnapshotsList(props) {
  const [snapshots, setSnapshots] = useState([]);

  useEffect(() => {
    if (!props.autoUpdate) {
      return;
    }
    
    let fetchSnapshots = () => {
      fetch(`${props.aggregator}/snapshots`)
        .then(response => response.json())
        .then(data => {
          let grouped_snapshots = [];
          for (let i = 0; i < data.length; i++) {
            // 4 snapshots per rows, if you modify this also update the "col--xx" class below
            if (i % 4 === 0) {
              grouped_snapshots.push([]);
            }
            let snapshot = data[i];
            snapshot.is_latest = i === 0;
            grouped_snapshots[grouped_snapshots.length - 1].push(snapshot);
          }
          
          setSnapshots(grouped_snapshots);
        })
        .catch(error => {
          setSnapshots([]);
          console.error("Fetch snapshots error:", error);
        });
    };
    
    // Fetch them once without waiting
    fetchSnapshots(); 
    
    const interval = setInterval(fetchSnapshots, props.updateInterval);
    return () => clearInterval(interval);
  }, [props.aggregator, props.updateInterval, props.autoUpdate]);

  return (
    <div className={props.className}>
      <h2>Snapshots</h2>
      {Object.entries(snapshots).length === 0
        ? <p>No snapshot available</p>
        :
        <div className={"container " + styles.snapshots_container}>
          {snapshots.map((snapshot_row, row_index) =>
            <div className="row" key={row_index}>
              {snapshot_row.map(snapshot =>
                <div key={snapshot.digest} className="padding-right--sm padding-bottom--sm col col--3">
                  <div className="card">
                    <div className="card__header">
                      <h3>
                        {snapshot.digest}
                      </h3>
                    </div>
                    <div className="card__body">
                      <div>Epoch: {snapshot.beacon.epoch}</div>
                      <div>Immutable File Number: {snapshot.beacon.immutable_file_number}</div>
                      <div>Certificate hash: <br/> {snapshot.certificate_hash}</div>
                      <div>Created at: <br/> {new Date(snapshot.created_at).toLocaleString()}</div>
                      <div>Size: {formatBytes(snapshot.size)}</div>
                    </div>
                    <div className="card__footer text--right">
                      {snapshot.is_latest &&
                        <span className="badge badge--primary margin-left--sm">Latest</span>
                      }
                      <span className="badge badge--secondary margin-left--sm">{snapshot.beacon.network}</span>
                    </div>
                  </div>
                </div>
              )}
            </div>
          )}
        </div>
      }
    </div>
  );
}
