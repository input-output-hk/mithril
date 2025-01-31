import React from "react";
import { Col } from "react-bootstrap";

import styles from "./styles.module.css";

export default function ArtifactCol({ label, children, className = "", ...props }) {
  return (
    <Col
      className={`mx-2 px-0 py-1 d-flex flex-column justify-content-between ${className}`}
      {...props}>
      <div>
        <div className={styles.artifactColLabel}>
          <em>{label}:</em>
        </div>
        <div className={`test-end`}>{children}</div>
      </div>
      <hr className="my-1" />
    </Col>
  );
}
