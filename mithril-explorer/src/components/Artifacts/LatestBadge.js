import { Badge } from "react-bootstrap";
import React from "react";

export default function LatestBadge({ show }) {
  if (show === true) {
    return (
      <>
        <Badge bg="primary">Latest</Badge>{" "}
      </>
    );
  } else {
    return <></>;
  }
}
