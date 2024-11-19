import React, { useState } from "react";
import { Form, OverlayTrigger, Row, Stack, Tooltip } from "react-bootstrap";
import AggregatorSetter from "#/ControlPanel/AggregatorSetter";
import AggregatorStatus from "#/ControlPanel/AggregatorStatus";
import IntervalSetter from "#/ControlPanel/IntervalSetter";

export default function ControlPanel() {
  const [showContent, setShowContent] = useState(true);

  return (
    <Stack gap={2}>
      <h3>
        <a
          role="button"
          onClick={() => setShowContent(!showContent)}
          aria-expanded={showContent}
          aria-controls="contentRow">
          Aggregator{" "}
          <OverlayTrigger overlay={<Tooltip>Show/hide Aggregator status</Tooltip>}>
            <i className={`bi bi-chevron-${showContent ? "up" : "down"}`}></i>
          </OverlayTrigger>
        </a>
      </h3>

      <Form>
        <Row xs={1} sm={2} className="row-gap-2">
          <AggregatorSetter sm={8} />
          <IntervalSetter sm={4} />
        </Row>
      </Form>

      <AggregatorStatus showContent={showContent} />
    </Stack>
  );
}
