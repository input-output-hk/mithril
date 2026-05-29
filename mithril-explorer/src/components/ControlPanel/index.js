import React, { useState } from "react";
import { OverlayTrigger, Row, Stack, Tooltip } from "react-bootstrap";
import AggregatorSetter from "#/ControlPanel/AggregatorSetter";
import AggregatorStatus from "#/ControlPanel/AggregatorStatus";
import IntervalSetter from "#/ControlPanel/IntervalSetter";
import AddAggregatorModal from "#/ControlPanel/AggregatorSetter/AddAggregatorModal";

export default function ControlPanel() {
  const [showContent, setShowContent] = useState(true);
  const [showAddModal, toggleAddModal] = useState(false);

  return (
    <Stack gap={2}>
      <AddAggregatorModal show={showAddModal} onAskClose={() => toggleAddModal(false)} />

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

      <Row xs={1} sm={2} className="row-gap-2">
        <AggregatorSetter sm={8} onAddAggregatorClick={() => toggleAddModal(true)} />
        <IntervalSetter sm={4} />
      </Row>

      <AggregatorStatus showContent={showContent} />
    </Stack>
  );
}
