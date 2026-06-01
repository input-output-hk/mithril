import React from "react";
import { useDispatch, useSelector } from "react-redux";
import { Button, Col, Form, InputGroup, OverlayTrigger, Tooltip } from "react-bootstrap";
import { removeSelectedAggregator, selectAggregator } from "@/store/settingsSlice";

export default function AggregatorSetter({ onAddAggregatorClick, ...props }) {
  const selectedAggregator = useSelector((state) => state.settings.selectedAggregator);
  const availableAggregators = useSelector((state) => state.settings.availableAggregators);
  const canRemoveSelected = useSelector((state) => state.settings.canRemoveSelected);
  const dispatch = useDispatch();

  function copySelected() {
    if (window.isSecureContext && selectedAggregator?.url) {
      navigator.clipboard.writeText(selectedAggregator.url).then(() => {});
    }
  }

  function handleSelectAggregator(aggregatorUrl) {
    const aggregator = availableAggregators.find((a) => a.url === aggregatorUrl);
    if (aggregator) {
      dispatch(selectAggregator(aggregator));
    }
  }

  return (
    <Form.Group as={Col} {...props}>
      <InputGroup>
        <Button variant="outline-success" type="button" onClick={onAddAggregatorClick}>
          <i className="bi bi-plus-circle"></i>
        </Button>
        {canRemoveSelected && (
          <>
            <Button
              variant="outline-danger"
              type="button"
              onClick={() => dispatch(removeSelectedAggregator())}>
              <i className="bi bi-dash-circle"></i>
            </Button>
            <OverlayTrigger overlay={<Tooltip>Unofficial Aggregator</Tooltip>}>
              <Button variant="outline-warning" type="button">
                <i className="bi bi-exclamation-triangle"></i>
              </Button>
            </OverlayTrigger>
          </>
        )}
        <Form.Select
          value={selectedAggregator.url}
          onChange={(e) => handleSelectAggregator(e.target.value)}>
          {availableAggregators.map((aggregator, index) => (
            <option key={`aggr-${index}`} value={aggregator.url}>
              {aggregator.url}
            </option>
          ))}
        </Form.Select>
        <OverlayTrigger overlay={<Tooltip>Copy url</Tooltip>}>
          <Button variant="outline-secondary" type="button" onClick={copySelected}>
            <i className="bi bi-clipboard"></i>
          </Button>
        </OverlayTrigger>
      </InputGroup>
    </Form.Group>
  );
}
