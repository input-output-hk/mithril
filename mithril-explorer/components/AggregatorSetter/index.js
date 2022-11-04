import React, { useEffect, useState } from 'react';
import { Button, Col, Form, InputGroup, OverlayTrigger, Tooltip } from "react-bootstrap";
import AddAggregatorModal from "./AddAggregatorModal";

export default function AggregatorSetter(props) {
  const CUSTOM_AGGREGATORS_KEY = "CUSTOM_AGGREGATORS";
  const [availableAggregators, setAvailableAggregators] = useState([]);
  const [showAddModal, toggleAddModal] = useState(false);
  const [canRemoveSelected, setCanRemoveSelected] = useState(false);

  useEffect(() => {
    setAvailableAggregators(getAggregatorsList());
  }, [props.defaultAvailableAggregators]);

  useEffect(() => {
    setCanRemoveSelected(!props.defaultAvailableAggregators.includes(props.aggregator));

    let aggregators = getAggregatorsList();
    if (!aggregators.includes(props.aggregator)) {
      aggregators.push(props.aggregator);
      saveCustomAggregatorSources(aggregators);
      setAvailableAggregators(aggregators);
    }
  }, [props.aggregator]);

  function getAggregatorsList() {
    let aggregators = [...props.defaultAvailableAggregators];

    const storedAggregators = JSON.parse(localStorage.getItem(CUSTOM_AGGREGATORS_KEY));
    if (storedAggregators) {
      aggregators = aggregators.concat(storedAggregators);
    }

    return aggregators;
  }

  function handleChange(aggregator) {
    props.onAggregatorChange(aggregator);
  }

  function copySelected() {
    if (window.isSecureContext && props.aggregator) {
      navigator.clipboard.writeText(props.aggregator).then(() => { });
    }
  }

  function showAddAggregatorSourceModal() {
    toggleAddModal(true);
  }

  function hideAddAggregatorSourceModal() {
    toggleAddModal(false);
  }

  function addAggregatorSource(aggregator) {
    if (availableAggregators.includes(aggregator)) {
      return;
    }

    const aggregators = [...availableAggregators, aggregator];
    setAvailableAggregators(aggregators);
    handleChange(aggregator);
    saveCustomAggregatorSources(aggregators);
  }

  function deleteSelectedAggregatorSource() {
    if (!canRemoveSelected) {
      return;
    }

    const aggregators = availableAggregators.filter(a => a !== props.aggregator);
    setAvailableAggregators(aggregators);
    handleChange(availableAggregators.at(0));
    saveCustomAggregatorSources(aggregators);
  }

  function saveCustomAggregatorSources(aggregators) {
    const customAggregators = aggregators.filter(a => !props.defaultAvailableAggregators.includes(a));
    localStorage.setItem(CUSTOM_AGGREGATORS_KEY, JSON.stringify(customAggregators));
  }

  return (
    <>
      <AddAggregatorModal
        show={showAddModal}
        onAdd={addAggregatorSource}
        onAskClose={hideAddAggregatorSourceModal} />

      <Form.Group as={Col} className={props.className}>
        <Form.Label>Aggregator:</Form.Label>
        <InputGroup>
          <Button variant="outline-success" onClick={showAddAggregatorSourceModal}>
            <i className="bi bi-plus-circle"></i>
          </Button>
          {canRemoveSelected &&
            <>
              <Button variant="outline-danger" onClick={deleteSelectedAggregatorSource}>
                <i className="bi bi-dash-circle"></i>
              </Button>
              <OverlayTrigger overlay={<Tooltip>Unofficial Aggregator</Tooltip>}>
                <Button variant="outline-warning" onClick={copySelected}>
                  <i className="bi bi-exclamation-triangle"></i>
                </Button>
              </OverlayTrigger>
            </>
          }
          <Form.Select value={props.aggregator} onChange={e => handleChange(e.target.value)}>
            {availableAggregators.map((aggregator, index) =>
              <option key={"agg-" + index} value={aggregator}>{aggregator}</option>
            )}
          </Form.Select>
          <OverlayTrigger overlay={<Tooltip>Copy url</Tooltip>}>
            <Button variant="outline-secondary" onClick={copySelected}>
              <i className="bi bi-clipboard"></i>
            </Button>
          </OverlayTrigger>
        </InputGroup>
      </Form.Group>
    </>
  );
}
