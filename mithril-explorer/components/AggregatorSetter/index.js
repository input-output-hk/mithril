import React, {useEffect, useState} from 'react';
import {Button, Col, Form, InputGroup, OverlayTrigger, Tooltip} from "react-bootstrap";
import AddAggregatorModal from "./AddAggregatorModal";

export default function AggregatorSetter(props) {
  const [availableAggregators, setAvailableAggregators] = useState([]);
  const [showAddModal, toggleAddModal] = useState(false);
  const [canRemoveSelected, setCanRemoveSelected] = useState(false);

  useEffect(() => {
    let aggregators = props.defaultAvailableAggregators;

    setAvailableAggregators(aggregators);
  }, [props.defaultAvailableAggregators]);

  useEffect(() => {
    setCanRemoveSelected(!props.defaultAvailableAggregators.includes(props.aggregator));
  }, [props.aggregator]);

  function handleChange(aggregator) {
    props.onAggregatorChange(aggregator);
  }

  function copySelected() {
    if (window.isSecureContext && props.aggregator) {
      navigator.clipboard.writeText(props.aggregator).then(() => {});
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

    setAvailableAggregators([...availableAggregators, aggregator]);
    handleChange(aggregator);
  }

  function deleteSelectedAggregatorSource() {
    if (!canRemoveSelected) {
      return;
    }

    setAvailableAggregators(availableAggregators.filter(a => a !== props.aggregator));
    handleChange(availableAggregators.at(0));
  }

  return (
    <>
      <AddAggregatorModal
        show={showAddModal}
        onAdd={addAggregatorSource}
        onAskClose={hideAddAggregatorSourceModal}/>

      <Form.Group as={Col} className={props.className}>
        <Form.Label>Aggregator:</Form.Label>
        <InputGroup>
          <Button variant="outline-success" onClick={showAddAggregatorSourceModal}>
            <i className="bi bi-plus-circle"></i>
          </Button>
          {canRemoveSelected &&
            <Button variant="outline-danger" onClick={deleteSelectedAggregatorSource}>
              <i className="bi bi-dash-circle"></i>
            </Button>
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
