import React, {useEffect, useReducer, useState} from 'react';
import { Button, Col, Form, InputGroup, OverlayTrigger, Tooltip } from "react-bootstrap";
import AddAggregatorModal from "./AddAggregatorModal";
import { initAggregatorList, aggregatorListReducer } from "./reducer";

export default function AggregatorSetter(props) {
  const [showAddModal, toggleAddModal] = useState(false);
  const [state, dispatch] =
    useReducer(
      aggregatorListReducer,
      [props.aggregator, props.defaultAvailableAggregators],
      initAggregatorList);
  
  useEffect(() => {
    const aggregator = props.aggregator;
    dispatch({type: 'addAggregator', aggregator: aggregator});
    props.onAggregatorChange(aggregator);
  }, [props.aggregator]);

  function copySelected() {
    if (window.isSecureContext && props.aggregator) {
      navigator.clipboard.writeText(props.aggregator).then(() => { });
    }
  }

  function handleChange(aggregator) {
    dispatch({type: 'aggregatorSelected', aggregator: aggregator});
    props.onAggregatorChange(aggregator);
  }

  function addAggregatorSource(aggregator) {
    props.onAggregatorChange(aggregator);
  }

  function deleteSelectedAggregatorSource() {
    dispatch({type: 'deleteSelected', aggregator: props.aggregator});
    props.onAggregatorChange(state.availableAggregators.at(0));
  }

  return (
    <>
      <AddAggregatorModal
        show={showAddModal}
        onAdd={addAggregatorSource}
        onAskClose={() => toggleAddModal(false)} />

      <Form.Group as={Col} className={props.className}>
        <Form.Label>Aggregator:</Form.Label>
        <InputGroup>
          <Button variant="outline-success" onClick={() => toggleAddModal(true)}>
            <i className="bi bi-plus-circle"></i>
          </Button>
          {state.canRemoveSelected &&
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
            {state.availableAggregators.map((aggregator, index) =>
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
