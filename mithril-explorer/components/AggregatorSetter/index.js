import React, { useState, useEffect } from 'react';
import { Button, InputGroup, Form, Col, OverlayTrigger, Tooltip } from "react-bootstrap";

export default function AggregatorSetter(props) {
  const [availableAggregators, setAvailableAggregators] = useState([]);
  const [selectedAggregator, setSelectedAggregator] = useState();

  useEffect(() => {
    let aggregators = [];
    aggregators.push(...props.defaultAvailableAggregators);
    setAvailableAggregators(aggregators);
    
    setSelectedAggregator(aggregators.at(0))
  }, [props.defaultAvailableAggregators]);

  function handleChange(event) {
    props.onAggregatorChange(event.target.value);
    setSelectedAggregator(event.target.value);
  }
  
  function copySelected() {
    if (window.isSecureContext && selectedAggregator) {
      navigator.clipboard.writeText(selectedAggregator);
    }
  }
  
  return (
    <Form.Group as={Col} className={props.className}>
      <Form.Label>Aggregator:</Form.Label>
      <InputGroup>
          <Form.Select value={props.aggregator} onChange={handleChange}>
            {availableAggregators.map((aggregator, index) =>
              <option key={"agg-" + index} value={aggregator}>{aggregator}</option>
            )}
          </Form.Select>
        <OverlayTrigger overlay={<Tooltip>Copy url</Tooltip>} >
          <Button variant="outline-secondary" onClick={copySelected}>
            <i className="bi bi-clipboard"></i>
          </Button>
        </OverlayTrigger>
      </InputGroup>
    </Form.Group>
  )
}
