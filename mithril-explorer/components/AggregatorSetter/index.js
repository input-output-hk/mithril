import React, {useState} from 'react';
import {Button, Col, Form, InputGroup, OverlayTrigger, Tooltip} from "react-bootstrap";
import {useDispatch, useSelector} from "react-redux";
import {removeCustomAggregator, selectAggregator} from "../../store/settingsSlice";
import AddAggregatorModal from "./AddAggregatorModal";

export default function AggregatorSetter(props) {
  const [showAddModal, toggleAddModal] = useState(false);
  const selectedAggregator = useSelector((state) => state.settings.selectedAggregator);
  const availableAggregators = useSelector((state) => state.settings.availableAggregators);
  const canRemoveSelected = useSelector((state) => state.settings.canRemoveSelected);
  const dispatch = useDispatch();

  function copySelected() {
    if (window.isSecureContext && selectedAggregator) {
      navigator.clipboard.writeText(selectedAggregator).then(() => {
      });
    }
  }

  return (
    <>
      <AddAggregatorModal
        show={showAddModal}
        onAskClose={() => toggleAddModal(false)}/>

      <Form.Group as={Col} className={props.className}>
        <Form.Label>Aggregator:</Form.Label>
        <InputGroup>
          <Button variant="outline-success" onClick={() => toggleAddModal(true)}>
            <i className="bi bi-plus-circle"></i>
          </Button>
          {canRemoveSelected &&
            <>
              <Button variant="outline-danger" onClick={() => dispatch(removeCustomAggregator())}>
                <i className="bi bi-dash-circle"></i>
              </Button>
              <OverlayTrigger overlay={<Tooltip>Unofficial Aggregator</Tooltip>}>
                <Button variant="outline-warning">
                  <i className="bi bi-exclamation-triangle"></i>
                </Button>
              </OverlayTrigger>
            </>
          }
          <Form.Select value={selectedAggregator} onChange={(e) => dispatch(selectAggregator(e.target.value))}>
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
