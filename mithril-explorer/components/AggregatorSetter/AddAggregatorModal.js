import React, {useState} from 'react';
import {Button, Form, FormGroup, Modal} from "react-bootstrap";
import {useDispatch} from "react-redux";
import {selectAggregator} from "../../store/settingsSlice";
import {checkUrl} from "../../utils";

export default function AddAggregatorModal(props) {
  const [value, setValue] = useState("");
  const [isInvalid, setIsInvalid] = useState(false);
  const dispatch = useDispatch();

  function handleClose() {
    props.onAskClose();
    setIsInvalid(false);
    setValue("");
  }

  function handleSave(event) {
    // Avoid form submit if the enter key is pressed
    event.preventDefault();

    if (checkUrl(value)) {
      handleClose();
      dispatch(selectAggregator(value));
    } else {
      setIsInvalid(true);
    }
  }

  return (
    <Modal
      show={props.show}
      onHide={handleClose}
      size="lg"
      aria-labelledby="add-aggregator-title"
      centered>
      <Modal.Header closeButton>
        <Modal.Title id="add-aggregator-title">
          New aggregator source
        </Modal.Title>
      </Modal.Header>

      <Modal.Body>
        <Form onSubmit={handleSave}>
          <FormGroup>
            <Form.Label>URL</Form.Label>
            <Form.Control
              type="url"
              value={value}
              onChange={e => setValue(e.target.value)}
              isInvalid={isInvalid}
              autoFocus/>
            <Form.Control.Feedback type="invalid">
              Invalid URL
            </Form.Control.Feedback>
          </FormGroup>
        </Form>
      </Modal.Body>

      <Modal.Footer>
        <Button variant="secondary" onClick={handleClose}>
          Close
        </Button>
        <Button variant="primary" onClick={handleSave}>
          Save
        </Button>
      </Modal.Footer>
    </Modal>
  );
}
