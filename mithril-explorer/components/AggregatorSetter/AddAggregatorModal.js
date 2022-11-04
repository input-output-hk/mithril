import React, {useState} from 'react';
import {Button, Form, FormGroup, Modal} from "react-bootstrap";

export default function AddAggregatorModal(props) {
  const [value, setValue] = useState("");
  const [isInvalid, setIsInvalid] = useState(false);

  function handleClose() {
    props.onAskClose();
    setIsInvalid(false);
    setValue("");
  }

  function handleSave(event) {
    // Avoid form submit if the enter key is pressed
    event.preventDefault();

    try {
      // Use the url constructor to check if the value is an url
      new URL(value);
      props.onAdd(value);
      handleClose();
    } catch (ex) {
      console.warn("invalid url", ex);
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
