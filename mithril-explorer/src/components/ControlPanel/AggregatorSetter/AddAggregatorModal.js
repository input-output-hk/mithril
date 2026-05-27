import React, { useState } from "react";
import { useDispatch } from "react-redux";
import { Button, Form, Modal } from "react-bootstrap";
import { selectAggregator } from "@/store/settingsSlice";
import { checkUrl } from "@/utils";

export default function AddAggregatorModal(props) {
  const [url, setUrl] = useState("");
  const [genesisVerificationKey, setGenesisVerificationKey] = useState("");
  const [isInvalid, setIsInvalid] = useState(false);
  const dispatch = useDispatch();

  function handleUrlChange(event) {
    setUrl(event.target.value);
  }

  function handleClose() {
    props.onAskClose();
    setIsInvalid(false);
    setUrl("");
    setGenesisVerificationKey("");
  }

  function handleSave(event) {
    // Avoid form submit if the enter key is pressed
    event.preventDefault();

    const form = event.target;

    if (form.checkValidity() === true && checkUrl(url)) {
      handleClose();
      dispatch(selectAggregator({ url: url, genesisVerificationKey: genesisVerificationKey }));
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
        <Modal.Title id="add-aggregator-title">New aggregator source</Modal.Title>
      </Modal.Header>

      <Modal.Body>
        <Form onSubmit={handleSave}>
          <Form.Group>
            <Form.Label>URL</Form.Label>
            <Form.Control
              name="aggregatorUrl"
              type="url"
              value={url}
              onChange={handleUrlChange}
              isInvalid={isInvalid}
              required
              autoFocus
            />
            <Form.Control.Feedback type="invalid">Invalid URL</Form.Control.Feedback>
          </Form.Group>

          <Form.Group>
            <Form.Label>Genesis verification key</Form.Label>
            <Form.Control
              name="genesisVerificationKey"
              type="text"
              value={genesisVerificationKey}
              onChange={(e) => setGenesisVerificationKey(e.target.value)}
            />
            <Form.Text className="text-muted">
              Optional, allows verification of the certificate chain and of Cardano artifacts (e.g.
              transactions).
            </Form.Text>
          </Form.Group>
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
