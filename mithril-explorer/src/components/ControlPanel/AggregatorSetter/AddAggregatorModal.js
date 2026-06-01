import React, { useState } from "react";
import { useDispatch } from "react-redux";
import { Button, Form, Modal } from "react-bootstrap";
import { selectAggregator } from "@/store/settingsSlice";
import { checkUrl } from "@/utils";

export default function AddAggregatorModal({ show, onAskClose }) {
  const [isInvalid, setIsInvalid] = useState(false);
  const dispatch = useDispatch();

  function handleClose() {
    onAskClose();
    setIsInvalid(false);
  }

  function handleSave(event) {
    // Prevent browser navigation/page reload.
    event.preventDefault();

    const form = event.currentTarget;
    const formData = new FormData(form);
    const formJson = Object.fromEntries(formData.entries());

    if (form.checkValidity() && checkUrl(formJson?.aggregatorUrl)) {
      dispatch(
        selectAggregator({
          url: formJson.aggregatorUrl,
          genesisVerificationKey: formJson?.genesisVerificationKey ?? "",
        }),
      );
      handleClose();
    } else {
      setIsInvalid(true);
    }
  }

  return (
    <Modal
      show={show}
      onHide={handleClose}
      size="lg"
      aria-labelledby="add-aggregator-title"
      centered>
      <Modal.Header closeButton>
        <Modal.Title id="add-aggregator-title">New aggregator source</Modal.Title>
      </Modal.Header>

      <Modal.Body>
        <Form id="add-aggregator-form" onSubmit={handleSave} validated={isInvalid} noValidate>
          <Form.Group>
            <Form.Label>URL</Form.Label>
            <Form.Control name="aggregatorUrl" type="url" required autoFocus />
            <Form.Control.Feedback type="invalid">Invalid URL</Form.Control.Feedback>
          </Form.Group>

          <Form.Group>
            <Form.Label>Genesis verification key</Form.Label>
            <Form.Control name="genesisVerificationKey" type="text" />
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
        <Button variant="primary" type="submit" form="add-aggregator-form">
          Save
        </Button>
      </Modal.Footer>
    </Modal>
  );
}
