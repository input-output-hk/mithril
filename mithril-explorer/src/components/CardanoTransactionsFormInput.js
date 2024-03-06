import { Button, Form, FormGroup, InputGroup } from "react-bootstrap";
import React from "react";

export default function CardanoTransactionsFormInput() {
  return (
    <FormGroup>
      <InputGroup hasValidation>
        <Button type="submit">Certify transactions</Button>
        <Form.Control
          name="txHashes"
          type="text"
          placeholder="1cfbee5ed59c, 3bf71cd66d48, b16b6e006b1d, ..."
          required
          pattern=" *(\w+ *, *)*\w+,? *"
        />
        <Form.Control.Feedback type="invalid">
          Please provide a comma-separated list of transactions hashes.
        </Form.Control.Feedback>
      </InputGroup>
    </FormGroup>
  );
}
