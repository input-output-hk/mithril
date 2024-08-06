import { Button, Form, FormGroup, InputGroup } from "react-bootstrap";
import React from "react";

export default function CardanoTransactionsFormInput({ maxAllowedHashesByRequest }) {
  const maxHashesAllowed = Math.max(maxAllowedHashesByRequest, 1);
  const validationPattern = ` *(\\w+ *, *){0,${maxHashesAllowed - 1}}\\w+,? *`;

  return (
    <FormGroup>
      <InputGroup hasValidation>
        <Button type="submit">Certify transactions</Button>
        <Form.Control
          name="txHashes"
          type="text"
          placeholder="comma-separated list of transactions hashes"
          required
          pattern={validationPattern}
        />
        <Form.Control.Feedback type="invalid">
          Please provide a comma-separated list of transactions hashes.
        </Form.Control.Feedback>
      </InputGroup>
    </FormGroup>
  );
}
