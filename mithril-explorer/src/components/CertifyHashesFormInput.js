import { Button, Form, InputGroup } from "react-bootstrap";
import React, { useState } from "react";

export default function CertifyHashesFormInput({
  submitButtonLabel = "Run",
  maxAllowedHashesByRequest,
  certifiedMessageType,
}) {
  const maxHashesAllowed = Math.max(maxAllowedHashesByRequest, 1);
  const validationPattern = ` *(\\w+ *, *){0,${maxHashesAllowed - 1}}\\w+,? *`;
  const [hashesByType, setHashesByType] = useState({
    block: "",
    transaction: "",
  });

  return (
    <Form.Group>
      <InputGroup hasValidation>
        <Button type="submit">{submitButtonLabel}</Button>
        <Form.Control
          id={`${certifiedMessageType.name}Hashes`}
          name={`${certifiedMessageType.name}Hashes`}
          type="text"
          placeholder={`comma-separated list of ${certifiedMessageType.pluralName} hashes`}
          required
          pattern={validationPattern}
          value={hashesByType[certifiedMessageType.name] ?? ""}
          onChange={(e) =>
            setHashesByType((prev) => ({ ...prev, [certifiedMessageType.name]: e.target.value }))
          }
        />
        <Form.Control.Feedback type="invalid">
          Please provide a comma-separated list of {certifiedMessageType.pluralName} hashes.
        </Form.Control.Feedback>
      </InputGroup>
    </Form.Group>
  );
}
