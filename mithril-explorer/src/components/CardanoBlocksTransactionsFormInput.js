import { Button, Form, FormGroup, InputGroup, ToggleButton } from "react-bootstrap";
import React, { useState } from "react";
import { certifiedMessageTypes } from "#/CertifyCardanoBlocksOrTransactionsModal";

export default function CardanoBlocksTransactionsFormInput({
  maxAllowedHashesByRequest,
  certifiedMessageType,
  onCertifiedMessageTypeChange = (type) => {},
}) {
  const maxHashesAllowed = Math.max(maxAllowedHashesByRequest, 1);
  const validationPattern = ` *(\\w+ *, *){0,${maxHashesAllowed - 1}}\\w+,? *`;
  const [transactionHashes, setTransactionHashes] = useState("");
  const [blockHashes, setBlockHashes] = useState("");

  return (
    <FormGroup>
      <InputGroup hasValidation>
        <Button type="submit">Certify</Button>
        {Object.values(certifiedMessageTypes).map((type) => (
          <ToggleButton
            key={type.name}
            id={`toogle-${type.name}`}
            name="certifiedMessageType"
            value={type.name}
            type="radio"
            variant="outline-dark"
            checked={certifiedMessageType === type}
            onChange={() => onCertifiedMessageTypeChange(type)}>
            <span className="text-capitalize">{type.pluralName}</span>
          </ToggleButton>
        ))}
        {certifiedMessageType === certifiedMessageTypes.block ? (
          <Form.Control
            id="blockHashes"
            name="blockHashes"
            type="text"
            placeholder="comma-separated list of block hashes"
            required
            pattern={validationPattern}
            value={blockHashes}
            onChange={(e) => setBlockHashes(e.target.value)}
          />
        ) : (
          <Form.Control
            id="txHashes"
            name="txHashes"
            type="text"
            placeholder="comma-separated list of transactions hashes"
            required
            pattern={validationPattern}
            value={transactionHashes}
            onChange={(e) => setTransactionHashes(e.target.value)}
          />
        )}
        <Form.Control.Feedback type="invalid">
          Please provide a comma-separated list of {certifiedMessageType.pluralName} hashes.
        </Form.Control.Feedback>
      </InputGroup>
    </FormGroup>
  );
}
