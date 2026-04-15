import { Button, Form, FormGroup, InputGroup, ToggleButton } from "react-bootstrap";
import React, { useState } from "react";

export default function CardanoBlocksTransactionsFormInput({ maxAllowedHashesByRequest }) {
  const maxHashesAllowed = Math.max(maxAllowedHashesByRequest, 1);
  const validationPattern = ` *(\\w+ *, *){0,${maxHashesAllowed - 1}}\\w+,? *`;
  const [certifiedItemType, setCertifiedItemType] = useState("transaction");
  const [transactionHashes, setTransactionHashes] = useState("");
  const [blockHashes, setBlockHashes] = useState("");

  const certifiedItemTypes = [
    { value: "transaction", label: "Transactions" },
    { value: "block", label: "Blocks" },
  ];

  return (
    <FormGroup>
      <InputGroup hasValidation>
        <Button type="submit">Certify</Button>
        {certifiedItemTypes.map((type) => (
          <ToggleButton
            key={type.value}
            id={`toogle-${type.value}`}
            name="certifiedItemType"
            value={type.value}
            type="radio"
            variant="outline-dark"
            checked={certifiedItemType === type.value}
            onChange={() => setCertifiedItemType(type.value)}>
            {type.label}
          </ToggleButton>
        ))}
        {certifiedItemType === "block" ? (
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
          Please provide a comma-separated list of {certifiedItemType} hashes.
        </Form.Control.Feedback>
      </InputGroup>
    </FormGroup>
  );
}
