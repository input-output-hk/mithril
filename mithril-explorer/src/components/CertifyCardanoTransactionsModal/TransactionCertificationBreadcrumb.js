import React from "react";
import { ListGroup, Spinner } from "react-bootstrap";
import { validationSteps } from "./index";

export function TransactionCertificationBreadcrumb({ currentStep, isSuccess }) {
  function variantForDoneStep() {
    let variant = "light";
    if (currentStep === validationSteps.done) {
      variant = isSuccess ? "success" : "danger";
    }
    return variant;
  }

  const activeSteps = [
    {
      step: validationSteps.fetchingProof,
      text: "Fetching Proof for the Transactions",
    },
    {
      step: validationSteps.validatingCertificateChain,
      text: "Validating Certificate Chain",
    },
    {
      step: validationSteps.validatingProof,
      text: "Verifying Proof against its Certificate",
    },
  ];

  return (
    <ListGroup horizontal="sm" activeKey={currentStep} numbered>
      {activeSteps.map(({ step, text }) => (
        <ListGroup.Item
          key={step}
          eventKey={step}
          variant={currentStep === step ? "primary" : "light"}>
          {text} {currentStep === step && <Spinner size="sm" />}
        </ListGroup.Item>
      ))}
      <ListGroup.Item eventKey={validationSteps.done} variant={variantForDoneStep()}>
        Finish
      </ListGroup.Item>
    </ListGroup>
  );
}
