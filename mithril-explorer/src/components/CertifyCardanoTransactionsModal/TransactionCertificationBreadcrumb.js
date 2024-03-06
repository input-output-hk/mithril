import React from "react";
import { ListGroup, Spinner } from "react-bootstrap";
import { validationSteps } from "./index";

export function TransactionCertificationBreadcrumb({
  currentStep,
  isSuccess,
  onStepClick = (step) => {},
}) {
  function variantForDoneStep(currentStep) {
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
          action
          key={step}
          eventKey={step}
          onClick={() => onStepClick(step)}
          variant={currentStep === step ? "primary" : "light"}>
          {text} {currentStep === step && <Spinner size="sm" />}
        </ListGroup.Item>
      ))}
      <ListGroup.Item
        action
        onClick={() => onStepClick(validationSteps.done)}
        eventKey={validationSteps.done}
        variant={variantForDoneStep(currentStep)}>
        Finish
      </ListGroup.Item>
    </ListGroup>
  );
}
