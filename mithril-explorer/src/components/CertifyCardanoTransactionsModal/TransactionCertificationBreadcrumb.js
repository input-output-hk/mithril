import React from "react";
import { ListGroup, Spinner } from "react-bootstrap";
import { validationSteps } from "./index";

export default function TransactionCertificationBreadcrumb({
  currentStep,
  // Note: validity values are only relevant after their respective steps
  isProofValid,
  isCertificateChainValid,
  onStepClick = (step) => {},
}) {
  function variantForActiveStep(currentStep, step) {
    if (currentStep < step) {
      return "light";
    } else if (step === currentStep) {
      return "primary";
    }

    let altVariant = "";
    switch (step) {
      case validationSteps.fetchingProof:
        altVariant = "light";
        break;
      case validationSteps.validatingProof:
        altVariant = isProofValid ? "light" : "danger";
        break;
      case validationSteps.validatingCertificateChain:
        altVariant = isProofValid && isCertificateChainValid ? "light" : "danger";
        break;
    }
    return altVariant;
  }

  function variantForDoneStep(currentStep) {
    let variant = "light";
    if (currentStep === validationSteps.done) {
      variant = isProofValid && isCertificateChainValid ? "success" : "danger";
    }
    return variant;
  }

  const activeSteps = [
    {
      step: validationSteps.fetchingProof,
      text: "Fetching Transactions Proof",
    },
    {
      step: validationSteps.validatingProof,
      text: "Verifying Transactions Proof",
    },
    {
      step: validationSteps.validatingCertificateChain,
      text: "Validating Certificate Chain",
    },
  ];

  return (
    <ListGroup horizontal="sm" activeKey={currentStep} numbered>
      {activeSteps.map(({ step, text }) => (
        <ListGroup.Item
          action
          key={step}
          eventKey={step}
          disabled={step > validationSteps.validatingProof && !isProofValid}
          onClick={() => onStepClick(step)}
          variant={variantForActiveStep(currentStep, step)}>
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
