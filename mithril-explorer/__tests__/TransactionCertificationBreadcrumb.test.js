import { render, screen } from "@testing-library/react";
import "@testing-library/jest-dom";
import TransactionCertificationBreadcrumb from "#/CertifyCardanoTransactionsModal/TransactionCertificationBreadcrumb";
import { validationSteps } from "#/CertifyCardanoTransactionsModal";

function setup(currentStep, isProofValid, isCertificateChainValid) {
  const utils = [
    render(
      <TransactionCertificationBreadcrumb
        currentStep={currentStep}
        isProofValid={isProofValid}
        isCertificateChainValid={isCertificateChainValid}
      />,
    ),
  ];

  const tabs = new Map();
  tabs.set(validationSteps.fetchingProof, screen.getByText(/Fetching Transactions Proof/i));
  tabs.set(validationSteps.validatingProof, screen.getByText(/Verifying Transactions Proof/i));
  tabs.set(
    validationSteps.validatingCertificateChain,
    screen.getByText(/Validating Certificate Chain/i),
  );
  tabs.set(validationSteps.done, screen.getByText(/Finish/i));

  return {
    tabs: tabs,
    ...utils,
  };
}

const classForVariant = (variant) => `list-group-item-${variant}`;

describe("TransactionCertificationBreadcrumb", () => {
  it("The tab variant are light when default", () => {
    const { tabs } = setup(validationSteps.ready, false, false);

    for (const [_key, tab] of tabs) {
      expect(tab).toHaveClass(classForVariant("light"));
    }
  });

  it.each([
    ["fetchingProof", validationSteps.fetchingProof],
    ["validatingProof", validationSteps.validatingProof],
    ["validatingCertificateChain", validationSteps.validatingCertificateChain],
  ])("The tab variant is primary when current and checks are valid : %s", (stepName, stepIndex) => {
    const { tabs } = setup(stepIndex, true, true);

    expect(tabs.get(stepIndex)).toHaveClass(classForVariant("primary"));
  });

  it("Current step is done and checks are valid then tabs are light except done step tab that is success", () => {
    const { tabs } = setup(validationSteps.done, true, true);

    expect(tabs.get(validationSteps.fetchingProof)).toHaveClass(classForVariant("light"));
    expect(tabs.get(validationSteps.validatingProof)).toHaveClass(classForVariant("light"));
    expect(tabs.get(validationSteps.validatingCertificateChain)).toHaveClass(
      classForVariant("light"),
    );
    expect(tabs.get(validationSteps.done)).toHaveClass(classForVariant("success"));
  });

  it("Current step is done and proof check is invalid then tabs are danger except tabs before proof validation", () => {
    const { tabs } = setup(validationSteps.done, false, true);

    expect(tabs.get(validationSteps.fetchingProof)).toHaveClass(classForVariant("light"));
    expect(tabs.get(validationSteps.validatingProof)).toHaveClass(classForVariant("danger"));
    expect(tabs.get(validationSteps.validatingCertificateChain)).toHaveClass(
      classForVariant("danger"),
    );
    expect(tabs.get(validationSteps.done)).toHaveClass(classForVariant("danger"));
  });

  it("Current step is done and certificate chain check is invalid then tabs are danger except tabs before certificate chain validation", () => {
    const { tabs } = setup(validationSteps.done, true, false);

    expect(tabs.get(validationSteps.fetchingProof)).toHaveClass(classForVariant("light"));
    expect(tabs.get(validationSteps.validatingProof)).toHaveClass(classForVariant("light"));
    expect(tabs.get(validationSteps.validatingCertificateChain)).toHaveClass(
      classForVariant("danger"),
    );
    expect(tabs.get(validationSteps.done)).toHaveClass(classForVariant("danger"));
  });
});
