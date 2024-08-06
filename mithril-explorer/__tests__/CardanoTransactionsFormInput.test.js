import { fireEvent, render, screen } from "@testing-library/react";
import "@testing-library/jest-dom";
import CardanoTransactionsFormInput from "#/CardanoTransactionsFormInput";

function setup(maxHashesAllowedByRequest = 100) {
  const utils = [
    render(<CardanoTransactionsFormInput maxAllowedHashesByRequest={maxHashesAllowedByRequest} />),
  ];
  return {
    input: screen.getByRole("textbox"),
    ...utils,
  };
}

describe("CardanoTransactionsFormInput", () => {
  it("Empty default to invalid", () => {
    const { input } = setup();
    expect(input.checkValidity()).toBeFalsy();
    expect(input.value).toBe("");
  });

  it("Setting empty string is invalid", () => {
    const { input } = setup();
    fireEvent.change(input, { target: { value: "" } });
    expect(input.checkValidity()).toBeFalsy();
    expect(input.value).toBe("");
  });

  it.each([
    ["one transaction", "c44d1f8df92bc75fe74d69b236b1c0ebcab1e3c350ee5ac70e8a8ef53c1e5918"],
    [
      "one transaction with space before",
      "  c44d1f8df92bc75fe74d69b236b1c0ebcab1e3c350ee5ac70e8a8ef53c1e5918",
    ],
    [
      "one transaction with space after",
      "c44d1f8df92bc75fe74d69b236b1c0ebcab1e3c350ee5ac70e8a8ef53c1e5918     ",
    ],
    [
      "one transaction surrounded with spaces",
      " c44d1f8df92bc75fe74d69b236b1c0ebcab1e3c350ee5ac70e8a8ef53c1e5918  ",
    ],
    [
      "two transactions comma separated",
      "c44d1f8df92bc75fe74d69b236b1c0ebcab1e3c350ee5ac70e8a8ef53c1e5918,b45d2f9df92bc75fe74e69b236b1c0ebcab1e3c350ee5ac71e8a8ef53c1e5918",
    ],
    [
      "two transactions separated with a comma and space",
      "c44d1f8df92bc75fe74d69b236b1c0ebcab1e3c350ee5ac70e8a8ef53c1e5918, b45d2f9df92bc75fe74e69b236b1c0ebcab1e3c350ee5ac71e8a8ef53c1e5918",
    ],
    [
      "two transactions comma separated with spaces before each transaction",
      "   c44d1f8df92bc75fe74d69b236b1c0ebcab1e3c350ee5ac70e8a8ef53c1e5918, b45d2f9df92bc75fe74e69b236b1c0ebcab1e3c350ee5ac71e8a8ef53c1e5918",
    ],
    [
      "two transactions comma separated with spaces after each transaction",
      "c44d1f8df92bc75fe74d69b236b1c0ebcab1e3c350ee5ac70e8a8ef53c1e5918    ,b45d2f9df92bc75fe74e69b236b1c0ebcab1e3c350ee5ac71e8a8ef53c1e5918   ",
    ],
    [
      "three transactions comma separated surrounded with spaces",
      "   c44d1f8df92bc75fe74d69b236b1c0ebcab1e3c350ee5ac70e8a8ef53c1e5918 , a60d2f9df92bc75fe74e69b224b1c0bacab1e3c350ee5ac71e8a8ef65c1e5918   ,b45d2f9df92bc75fe74e69b236b1c0ebcab1e3c350ee5ac71e8a8ef53c1e5918   ",
    ],
  ])("Setting valid transactions: %s", (_, transactions) => {
    const { input } = setup();
    fireEvent.change(input, {
      target: { value: transactions },
    });

    expect(input.checkValidity()).toBeTruthy();
    expect(input.value).toBe(transactions);
  });

  it.each([
    ["special characters", "$44d1f8df92bc75fe74d69b236b1c0ebcab1e3c350ee5ac70e8a8ef53c1e5918"],
    [
      "one valid, one invalid",
      "c44d1f8df92bc75fe74d69b236b1c0ebcab1e3c350ee5ac70e8a8ef53c1e5918, @45d2f9df92bc75fe74e69b236b1c0ebcab1e3c350ee5ac71e8a8ef53c1e5918",
    ],
  ])("Setting invalid transactions: %s", (_, transactions) => {
    const { input } = setup();
    fireEvent.change(input, {
      target: { value: transactions },
    });

    expect(input.checkValidity()).toBeFalsy();
    expect(input.value).toBe(transactions);
  });

  it.each([
    ["Max 'one'", 1],
    ["Max 'zero' should be equivalent to max 'one'", 0],
    ["Max 'two'", 2],
  ])("Setting more than max hashes allowed by request: %s", (_, maxHashesAllowedByRequest) => {
    const { input } = setup(maxHashesAllowedByRequest);
    const transactions =
      "c44d1f8df92bc75fe74d69b236b1c0ebcab1e3c350ee5ac70e8a8ef53c1e5918,b45d2f9df92bc75fe74e69b236b1c0ebcab1e3c350ee5ac71e8a8ef53c1e5918,a60d2f9df92bc75fe74e69b224b1c0bacab1e3c350ee5ac71e8a8ef65c1e5918";
    fireEvent.change(input, {
      target: { value: transactions },
    });

    expect(input.checkValidity()).toBeFalsy();
    expect(input.value).toBe(transactions);
  });
});
