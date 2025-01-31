import { fireEvent, render, screen } from "@testing-library/react";
import { test, fc } from "@fast-check/jest";
import "@testing-library/jest-dom";
import { DownloadImmutableFormInput } from "#/Artifacts/CardanoDbV2SnapshotsList/DownloadButton";

const maxImmutable = 100000;

function setup(max) {
  const utils = render(<DownloadImmutableFormInput max={max} />);
  return {
    // Note: in `fast-check` tests `screen.getByRole("spinbutton")` find two elements for a reason I don't understand, so
    // I'm using getAllByRole and selecting the first one to avoid the error.
    input: screen.getAllByRole("spinbutton")[0],
    ...utils,
  };
}

describe("DownloadImmutableFormInput", () => {
  it("Empty default to invalid", () => {
    const { input } = setup(maxImmutable);
    expect(input.checkValidity()).toBeFalsy();
    expect(input.value).toBe("");
  });

  it("Setting empty string is invalid", () => {
    const { input } = setup(maxImmutable);
    fireEvent.change(input, { target: { value: "" } });
    expect(input.checkValidity()).toBeFalsy();
    expect(input.value).toBe("");
  });

  test.prop({
    immutable_file_number: fc.nat({
      max: maxImmutable,
    }),
  })("Immutable below or equal to max allowed", ({ immutable_file_number }) => {
    const { input } = setup(maxImmutable);
    fireEvent.change(input, {
      // target: { value: `${immutable_file_number}` },
      target: { value: immutable_file_number },
    });

    expect(input.checkValidity()).toBeTruthy();
    expect(input.value).toBe(`${immutable_file_number}`);
  });

  test.prop({
    immutable_file_number: fc.oneof(fc.integer({ max: -1 }), fc.integer({ min: maxImmutable + 1 })),
  })("Immutable above max or below 0 is invalid", ({ immutable_file_number }) => {
    const { input } = setup(maxImmutable);
    fireEvent.change(input, {
      target: { value: immutable_file_number },
    });

    expect(input.checkValidity()).toBeFalsy();
  });

  test.prop({
    immutable_file_number: fc.string({ minLength: 1 }).filter((s) => isNaN(parseInt(s))),
  })("Non-number is invalid", ({ immutable_file_number }) => {
    const { input } = setup({ maxImmutable });
    fireEvent.change(input, {
      target: { value: immutable_file_number },
    });

    expect(input.checkValidity()).toBeFalsy();
  });

  test.prop({
    immutable_file_number: fc.float({ noInteger: true }),
  })("Float is invalid", ({ immutable_file_number }) => {
    const { input } = setup({ maxImmutable });
    fireEvent.change(input, {
      target: { value: immutable_file_number },
    });

    expect(input.checkValidity()).toBeFalsy();
  });
});
