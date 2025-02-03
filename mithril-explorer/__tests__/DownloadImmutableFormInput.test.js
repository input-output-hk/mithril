import { fireEvent, render, screen } from "@testing-library/react";
import "@testing-library/jest-dom";
import { DownloadImmutableFormInput } from "#/Artifacts/CardanoDbV2SnapshotsList/DownloadButton";

const maxImmutable = 100_000;

function setup(max) {
  const utils = render(<DownloadImmutableFormInput max={max} />);
  return {
    input: screen.getByRole("spinbutton"),
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

  it.each([0, 123, 67_782, maxImmutable])(
    "Immutable below or equal to max allowed: %d",
    (immutable_file_number) => {
      const { input } = setup(maxImmutable);
      fireEvent.change(input, {
        target: { value: immutable_file_number },
      });

      expect(input.checkValidity()).toBeTruthy();
      expect(input.value).toBe(`${immutable_file_number}`);
    },
  );

  it.each([-4328, -1, maxImmutable + 1, 528_432])(
    "Immutable above max or below 0 is invalid: %d",
    (immutable_file_number) => {
      const { input } = setup(maxImmutable);
      fireEvent.change(input, {
        target: { value: immutable_file_number },
      });

      expect(input.checkValidity()).toBeFalsy();
    },
  );

  it.each(["@124", "⚠️", "text"])("Non-number is invalid: %s", (immutable_file_number) => {
    const { input } = setup({ maxImmutable });
    fireEvent.change(input, {
      target: { value: immutable_file_number },
    });

    expect(input.checkValidity()).toBeFalsy();
  });

  it.each([0.1, 1.432, 67_782.32])("Float is invalid: %f", ({ immutable_file_number }) => {
    const { input } = setup({ maxImmutable });
    fireEvent.change(input, {
      target: { value: immutable_file_number },
    });

    expect(input.checkValidity()).toBeFalsy();
  });
});
