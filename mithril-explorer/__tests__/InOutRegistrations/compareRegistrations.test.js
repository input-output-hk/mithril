import { compareRegistrations } from "@/utils";
import { reg } from "../helpers";

describe("Compare registrations", () => {
  it("empty registrations return empty result", () => {
    const result = compareRegistrations([], []);

    expect(result).toEqual({ in: [], out: [] });
  });

  it("One registration 'still there'", () => {
    const result = compareRegistrations([reg("party1", 10)], [reg("party1", 10)]);

    expect(result).toEqual({ in: [], out: [] });
  });

  it("One 'in' registration", () => {
    const result = compareRegistrations([reg("party1", 10)], []);

    expect(result).toEqual({ in: [reg("party1", 10)], out: [] });
  });

  it("One 'out' registration", () => {
    const result = compareRegistrations([], [reg("party1", 10)]);

    expect(result).toEqual({ in: [], out: [reg("party1", 10)] });
  });

  it("'In', 'out', and 'still there' all together", () => {
    const result = compareRegistrations(
      [reg("party1", 10), reg("party3", 12), reg("party4", 13)],
      [reg("party1", 10), reg("party2", 11), reg("party3", 12)],
    );

    expect(result).toEqual({ in: [reg("party4", 13)], out: [reg("party2", 11)] });
  });
});
