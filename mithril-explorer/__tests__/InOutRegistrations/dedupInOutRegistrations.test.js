import { dedupInOutRegistrations } from "@/utils";
import { reg } from "../helpers";

describe("In/Out registrations deduplication", () => {
  it("should remove duplicate 'in' registrations", () => {
    const registrations = {
      4: {
        in: [],
        out: [reg("party1", 10), reg("party2", 20)],
      },
      3: {
        in: [reg("party2", 20)],
        out: [],
      },
      2: {
        in: [reg("party1", 10)],
        out: [],
      },
    };

    const result = dedupInOutRegistrations(registrations);

    expect(result).toEqual({
      4: {
        in: [],
        out: [reg("party1", 10), reg("party2", 20)],
      },
    });
  });

  it("should remove duplicate 'out' registrations", () => {
    const registrations = {
      4: {
        in: [reg("party1", 10), reg("party2", 20)],
        out: [],
      },
      3: {
        in: [],
        out: [reg("party2", 20)],
      },
      2: {
        in: [],
        out: [reg("party1", 10)],
      },
    };

    const result = dedupInOutRegistrations(registrations);

    expect(result).toEqual({
      4: {
        in: [reg("party1", 10), reg("party2", 20)],
        out: [],
      },
    });
  });
});
