import { compareRegistrations, computeInOutRegistrations, dedupInOutRegistrations } from "@/utils";

function reg(party_id, stake) {
  return { party_id, stake };
}

describe("Compare registrations", () => {
  it("empty registrations return empty result", () => {
    const result = compareRegistrations([], []);

    expect(result).toEqual({ in: [], out: [] });
  });

  it("One registration still there", () => {
    const result = compareRegistrations([reg("party1", 10)], [reg("party1", 10)]);

    expect(result).toEqual({ in: [], out: [] });
  });

  it("One in registration", () => {
    const result = compareRegistrations([reg("party1", 10)], []);

    expect(result).toEqual({ in: [reg("party1", 10)], out: [] });
  });

  it("One out registration", () => {
    const result = compareRegistrations([], [reg("party1", 10)]);

    expect(result).toEqual({ in: [], out: [reg("party1", 10)] });
  });

  it("In, out, and still there altogether", () => {
    const result = compareRegistrations(
      [reg("party1", 10), reg("party3", 12), reg("party4", 13)],
      [reg("party1", 10), reg("party2", 11), reg("party3", 12)],
    );

    expect(result).toEqual({ in: [reg("party4", 13)], out: [reg("party2", 11)] });
  });
});

describe("In/Out registrations computation", () => {
  it("giving only one registrations return empty result", () => {
    const result = computeInOutRegistrations({ registered_at: 4, registrations: [] });

    expect(result).toEqual({});
  });

  it("empty registrations return empty result", () => {
    const result = computeInOutRegistrations(
      { registered_at: 4, registrations: [] },
      { registered_at: 3, registrations: [] },
      { registered_at: 2, registrations: [] },
    );

    expect(result).toEqual({});
  });

  it("In/Out list show latest stakes values", () => {
    const result = computeInOutRegistrations(
      { registered_at: 4, registrations: [reg("party1", 15)] },
      { registered_at: 3, registrations: [reg("party2", 20)] },
      { registered_at: 2, registrations: [reg("party1", 10), reg("party2", 11)] },
    );

    expect(result).toEqual({
      4: {
        in: [reg("party1", 15)],
        out: [reg("party2", 20)],
      },
      3: {
        in: [],
        out: [reg("party1", 10)],
      },
    });
  });

  it("Compare two epochs registrations", () => {
    const result = computeInOutRegistrations(
      {
        registered_at: 4,
        registrations: [reg("party1", 10), reg("party3", 12), reg("party4", 13)],
      },
      {
        registered_at: 3,
        registrations: [reg("party1", 10), reg("party2", 11), reg("party3", 12)],
      },
    );

    expect(result).toEqual({
      4: {
        in: [reg("party4", 13)],
        out: [reg("party2", 11)],
      },
    });
  });

  it("Compare three epochs registrations", () => {
    const result = computeInOutRegistrations(
      {
        registered_at: 4,
        registrations: [reg("party2", 11), reg("party4", 13), reg("party5", 14)],
      },
      {
        registered_at: 3,
        registrations: [reg("party1", 10), reg("party2", 11), reg("party4", 13)],
      },
      {
        registered_at: 2,
        registrations: [reg("party1", 10), reg("party2", 11), reg("party3", 12)],
      },
    );

    expect(result).toEqual({
      4: {
        in: [reg("party5", 14)],
        out: [reg("party1", 10)],
      },
      3: {
        in: [reg("party4", 13)],
        out: [reg("party3", 12)],
      },
    });
  });

  it("Compare four epochs registrations", () => {
    const result = computeInOutRegistrations(
      {
        registered_at: 5,
        registrations: [reg("party1", 10), reg("party2", 11), reg("party6", 15), reg("party7", 16)],
      },
      {
        registered_at: 4,
        registrations: [reg("party2", 11), reg("party4", 13), reg("party5", 14)],
      },
      {
        registered_at: 3,
        registrations: [reg("party1", 10), reg("party2", 11), reg("party4", 13)],
      },
      {
        registered_at: 2,
        registrations: [reg("party1", 10), reg("party2", 11), reg("party3", 12)],
      },
    );

    expect(result).toEqual({
      5: {
        in: [reg("party1", 10), reg("party6", 15), reg("party7", 16)],
        out: [reg("party4", 13), reg("party5", 14)],
      },
      4: {
        in: [reg("party5", 14)],
        out: [reg("party1", 10)],
      },
      3: {
        in: [reg("party4", 13)],
        out: [reg("party3", 12)],
      },
    });
  });
});

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
