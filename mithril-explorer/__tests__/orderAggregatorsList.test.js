const { compareNetworks } = require("../scripts/generate.aggregators-list");

describe("compareNetworks", () => {
  it.each([
    [
      ["release-preprod", "release-mainnet", "release-preview"],
      ["release-mainnet", "release-preprod", "release-preview"],
    ],
    [
      ["testing-preview", "pre-release-preview", "release-preprod", "release-mainnet"],
      ["release-mainnet", "release-preprod", "pre-release-preview", "testing-preview"],
    ],
    [
      ["custom-preview", "custom-mainnet", "custom-preprod"],
      ["custom-mainnet", "custom-preprod", "custom-preview"],
    ],
  ])("orders first by main network: mainnet, preprod, preview", (list, expected) => {
    expect(list.sort(compareNetworks)).toEqual(expected);
  });

  it.each([
    [
      ["testing-mainnet", "pre-release-mainnet", "release-mainnet"],
      ["release-mainnet", "pre-release-mainnet", "testing-mainnet"],
    ],
    [
      ["testing-preprod", "release-preprod", "pre-release-preprod"],
      ["release-preprod", "pre-release-preprod", "testing-preprod"],
    ],
    [
      ["testing-preview", "pre-release-preview", "release-preview"],
      ["release-preview", "pre-release-preview", "testing-preview"],
    ],
  ])("orders second by subtype: release, pre-release, testing", (list, expected) => {
    expect(list.sort(compareNetworks)).toEqual(expected);
  });

  it.each([
    [
      ["zeta-mainnet", "alpha-mainnet", "beta-mainnet"],
      ["alpha-mainnet", "beta-mainnet", "zeta-mainnet"],
    ],
    [
      ["release-preview-zeta", "release-preview-alpha", "release-preview-beta"],
      ["release-preview-alpha", "release-preview-beta", "release-preview-zeta"],
    ],
    [
      ["testing-preprod-2", "testing-preprod-1", "testing-preprod-3"],
      ["testing-preprod-1", "testing-preprod-2", "testing-preprod-3"],
    ],
  ])(
    "orders alphabetically when main network and subtype have the same priority",
    (list, expected) => {
      expect(list.sort(compareNetworks)).toEqual(expected);
    },
  );

  it.each([
    [
      ["unknown-zeta", "unknown-alpha", "unknown-beta"],
      ["unknown-alpha", "unknown-beta", "unknown-zeta"],
    ],
    [
      ["release-unknown-zeta", "release-unknown-alpha", "release-unknown-beta"],
      ["release-unknown-alpha", "release-unknown-beta", "release-unknown-zeta"],
    ],
  ])("orders alphabetically when networks do not match a main network", (list, expected) => {
    expect(list.sort(compareNetworks)).toEqual(expected);
  });

  it.each([
    ["release-mainnet", "release-preprod"],
    ["release-preprod", "release-preview"],
    ["release-mainnet", "testing-mainnet"],
    ["pre-release-mainnet", "testing-mainnet"],
    ["alpha-mainnet", "beta-mainnet"],
  ])("returns an opposite sign when arguments are reversed: %s / %s", (left, right) => {
    expect(Math.sign(compareNetworks(left, right))).toBe(-Math.sign(compareNetworks(right, left)));
  });

  it.each([["release-mainnet"], ["pre-release-preprod"], ["testing-preview"], ["unknown-network"]])(
    "returns 0 when comparing identical network names: %s",
    (network) => {
      expect(compareNetworks(network, network)).toBe(0);
    },
  );
});
