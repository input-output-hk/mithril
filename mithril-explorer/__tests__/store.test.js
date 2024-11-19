import default_available_aggregators from "@/aggregators-list";
import { signedEntityType } from "@/constants";
import { poolsSlice } from "@/store/poolsSlice";
import {
  changeRefreshSeed,
  removeSelectedAggregator,
  selectAggregator,
  settingsSlice,
  setUpdateInterval,
} from "@/store/settingsSlice";
import { saveToLocalStorage, storeBuilder } from "@/store/store";
import { waitFor } from "@testing-library/react";
import { initStore } from "./helpers";

const expectedCapabilities = {
  signed_entity_types: [signedEntityType.CardanoTransactions],
};

jest.mock("../src/aggregator-api", () => {
  return {
    fetchAggregatorCapabilities: jest.fn().mockImplementation(() => {
      return expectedCapabilities;
    }),
  };
});

describe("Store Initialization", () => {
  it("init with settings initialState without local storage", () => {
    const store = storeBuilder();

    expect(store.getState().settings).toEqual(settingsSlice.getInitialState());
  });

  it("init with local storage saved state", () => {
    let aggregators = [...default_available_aggregators, "https://aggregator.test"];
    let expected = {
      pools: poolsSlice.getInitialState(),
      settings: {
        ...settingsSlice.getInitialState(),
        selectedAggregator: aggregators.at(aggregators.length - 1),
        availableAggregators: aggregators,
        updateInterval: 12345,
      },
    };
    saveToLocalStorage(expected);
    const store = storeBuilder();

    expect(store.getState()).toEqual(expected);
  });

  it("init with local storage and initial aggregator", () => {
    const initialAggregator = default_available_aggregators.at(1);
    let aggregators = [...default_available_aggregators, "https://aggregator.test"];
    let expected = {
      pools: poolsSlice.getInitialState(),
      settings: {
        ...settingsSlice.getInitialState(),
        selectedAggregator: aggregators.at(aggregators.length - 1),
        availableAggregators: aggregators,
        updateInterval: 12345,
      },
    };
    saveToLocalStorage(expected);
    expected.settings.selectedAggregator = initialAggregator;
    const store = storeBuilder(initialAggregator);

    expect(store.getState()).toEqual(expected);
  });

  it("Can change refreshSeed", () => {
    const initialSeed = 123;
    const store = initStore({
      settings: {
        refreshSeed: initialSeed,
        ...settingsSlice.getInitialState(),
      },
    });

    store.dispatch(changeRefreshSeed());
    expect(store.getState().settings.refreshSeed).not.toEqual(initialSeed);
  });

  it("Set updateInterval to a number", () => {
    const store = initStore({
      settings: {
        updateInterval: 0,
        ...settingsSlice.getInitialState(),
      },
    });
    const expected = 124325;

    store.dispatch(setUpdateInterval(expected));
    expect(store.getState().settings.updateInterval).toEqual(expected);
  });

  it.each(["string", 1.123, "143", null, undefined])(
    "Set updateInterval to something that is not an integer should yield undefined ('%s')",
    (value) => {
      const store = initStore({
        settings: {
          updateInterval: 1000,
          ...settingsSlice.getInitialState(),
        },
      });

      store.dispatch(setUpdateInterval(value));
      expect(store.getState().settings.updateInterval).toEqual(undefined);
    },
  );

  it("Can change selectedAggregator", () => {
    const store = initStore();
    const expected = default_available_aggregators[2];

    store.dispatch(selectAggregator(expected));
    expect(store.getState().settings.selectedAggregator).toEqual(expected);
  });

  it("Add a custom aggregator when selectAggregator is called with an unknown aggregator", () => {
    const store = initStore();
    const expected = "http://aggregator.test";

    store.dispatch(selectAggregator(expected));
    expect(store.getState().settings.selectedAggregator).toEqual(expected);
    expect(store.getState().settings.availableAggregators).toContain(expected);
  });

  it("Changing selectedAggregator launch aggregatorCapabilities fetching", async () => {
    const store = initStore({
      settings: {
        ...settingsSlice.getInitialState(),
        aggregatorCapabilities: { signed_entity_types: [] },
      },
    });
    const aggregator = "http://aggregator.test";

    store.dispatch(selectAggregator(aggregator));
    await waitFor(() =>
      expect(store.getState().settings.aggregatorCapabilities).toEqual(expectedCapabilities),
    );
  });

  it("Can't remove a default aggregator", () => {
    const store = initStore();

    store.dispatch(removeSelectedAggregator());
    expect(store.getState().settings.availableAggregators).toContain(
      default_available_aggregators[0],
    );
  });

  it("Can remove a custom aggregator", () => {
    const customAggregator = "http://aggregator.test";
    const store = initStore({
      settings: {
        ...settingsSlice.getInitialState(),
        selectedAggregator: customAggregator,
        availableAggregators: [...default_available_aggregators, customAggregator],
      },
    });

    store.dispatch(removeSelectedAggregator());
    expect(store.getState().settings.availableAggregators).not.toContain(customAggregator);
  });

  it("loading state from local storage should sort default aggregators", () => {
    const oldDefaultAggregators = [...default_available_aggregators, "http://aggregator.test"];
    oldDefaultAggregators.reverse();
    let expected = [...default_available_aggregators, "http://aggregator.test"];

    saveToLocalStorage({
      settings: {
        ...settingsSlice.getInitialState(),
        availableAggregators: oldDefaultAggregators,
      },
    });
    const store = storeBuilder();

    expect(store.getState().settings.availableAggregators).toEqual(expected);
  });

  it("loading state from local storage should add new default aggregators", () => {
    const oldDefaultAggregators = [...default_available_aggregators];
    const newDefaultAggregator = oldDefaultAggregators.shift();

    saveToLocalStorage({
      settings: {
        ...settingsSlice.getInitialState(),
        availableAggregators: oldDefaultAggregators,
      },
    });
    const store = storeBuilder();

    expect(store.getState().settings.availableAggregators).toContain(newDefaultAggregator);
  });
});
