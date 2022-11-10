import {saveToLocalStorage, storeBuilder} from "../store/store";
import {
  removeSelectedAggregator,
  selectAggregator,
  settingsSlice,
  setUpdateInterval,
  toggleAutoUpdate
} from "../store/settingsSlice";
import default_available_aggregators from "../aggregators-list";
import {initStore, resetLocation, setLocationToAggregator} from "./helpers";

describe('Store Initialization', () => {
  beforeEach(() => {
    resetLocation();
  });

  it('init with settings initialState without local storage', () => {
    const store = initStore();

    expect(store.getState().settings).toEqual(settingsSlice.getInitialState());
  });

  it('init with local storage saved state', () => {
    let aggregators = [...default_available_aggregators, "https://aggregator.test"];
    let expected = {
      settings: {
        ...settingsSlice.getInitialState(),
        selectedAggregator: aggregators.at(aggregators.length - 1),
        availableAggregators: aggregators,
        updateInterval: 12345,
      }
    };
    saveToLocalStorage(expected);
    const store = storeBuilder();

    expect(store.getState()).toEqual(expected);
  });

  it('init with local storage and default aggregator in url', () => {
    const aggregatorInUrl = default_available_aggregators.at(1);
    setLocationToAggregator(aggregatorInUrl);
    let aggregators = [...default_available_aggregators, "https://aggregator.test"];
    let expected = {
      settings: {
        ...settingsSlice.getInitialState(),
        selectedAggregator: aggregators.at(aggregators.length - 1),
        availableAggregators: aggregators,
        updateInterval: 12345,
      }
    };
    saveToLocalStorage(expected);
    expected.settings.selectedAggregator = aggregatorInUrl;
    const store = storeBuilder();

    expect(store.getState()).toEqual(expected);
  });

  it('Can toggle autoUpdate', () => {
    const store = initStore();

    store.dispatch(toggleAutoUpdate());
    expect(store.getState().settings.autoUpdate).toEqual(false);

    store.dispatch(toggleAutoUpdate());
    expect(store.getState().settings.autoUpdate).toEqual(true);
  });

  it('Can change updateInterval', () => {
    const store = initStore();
    const expected = 124325;

    store.dispatch(setUpdateInterval(expected));
    expect(store.getState().settings.updateInterval).toEqual(expected);
  });

  it('Can change selectedAggregator', () => {
    const store = initStore();
    const expected = default_available_aggregators[2];

    store.dispatch(selectAggregator(expected));
    expect(store.getState().settings.selectedAggregator).toEqual(expected);
  });

  it('Add a custom aggregator when selectAggregator is called with an unknown aggregator', () => {
    const store = initStore();
    const expected = "http://aggregator.test";

    store.dispatch(selectAggregator(expected));
    expect(store.getState().settings.selectedAggregator).toEqual(expected);
    expect(store.getState().settings.availableAggregators).toContain(expected);
  });

  it('Can\'t remove a default aggregator', () => {
    const store = initStore();

    store.dispatch(removeSelectedAggregator());
    expect(store.getState().settings.availableAggregators).toContain(default_available_aggregators[0]);
  });

  it('Can remove a custom aggregator', () => {
    const customAggregator = "http://aggregator.test";
    const store = initStore({
      settings: {
        ...settingsSlice.getInitialState(),
        selectedAggregator: customAggregator,
        availableAggregators: [...default_available_aggregators, customAggregator],
      }
    });

    store.dispatch(removeSelectedAggregator());
    expect(store.getState().settings.availableAggregators).not.toContain(customAggregator);
  });
});