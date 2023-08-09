import {configureStore} from "@reduxjs/toolkit";
import {initialState as settingsInitialState, settingsSlice} from "./settingsSlice";
import default_available_aggregators from "../aggregators-list";
import {checkUrl} from "../utils";

const SAVED_STATE_KEY = "Explorer_State";

export function saveToLocalStorage(state) {
  if (typeof window !== 'undefined' && localStorage) {
    localStorage.setItem(SAVED_STATE_KEY, JSON.stringify(state));
  }
}

function loadFromLocalStorage() {
  if (typeof window !== 'undefined' && localStorage) {
    const serialisedState = localStorage.getItem(SAVED_STATE_KEY);
    return serialisedState ? JSON.parse(serialisedState) : undefined;
  } else {
    return undefined;
  }
}

function reorderAggregatorAndAddMissingDefaults(currentList, defaultAggregators) {
  let not_default_aggregators = currentList.filter(a => !defaultAggregators.includes(a));
  return [...defaultAggregators, ...not_default_aggregators];
}

function getSettings(defaultSettings, initialAggregator) {
  let settings = defaultSettings ?? settingsInitialState;
  const aggregators =
    reorderAggregatorAndAddMissingDefaults(settings.availableAggregators, default_available_aggregators);

  if (initialAggregator && checkUrl(initialAggregator)) {
    if (!aggregators.includes(initialAggregator)) {
      aggregators.push(initialAggregator);
    }

    settings = {
      ...settings,
      selectedAggregator: initialAggregator,
      availableAggregators: aggregators,
      canRemoveSelected: !default_available_aggregators.includes(initialAggregator),
    };
  } else {
    settings = {
      ...settings,
      availableAggregators: aggregators,
    };
  }

  return settings;
}

function initStore(defaultState, initialAggregator) {
  return {
    ...defaultState,
    settings: getSettings(defaultState?.settings, initialAggregator),
  };
}

export function initStoreFromLocalStorage(initialAggregator) {
  return initStore(loadFromLocalStorage(), initialAggregator);
}

export const storeBuilder = (initialAggregator) => configureStore({
  reducer: {
    settings: settingsSlice.reducer,
  },
  preloadedState: initStoreFromLocalStorage(initialAggregator),
});
