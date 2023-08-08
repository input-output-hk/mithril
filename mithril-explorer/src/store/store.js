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
  }
  return undefined;
}

export function initStore(initialAggregator) {
  let state = loadFromLocalStorage();

  if (initialAggregator && checkUrl(initialAggregator)) {
    const baseState = (state)
      ? state
      : {
        settings: settingsInitialState
      };

    state = {
      ...baseState,
      settings: {
        ...baseState.settings,
        selectedAggregator: initialAggregator,
        availableAggregators:
          !baseState.settings.availableAggregators.includes(initialAggregator)
            ? [...baseState.settings.availableAggregators, initialAggregator]
            : baseState.settings.availableAggregators,
        canRemoveSelected: !default_available_aggregators.includes(initialAggregator),
      }
    };
  }

  return state;
}

export const storeBuilder = (initialAggregator) => configureStore({
  reducer: {
    settings: settingsSlice.reducer,
  },
  preloadedState: initStore(initialAggregator),
});
