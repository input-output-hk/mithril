import {configureStore} from "@reduxjs/toolkit";
import {createWrapper} from "next-redux-wrapper";
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

export function initStore() {
  let state = loadFromLocalStorage();

  if (location?.search) {
    const params = new URLSearchParams(location.search);
    const aggregator = params.get('aggregator');

    if (aggregator && checkUrl(aggregator)) {
      const baseState = (state)
        ? state
        : {
          settings: settingsInitialState
        };

      state = {
        ...baseState,
        settings: {
          ...baseState.settings,
          selectedAggregator: aggregator,
          availableAggregators:
            !baseState.settings.availableAggregators.includes(aggregator)
              ? [...baseState.settings.availableAggregators, aggregator]
              : baseState.settings.availableAggregators,
          canRemoveSelected: !default_available_aggregators.includes(aggregator),
        }
      };
    }
  }

  return state;
}

export const storeBuilder = () => configureStore({
  reducer: {
    settings: settingsSlice.reducer,
  },
  preloadedState: initStore(),
});

export const storeWrapper = createWrapper(storeBuilder);