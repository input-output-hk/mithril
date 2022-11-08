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

function initStore() {
  let state = loadFromLocalStorage();

  if (location?.search) {
    const params = new URLSearchParams(location.search);
    const aggregator = params.get('aggregator');

    if (aggregator && checkUrl(aggregator)) {
      const settings = {
        selectedAggregator: aggregator,
        availableAggregators:
          !default_available_aggregators.includes(aggregator)
            ? [...default_available_aggregators, aggregator]
            : default_available_aggregators,
        canRemoveSelected: !default_available_aggregators.includes(aggregator),
      };

      state = (state)
        ? {
          ...state,
          settings: {
            ...state.settings,
            ...settings
          }
        }
        : {
          settings: {
            ...settingsInitialState,
            ...settings,
          },
        };
    }
  }

  return state;
}

const storeBuilder = () => configureStore({
  reducer: {
    settings: settingsSlice.reducer,
  },
  preloadedState: initStore(),
});

export const storeWrapper = createWrapper(storeBuilder);