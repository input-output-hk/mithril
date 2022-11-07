import { configureStore } from "@reduxjs/toolkit";
import { createWrapper } from "next-redux-wrapper";
import {settingsSlice} from "./settingsSlice";

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

const storeBuilder = () => configureStore({
  reducer: {
    settings: settingsSlice.reducer,
  },
  preloadedState: loadFromLocalStorage(),
});

export const storeWrapper = createWrapper(storeBuilder);