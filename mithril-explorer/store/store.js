import { configureStore } from "@reduxjs/toolkit";
import { createWrapper } from "next-redux-wrapper";
import {settingsSlice} from "./settingsSlice";

const makeStore = () => configureStore({
  reducer: {
    settings: settingsSlice.reducer,
  },
})

export const storeWrapper = createWrapper(makeStore);