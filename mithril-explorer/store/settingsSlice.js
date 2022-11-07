import {createSlice} from "@reduxjs/toolkit";
import available_aggregators from "../aggregators-list";

export const settingsSlice = createSlice({
  name: 'settings',
  initialState: {
    aggregator: available_aggregators[0],
    interval: 10000,
    autoUpdate: true,
  },
  reducers: {
    setFetchInterval: (state, action) => {
      state.interval = state.value;
    },
    toggleAutoUpdate: (state) => {
      state.autoUpdate = !state.autoUpdate;
    },
  }
});

export const { setFetchInterval, toggleAutoUpdate } = settingsSlice.actions;

export default settingsSlice.reducer;