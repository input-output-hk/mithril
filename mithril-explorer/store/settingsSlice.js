import {createSlice} from "@reduxjs/toolkit";
import available_aggregators from "../aggregators-list";

export const settingsSlice = createSlice({
  name: 'settings',
  initialState: {
    aggregator: available_aggregators[0],
    updateInterval: 10000,
    autoUpdate: true,
  },
  reducers: {
    setUpdateInterval: (state, action) => {
      state.updateInterval = action.payload;
    },
    toggleAutoUpdate: (state) => {
      state.autoUpdate = !state.autoUpdate;
    },
  }
});

export const { setUpdateInterval, toggleAutoUpdate } = settingsSlice.actions;

export default settingsSlice.reducer;