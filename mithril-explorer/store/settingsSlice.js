import {createSlice} from "@reduxjs/toolkit";
import default_available_aggregators from "../aggregators-list";
import {checkUrl} from "../utils";

export const initialState = {
  autoUpdate: true,
  updateInterval: 10000,
  selectedAggregator: default_available_aggregators[0],
  availableAggregators: default_available_aggregators,
  canRemoveSelected: false,
};

export const settingsSlice = createSlice({
  name: 'settings',
  initialState: initialState,
  reducers: {
    setUpdateInterval: (state, action) => {
      state.updateInterval = action.payload;
    },
    toggleAutoUpdate: (state) => {
      state.autoUpdate = !state.autoUpdate;
    },
    selectAggregator: (state, action) => {
      if (!checkUrl(action.payload)) {
        return state;
      }

      const availableAggregators =
        state.availableAggregators.includes(action.payload)
          ? state.availableAggregators
          : [...state.availableAggregators, action.payload];

      return {
        ...state,
        selectedAggregator: action.payload,
        availableAggregators: availableAggregators,
        canRemoveSelected: !default_available_aggregators.includes(action.payload),
      }
    },
    removeCustomAggregator: (state) => {
      if (default_available_aggregators.includes(state.selectedAggregator)) {
        return state;
      }

      return {
        ...state,
        selectedAggregator: state.availableAggregators.at(0),
        availableAggregators: state.availableAggregators.filter(a => a !== state.selectedAggregator),
        canRemoveSelected: !default_available_aggregators.includes(state.availableAggregators.at(0)),
      }
    }
  }
});

export const {
  setUpdateInterval,
  toggleAutoUpdate,
  selectAggregator,
  removeCustomAggregator
} = settingsSlice.actions;

export default settingsSlice.reducer;