import { createSlice } from "@reduxjs/toolkit";
import aggregator_api from "@/aggregator-api";
import default_available_aggregators from "@/aggregators-list";
import { checkUrl } from "@/utils";
import { defaultAggregatorCapabilities } from "@/constants";

export const initialState = {
  updateInterval: 10000,
  refreshSeed: 0,
  selectedAggregator: default_available_aggregators[0],
  availableAggregators: default_available_aggregators,
  aggregatorCapabilities: defaultAggregatorCapabilities,
  canRemoveSelected: false,
};

export const settingsSlice = createSlice({
  name: "settings",
  initialState: initialState,
  reducers: {
    setUpdateInterval: (state, action) => {
      const interval = action.payload;
      state.updateInterval =
        !Number.isNaN(interval) && Number.isInteger(interval) ? interval : undefined;
    },
    changeRefreshSeed: (state) => {
      state.refreshSeed = Math.random();
    },
    selectAggregator: (state, action) => {
      if (!checkUrl(action.payload)) {
        return state;
      }

      const availableAggregators = state.availableAggregators.includes(action.payload)
        ? state.availableAggregators
        : [...state.availableAggregators, action.payload];

      return {
        ...state,
        selectedAggregator: action.payload,
        availableAggregators: availableAggregators,
        canRemoveSelected: !default_available_aggregators.includes(action.payload),
      };
    },
    removeSelectedAggregator: (state) => {
      if (default_available_aggregators.includes(state.selectedAggregator)) {
        return state;
      }

      return {
        ...state,
        selectedAggregator: state.availableAggregators.at(0),
        availableAggregators: state.availableAggregators.filter(
          (a) => a !== state.selectedAggregator,
        ),
        canRemoveSelected: !default_available_aggregators.includes(
          state.availableAggregators.at(0),
        ),
      };
    },
    updateSelectedAggregatorCapabilities: (state, action) => {
      return {
        ...state,
        aggregatorCapabilities: action.payload,
      };
    },
  },
});

export const addSettingsListeners = (listenerMiddleware) => {
  listenerMiddleware.startListening({
    actionCreator: selectAggregator,
    effect: async (action, listenerApi) => {
      const aggregator = action.payload;

      if (aggregator) {
        const capabilities = await aggregator_api.fetchAggregatorCapabilities(aggregator);
        listenerApi.dispatch(updateSelectedAggregatorCapabilities(capabilities));
      }
    },
  });
};

export const {
  setUpdateInterval,
  changeRefreshSeed,
  selectAggregator,
  removeSelectedAggregator,
  updateSelectedAggregatorCapabilities,
} = settingsSlice.actions;

export const selectedAggregator = (state) => state.settings.selectedAggregator;
export const selectedAggregatorCapabilities = (state) =>
  state.settings?.aggregatorCapabilities ?? defaultAggregatorCapabilities;
export const selectedAggregatorSignedEntities = (state) =>
  state.settings?.aggregatorCapabilities?.signed_entity_types ?? [];

export default settingsSlice.reducer;
