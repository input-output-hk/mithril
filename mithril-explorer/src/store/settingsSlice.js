import { createSlice } from "@reduxjs/toolkit";
import * as aggregator_api from "@/aggregator-api";
import default_available_aggregators from "@/aggregators-list.generated";
import { checkUrl } from "@/utils";
import { defaultAggregatorCapabilities } from "@/constants";

export const initialState = {
  updateInterval: undefined,
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
      if (!checkUrl(action.payload?.url)) {
        return state;
      }

      const availableAggregators = state.availableAggregators
        .map((a) => a.url)
        .includes(action.payload.url)
        ? state.availableAggregators
        : [...state.availableAggregators, action.payload];
      const selectedAggregator =
        state.availableAggregators.find((a) => a.url === action.payload.url) ?? action.payload;

      return {
        ...state,
        selectedAggregator: selectedAggregator,
        availableAggregators: availableAggregators,
        canRemoveSelected: !default_available_aggregators
          .map((d) => d.url)
          .includes(action.payload.url),
      };
    },
    removeSelectedAggregator: (state) => {
      if (default_available_aggregators.map((d) => d.url).includes(state.selectedAggregator.url)) {
        return state;
      }

      return {
        ...state,
        selectedAggregator: state.availableAggregators.at(0),
        availableAggregators: state.availableAggregators.filter(
          (a) => a.url !== state.selectedAggregator.url,
        ),
        canRemoveSelected: !default_available_aggregators
          .map((d) => d.url)
          .includes(state.availableAggregators.at(0).url),
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

      if (aggregator?.url) {
        const capabilities = await aggregator_api.fetchAggregatorCapabilities(aggregator.url);
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
export const selectedAggregatorUrl = (state) => state.settings.selectedAggregator.url;
export const selectedAggregatorCapabilities = (state) =>
  state.settings?.aggregatorCapabilities ?? defaultAggregatorCapabilities;
export const selectedAggregatorSignedEntities = (state) =>
  state.settings?.aggregatorCapabilities?.signed_entity_types ?? [];

export default settingsSlice.reducer;
