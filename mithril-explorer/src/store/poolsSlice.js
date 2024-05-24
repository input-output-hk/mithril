import { createAsyncThunk, createSelector, createSlice } from "@reduxjs/toolkit";
import aggregator_api from "@/aggregator-api";

// Delete all caches older than 6 hours
const cacheRefreshIntervalInMilliseconds = 6 * 3600 * 1000;

export const poolsSlice = createSlice({
  name: "pools",
  initialState: { list: [] },
  reducers: {},
  extraReducers: (builder) =>
    builder.addCase(updatePoolsForAggregator.fulfilled, (state, action) => {
      if (action.payload.keep_cached_data) {
        return;
      }
      const existing = poolsForAggregator(state, action.payload.aggregator);

      if (existing) {
        existing.network = action.payload.network;
        existing.pools = action.payload.pools;
        existing.date = action.payload.date;
      } else {
        state.list.push({
          aggregator: action.payload.aggregator,
          date: action.payload.date,
          network: action.payload.network,
          pools: action.payload.pools,
        });
      }
    }),
});

export const updatePoolsForAggregator = createAsyncThunk(
  "pools/updateForAggregator",
  (aggregator, thunkAPI) => {
    const state = thunkAPI.getState();
    const aggregatorPools = poolsForAggregator(state.pools, aggregator);
    const now = Date.now();

    const millisecondsSinceLastRefresh = now - (aggregatorPools?.date ?? 0);

    if (millisecondsSinceLastRefresh > cacheRefreshIntervalInMilliseconds) {
      return aggregator_api.fetchSignersTickers(aggregator).then((data) => {
        return {
          aggregator: aggregator,
          date: now,
          network: data.network,
          pools: data.signers ?? [],
        };
      });
    }

    return { keep_cached_data: true };
  },
);

const poolsForAggregator = (poolsSlice, aggregator) => {
  return poolsSlice.list.find((poolsData) => poolsData.aggregator === aggregator);
};

export const getPoolForSelectedAggregator = createSelector(
  [(state) => state.settings.selectedAggregator, (state) => state.pools, (state, poolId) => poolId],
  (aggregator, pools, poolId) => {
    const aggregatorPools = poolsForAggregator(pools, aggregator);
    const data = aggregatorPools?.pools.find((pool) => pool.party_id === poolId);

    return {
      network: aggregatorPools?.network,
      ...data,
    };
  },
);

export const getSelectedAggregatorPools = createSelector(
  [(state) => state.settings.selectedAggregator, (state) => state.pools],
  (aggregator, pools) => {
    return poolsForAggregator(pools, aggregator);
  },
);

export const getSelectedAggregatorNetwork = createSelector(
  [(state) => state.settings.selectedAggregator, (state) => state.pools],
  (aggregator, pools) => {
    const aggregatorPools = poolsForAggregator(pools, aggregator);
    return aggregatorPools?.network;
  },
);

export default poolsSlice.reducer;
