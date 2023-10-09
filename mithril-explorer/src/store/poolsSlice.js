import { createAsyncThunk, createSelector, createSlice } from "@reduxjs/toolkit";

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
      return fetch(`${aggregator}/signers/tickers`)
        .then((response) => (response.status === 200 ? response.json() : {}))
        .then((data) => {
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

export const getPool = createSelector(
  [
    (state) => state.pools,
    (state, aggregator, poolId) => ({ aggregator: aggregator, poolId: poolId }),
  ],
  (pools, args) => {
    const aggregator = poolsForAggregator(pools, args.aggregator);
    const data = aggregator?.pools.find((pool) => pool.party_id === args.poolId);

    return {
      network: aggregator?.network,
      ...data,
    };
  },
);

export default poolsSlice.reducer;
