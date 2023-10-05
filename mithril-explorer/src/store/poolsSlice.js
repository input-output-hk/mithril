import {createAsyncThunk, createSelector, createSlice} from "@reduxjs/toolkit";

export const poolsSlice = createSlice({
    name: 'pools',
    initialState: { list: [] },
    reducers: {},
    extraReducers: builder => builder.addCase(updatePoolsForAggregator.fulfilled, (state, action) => {
        let existing = poolsForAggregator(state, action.payload.aggregator);

        if (existing) {
            existing.date = action.payload.date;
            existing.pools = action.payload.pools;
        } else {
            state.list.push({
                aggregator: action.payload.aggregator,
                date: action.payload.date,
                network: action.payload.network,
                pools: action.payload.pools,
            });
        }
    })
});

export const updatePoolsForAggregator = createAsyncThunk('pools/updateForAggregator', aggregator => {
    return fetch(`${aggregator}/signers/tickers`)
        .then(response => response.status === 200 ? response.json() : {})
        .then(data => {
            return {
                aggregator: aggregator,
                date: Date.now(),
                network: data.network,
                pools: data.signers,
            };
        });
});

const poolsForAggregator = (state, aggregator) => {
    return state.list.find(poolsData => poolsData.aggregator === aggregator);
};

export const getPool = createSelector([
        state => state.pools,
        (state, aggregator, poolId) => {
    return {
        aggregator: aggregator,
        poolId: poolId
    }},
    ],
    (pools, args) => {
    const aggregator = poolsForAggregator(pools, args.aggregator);
    const data = aggregator?.pools.find(pool => pool.party_id === args.poolId);

    return {
        network: aggregator?.network,
        ...data,
    };
});

export default poolsSlice.reducer;