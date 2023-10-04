import {createAsyncThunk, createSlice} from "@reduxjs/toolkit";
import {selectedAggregator} from "./settingsSlice";

export const poolsSlice = createSlice({
    name: 'pools',
    initialState: {},
    reducers: {},
    extraReducers: builder => builder.addCase(updatePoolsForAggregator.fulfilled, (state, action) => {
        state[action.payload.aggregator] = {
            date: action.payload.date,
            network: action.payload.network,
            pools: action.payload.pools,
        };
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

export default poolsSlice.reducer;