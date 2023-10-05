import {createAsyncThunk, createSelector, createSlice} from "@reduxjs/toolkit";

const CACHE_NAME = "v1";
// Delete all caches older than 6 hours
const cacheRefreshIntervalInMilliseconds = 6 * 3600 * 1000;

export const poolsSlice = createSlice({
    name: 'pools',
    initialState: { list: [] },
    reducers: {
        refreshPoolsCaches: (state) => {
            const now = Date.now();
            for (const entry of state.list.map(p => ({date: p.date, url: poolTickersUrl(p.aggregator)}))) {
                const millisecondsSinceLastRefresh = now - entry.date;

                if (millisecondsSinceLastRefresh > cacheRefreshIntervalInMilliseconds) {
                    caches.open(CACHE_NAME).then(async (cache) => {
                        const deletionSuccessful = await cache.delete(entry.url);
                        if (!deletionSuccessful) {
                            console.error(`Failed to delete cache: ${entry.url}`);
                        }
                    });
                }
            }
        }
    },
    extraReducers: builder => builder.addCase(updatePoolsForAggregator.fulfilled, (state, action) => {
        let existing = poolsForAggregator(state, action.payload.aggregator);

        if (existing) {
            existing.network = action.payload.network;
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

export const {
    refreshPoolsCaches
} = poolsSlice.actions;

const poolTickersUrl = aggregator => `${aggregator}/signers/tickers`;

export const updatePoolsForAggregator = createAsyncThunk('pools/updateForAggregator', aggregator => {
    const url = poolTickersUrl(aggregator);
    return caches.match(url)
        .then(cached => {
            // Cache hit
            if (cached) {
                return cached;
            }

            // Cache miss
            return fetch(url).then(response => {
                if (response.status === 200) {
                    caches.open(CACHE_NAME).then(async (cache) => {
                        const putSuccessful = await cache.put(url, response);
                        if (!putSuccessful) {
                            console.error(`Failed to put cache: ${url}`);
                        }
                    });
                }
                return response.clone();
            })
        })
        .then(response => response.status === 200 ? response.json() : {})
        .then(data => {
            return {
                aggregator: aggregator,
                date: Date.now(),
                network: data.network,
                pools: data.signers ?? [],
            };
        });
});

const poolsForAggregator = createSelector( [
        state => state.pools,
        (state, aggregator) => aggregator
    ],
    (pools, aggregator) => {
        return pools.list.find(poolsData => poolsData.aggregator === aggregator);
    }
);

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