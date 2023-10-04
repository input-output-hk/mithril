import React from "react";
import {useSelector} from "react-redux";
import {getPool} from "../store/poolsSlice";

export default function PoolTicker({aggregator, partyId, ...props}) {
    const poolTicker = useSelector((state) => getPool(state, aggregator, partyId).pool_ticker);

    return <>{poolTicker}</>;
}

