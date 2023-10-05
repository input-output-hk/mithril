import React, {useEffect, useState} from "react";
import {useSelector} from "react-redux";
import {getPool} from "../store/poolsSlice";
import {getCExplorerUrlForPool} from "../utils";

export default function PoolTicker({aggregator, partyId, ...props}) {
    const pool = useSelector((state) => getPool(state, aggregator, partyId));
    const [url, setUrl] = useState(undefined);

    useEffect(() => {
        if (pool?.network) {
            setUrl(getCExplorerUrlForPool(pool.network, pool.party_id));
        } else {
            setUrl(undefined);
        }
    }, [pool.party_id, pool.network])

    return (url !== undefined)
            ? <a href={url} target="_blank" className="link-dark link-underline-light">
            <span>{pool.pool_ticker}</span>
    </a>
            : <span>{pool.pool_ticker}</span>;
}
