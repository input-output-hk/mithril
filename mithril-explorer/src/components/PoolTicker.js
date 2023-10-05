import React, {useEffect, useState} from "react";
import {useSelector} from "react-redux";
import {getPool} from "../store/poolsSlice";
import {getCExplorerUrlForPool} from "../utils";
import Image from "next/image";
import {OverlayTrigger, Tooltip} from "react-bootstrap";

export default function PoolTicker({aggregator, partyId, ...props}) {
    const pool = useSelector((state) => getPool(state, aggregator, partyId));
    const [url, setUrl] = useState(undefined);

    useEffect(() => {
        if (pool?.network) {
            setUrl(getCExplorerUrlForPool(pool.network, partyId));
        } else {
            setUrl(undefined);
        }
    }, [partyId, pool.network])

    return (url !== undefined)
            ? <>
            <a href={url} target="_blank" className="link-dark link-underline-light">
                <OverlayTrigger overlay={<Tooltip>See in CExplorer</Tooltip>}>
                    <span>
                        <Image src="/explorer/cexplorer_logo.png"
                               alt="CExplorer Logo"
                               style={{verticalAlign:"text-top"}}
                               width={20} height={20}/>
                        <> </>
                        {pool.pool_ticker ?? "Not available"}
                    </span>
                </OverlayTrigger>
            </a>
        </>
            : <span>{pool.pool_ticker}</span>;
}
