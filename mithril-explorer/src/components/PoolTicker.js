import React, { useEffect, useState } from "react";
import { useSelector } from "react-redux";
import { getPoolForSelectedAggregator } from "../store/poolsSlice";
import { getCExplorerUrlForPool } from "../utils";
import Image from "next/image";
import { OverlayTrigger, Tooltip } from "react-bootstrap";

export default function PoolTicker({ partyId }) {
  const pool = useSelector((state) => getPoolForSelectedAggregator(state, partyId));
  const [url, setUrl] = useState(undefined);

  useEffect(() => {
    setUrl(getCExplorerUrlForPool(pool?.network, partyId));
  }, [pool.network, partyId]);

  return url !== undefined ? (
    <>
      <a href={url} target="_blank" className="link-dark link-underline-light">
        <OverlayTrigger overlay={<Tooltip>See in CExplorer</Tooltip>}>
          <span>
            <Image
              src="/explorer/cexplorer_logo.png"
              alt="CExplorer Logo"
              style={{ verticalAlign: "text-top" }}
              width={20}
              height={20}
            />
            <> </>
            {pool?.pool_ticker ?? "Not available"}
          </span>
        </OverlayTrigger>
      </a>
    </>
  ) : (
    <span>{pool?.pool_ticker}</span>
  );
}
