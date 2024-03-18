import React from "react";
import { useSelector } from "react-redux";
import { getPoolForSelectedAggregator } from "@/store/poolsSlice";
import { getCExplorerUrl } from "@/utils";
import { CExplorerUrl } from "#/CExplorerUrl";

export default function PoolTicker({ partyId }) {
  const pool = useSelector((state) => getPoolForSelectedAggregator(state, partyId));
  const cExplorerUrl = getCExplorerUrl(pool.network);
  const url = cExplorerUrl ? `${cExplorerUrl}/pool/${partyId}` : undefined;

  return <CExplorerUrl url={url} text={pool?.pool_ticker} />;
}
