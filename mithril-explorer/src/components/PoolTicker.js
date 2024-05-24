import React from "react";
import { useSelector } from "react-redux";
import { getPoolForSelectedAggregator } from "@/store/poolsSlice";
import { poolTickerCExplorerUrl } from "@/utils";
import { CExplorerUrl } from "#/CExplorerUrl";

export default function PoolTicker({ partyId }) {
  const pool = useSelector((state) => getPoolForSelectedAggregator(state, partyId));
  const cExplorerUrl = poolTickerCExplorerUrl(pool.network, partyId);

  return <CExplorerUrl url={cExplorerUrl} text={pool?.pool_ticker} />;
}
