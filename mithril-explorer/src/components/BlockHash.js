import React from "react";
import { useSelector } from "react-redux";
import { getSelectedAggregatorNetwork } from "@/store/poolsSlice";
import { getCExplorerUrl } from "@/utils";
import { CExplorerUrl } from "#/CExplorerUrl";

export default function BlockHash({ hash }) {
  const network = useSelector((state) => getSelectedAggregatorNetwork(state));
  const cExplorerUrl = getCExplorerUrl(network);
  const url = cExplorerUrl ? `${cExplorerUrl}/block/${hash}` : undefined;

  return <CExplorerUrl url={url} text={hash} textClass="text-break" showCopyButton />;
}
