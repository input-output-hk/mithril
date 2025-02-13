"use client";
/*
 * Helpers for the wasm client.
 *
 * *IMPORTANT*: Those functions should never be imported directly using `import` since this
 * may cause the wasm client to be included in SSR builds, which will fail with the following error:
 * ```
 * Error: ENOENT: no such file or directory, open '/mithril/mithril-explorer/.next/server/app/mithril_client_wasm_bg.wasm'
 * ```
 * Instead, use `require` to import these functions:
 * ```js
 * const { newMithrilWasmClient, fetchGenesisVerificationKey } = require("@/wasm-client-helpers");
 * ```
 */

import { MithrilClient } from "@mithril-dev/mithril-client-wasm";
import { computeAggregatorNetworkFromUrl } from "@/utils";

async function newMithrilWasmClient(aggregator, genesisVerificationKey) {
  const isCacheEnabled = process.env.UNSTABLE === true;
  const client_options = process.env.UNSTABLE
    ? {
        // The following option activates the unstable features of the client.
        // Unstable features will trigger an error if this option is not set.
        unstable: true,
        enable_certificate_chain_verification_cache: isCacheEnabled,
      }
    : {};

  return new MithrilClient(aggregator, genesisVerificationKey, client_options);
}

async function fetchGenesisVerificationKey(aggregator) {
  const network = computeAggregatorNetworkFromUrl(aggregator);
  return fetch(
    `https://raw.githubusercontent.com/input-output-hk/mithril/main/mithril-infra/configuration/${network}/genesis.vkey`,
  )
    .then((res) => res.text())
    .catch((err) => console.error("Error fetching genesis verification key:", err));
}

module.exports = {
  newMithrilWasmClient,
  fetchGenesisVerificationKey,
};
