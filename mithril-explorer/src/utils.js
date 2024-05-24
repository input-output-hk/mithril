function checkUrl(url) {
  try {
    // Use the url constructor to check if the value is an url
    return Boolean(new URL(url));
  } catch (ex) {
    return false;
  }
}

const toAda = (lovelace) => lovelace / 1000000;

const formatCurrency = (number, maximumFractionDigits = 2) =>
  number.toLocaleString(undefined, {
    maximumFractionDigits: maximumFractionDigits,
  });

function formatPartyId(partyId) {
  if ((typeof partyId === "string" || partyId instanceof String) && partyId.length > 15) {
    return partyId.slice(0, 10) + "…" + partyId.slice(-5);
  } else {
    return partyId;
  }
}

function formatStake(lovelace) {
  // Credits to Jasser Mark Arioste for the original idea:
  // https://reacthustle.com/blog/how-to-convert-number-to-kmb-format-in-javascript
  const thresholds = [
    { suffix: "B", value: 1e9 },
    { suffix: "M", value: 1e6 },
    { suffix: "K", value: 1e3 },
    { suffix: "", value: 1 },
  ];
  const ada = toAda(lovelace);
  // Note: subtracting 0.001 to handle cases like `999,999₳` rounding up to `1,000₳` after string format.
  const threshold = thresholds.find((t) => Math.abs(ada) >= t.value - 0.001);

  if (threshold) {
    return `${formatCurrency(ada / threshold.value)}${threshold.suffix}₳`;
  }

  return `${formatCurrency(ada)}₳`;
}

/*
 * Code from: https://stackoverflow.com/a/18650828
 */
function formatBytes(bytes, decimals = 2) {
  if (bytes === 0) return "0 Bytes";

  const k = 1024;
  const dm = decimals < 0 ? 0 : decimals;
  const sizes = ["Bytes", "KiB", "MiB", "GiB", "TiB", "PiB", "EiB", "ZiB", "YiB"];

  const i = Math.floor(Math.log(bytes) / Math.log(k));

  return parseFloat((bytes / Math.pow(k, i)).toFixed(dm)) + " " + sizes[i];
}

function getCExplorerUrl(network) {
  const urlWithoutNetwork = "cexplorer.io";
  let url = undefined;

  switch (network) {
    case "mainnet":
      url = `https://${urlWithoutNetwork}`;
      break;
    case "preprod":
      url = `https://preprod.${urlWithoutNetwork}`;
      break;
    case "preview":
      url = `https://preview.${urlWithoutNetwork}`;
      break;
    case "sanchonet":
      url = `https://sancho.${urlWithoutNetwork}`;
      break;
    default:
      break;
  }

  return url;
}

function poolTickerCExplorerUrl(network, partyId) {
  const cExplorerUrl = getCExplorerUrl(network);
  return cExplorerUrl ? `${cExplorerUrl}/pool/${partyId}` : undefined;
}

function formatProcessDuration(startTime) {
  const duration = performance.now() - startTime;
  const minutes = Math.floor(duration / 60000);
  const seconds = Math.floor((duration % 60000) / 1000);
  return minutes > 0 ? `${minutes} minutes and ${seconds} seconds` : `${seconds} seconds`;
}

function computeAggregatorNetworkFromUrl(aggregatorUrl) {
  const network = aggregatorUrl.match(/aggregator\.(.*?)\.api/);
  return network && network[1] ? network[1] : null;
}

async function fetchGenesisVerificationKey(aggregator) {
  const network = computeAggregatorNetworkFromUrl(aggregator);
  return fetch(
    `https://raw.githubusercontent.com/input-output-hk/mithril/main/mithril-infra/configuration/${network}/genesis.vkey`,
  )
    .then((res) => res.text())
    .catch((err) => console.error("Error fetching genesis verification key:", err));
}

/**
 * Compute the in and out registrations for given epochs.
 *
 * All registrations are expected to use the following format:
 * `{ registered_at: number, registrations: [{ party_id: string, stake: number }] }`;
 */
function computeInOutRegistrations(lastEpochRegistrations, ...previousEpochsRegistrations) {
  const result = {};
  let currentRegistrations = lastEpochRegistrations.registrations;

  for (const { registered_at, registrations } of previousEpochsRegistrations) {
    const epochOfChange = registered_at + 1;
    const diff = compareRegistrations(currentRegistrations, registrations);

    if (diff.in.length > 0 || diff.out.length > 0) {
      result[epochOfChange] = { in: diff.in, out: diff.out };
    }
    currentRegistrations = registrations;
  }

  return result;
}

/**
 * Take the result of `computeInOutRegistrations` and remove any duplicate in/out registrations.
 *
 * Duplicate are defined as parties that are in both the 'in' and 'out' registrations, only the
 * latest state should be kept.
 * IE: if a party is defined as 'out' in epoch 3 but is back 'in' some epoch later, it will be removed
 * (same if it was 'out' in epoch 2 and back 'in' in epoch 3).
 */
function dedupInOutRegistrations(inOutRegistrations) {
  const result = {};
  const previouslyIn = [];
  const previouslyOut = [];

  // Object.entries return alphanumeric ordered keys, so we reverse the order to start from the latest epoch
  for (const [epoch, registrations] of Object.entries(inOutRegistrations).reverse()) {
    const epochRegistrations = {
      in: registrations.in.filter((r) => !previouslyOut.includes(r.party_id)),
      out: registrations.out.filter((r) => !previouslyIn.includes(r.party_id)),
    };

    previouslyOut.push(...registrations.out.map((r) => r.party_id));
    previouslyIn.push(...registrations.in.map((r) => r.party_id));

    if (epochRegistrations.in.length > 0 || epochRegistrations.out.length > 0) {
      result[epoch] = epochRegistrations;
    }
  }

  return result;
}

/**
 * Return a list of in/out registrations between the given parameters.
 */
function compareRegistrations(left, right) {
  const left_party_ids = left.map((r) => r.party_id);
  const right_party_ids = right.map((r) => r.party_id);
  return {
    in: [...left.filter((l) => !right_party_ids.includes(l.party_id))],
    out: [...right.filter((l) => !left_party_ids.includes(l.party_id))],
  };
}

module.exports = {
  checkUrl,
  formatStake,
  toAda,
  formatCurrency,
  formatBytes,
  formatPartyId,
  getCExplorerUrl,
  poolTickerCExplorerUrl,
  formatProcessDuration,
  computeAggregatorNetworkFromUrl,
  fetchGenesisVerificationKey,
  computeInOutRegistrations,
  dedupInOutRegistrations,
  compareRegistrations,
};
