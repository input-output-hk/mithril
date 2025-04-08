const { defaultAggregatorCapabilities } = require("./constants");

function fetchAggregator(aggregator) {
  return fetch(aggregator, {
    headers: {
      "mithril-origin-tag": "EXPLORER",
    },
  });
}

function fetchAggregatorCapabilities(aggregator) {
  return fetchAggregator(aggregator)
    .then((response) => (response.status === 200 ? response.json() : {}))
    .then((data) => data?.capabilities ?? defaultAggregatorCapabilities)
    .catch((error) => {
      console.error("Fetch aggregator capabilities error:", error);
      return defaultAggregatorCapabilities;
    });
}

function fetchSignersTickers(aggregator) {
  return fetchAggregator(`${aggregator}/signers/tickers`)
    .then((response) => (response.status === 200 ? response.json() : {}))
    .catch((error) => {
      console.error("Fetch signers tickers error:", error);
    });
}

function fetchEpochSettings(aggregator) {
  return fetchAggregator(`${aggregator}/epoch-settings`)
    .then((response) => (response.status === 200 ? response.json() : {}))
    .catch((error) => {
      console.error("Fetch epoch settings error:", error);
    });
}

function fetchRegistrations(aggregator, epoch) {
  return fetchAggregator(`${aggregator}/signers/registered/${epoch}`)
    .then((response) => (response.status === 200 ? response.json() : {}))
    .catch((error) => {
      console.error("Fetch registrations error:", error);
    });
}

module.exports = {
  fetchAggregator,
  fetchAggregatorCapabilities,
  fetchSignersTickers,
  fetchEpochSettings,
  fetchRegistrations,
};
