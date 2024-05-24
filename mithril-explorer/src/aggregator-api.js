const { defaultAggregatorCapabilities } = require("./constants");

function fetchAggregatorCapabilities(aggregator) {
  return fetch(aggregator)
    .then((response) => (response.status === 200 ? response.json() : {}))
    .then((data) => data?.capabilities ?? defaultAggregatorCapabilities)
    .catch((error) => {
      console.error("Fetch aggregator capabilities error:", error);
      return defaultAggregatorCapabilities;
    });
}

function fetchSignersTickers(aggregator) {
  return fetch(`${aggregator}/signers/tickers`)
    .then((response) => (response.status === 200 ? response.json() : {}))
    .catch((error) => {
      console.error("Fetch signers tickers error:", error);
    });
}

function fetchEpochSettings(aggregator) {
  return fetch(`${aggregator}/epoch-settings`)
    .then((response) => (response.status === 200 ? response.json() : {}))
    .catch((error) => {
      console.error("Fetch epoch settings error:", error);
    });
}

function fetchRegistrations(aggregator, epoch) {
  return fetch(`${aggregator}/signers/registered/${epoch}`)
    .then((response) => (response.status === 200 ? response.json() : {}))
    .catch((error) => {
      console.error("Fetch registrations error:", error);
    });
}

module.exports = {
  fetchAggregatorCapabilities,
  fetchSignersTickers,
  fetchEpochSettings,
  fetchRegistrations,
};
