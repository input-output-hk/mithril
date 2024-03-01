import { defaultAggregatorCapabilities } from "./constants";

function fetchAggregatorCapabilities(aggregator) {
  return fetch(aggregator)
    .then((response) => (response.status === 200 ? response.json() : {}))
    .then((data) => data?.capabilities ?? defaultAggregatorCapabilities)
    .catch(() => {
      return {
        signed_entity_types: [],
      };
    });
}

function fetchSignersTickers(aggregator) {
  return fetch(`${aggregator}/signers/tickers`).then((response) =>
    response.status === 200 ? response.json() : {},
  );
}

module.exports = {
  fetchAggregatorCapabilities,
  fetchSignersTickers,
};
