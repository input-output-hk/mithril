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

module.exports = {
  fetchAggregatorCapabilities,
};
