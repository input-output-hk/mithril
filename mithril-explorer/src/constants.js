export const aggregatorSearchParam = "aggregator";

export const signedEntityType = {
  MithrilStakeDistribution: "MithrilStakeDistribution",
  CardanoStakeDistribution: "CardanoStakeDistribution",
  CardanoImmutableFilesFull: "CardanoImmutableFilesFull",
  CardanoTransactions: "CardanoTransactions",
};

export const defaultAggregatorCapabilities = {
  signed_entity_types: [],
  cardano_transactions_prover: {
    max_hashes_allowed_by_request: 100,
  },
};
