const CUSTOM_AGGREGATORS_KEY = "CUSTOM_AGGREGATORS";

function init(args) {
  const selected = args[0];
  const defaultAvailableAggregators = args[1] ?? []
  let aggregators = [...defaultAvailableAggregators];

  if (typeof window !== 'undefined' && localStorage) {
    const storedAggregators = JSON.parse(localStorage.getItem(CUSTOM_AGGREGATORS_KEY));

    if (storedAggregators) {
      aggregators = aggregators.concat(storedAggregators);
    }
  }

  if (!aggregators.includes(selected)) {
    aggregators.push(selected);
    saveCustomAggregatorSources(aggregators, defaultAvailableAggregators);
  }

  return {
    availableAggregators: aggregators,
    defaultAvailableAggregators: defaultAvailableAggregators,
    canRemoveSelected: canRemoveSelected(selected, defaultAvailableAggregators),
  };
}

function canRemoveSelected(aggregator, defaultAvailableAggregators) {
  return !defaultAvailableAggregators.includes(aggregator);
}

function saveCustomAggregatorSources(aggregators, defaultAvailableAggregators) {
  if (typeof window !== 'undefined' && localStorage) {
    const customAggregators = aggregators.filter(a => !defaultAvailableAggregators.includes(a));
    localStorage.setItem(CUSTOM_AGGREGATORS_KEY, JSON.stringify(customAggregators));
  }
}

function reducer(state, action) {
  switch (action.type) {
    case 'aggregatorSelected':
      return {
        ...state,
        canRemoveSelected: canRemoveSelected(action.aggregator, state.defaultAvailableAggregators),
      };
    case 'addAggregator': {
      if (state.availableAggregators.includes(action.aggregator)) {
        return state;
      }

      const availableAggregators = [...state.availableAggregators, action.aggregator];
      saveCustomAggregatorSources(availableAggregators, state.defaultAvailableAggregators);
      return {
        ...state,
        availableAggregators: availableAggregators,
        canRemoveSelected: true
      };
    }
    case 'deleteSelected': {
      if (!canRemoveSelected(action.aggregator, state.defaultAvailableAggregators)) {
        return state;
      }
      
      const availableAggregators = state.availableAggregators.filter(a => a !== action.aggregator);
      saveCustomAggregatorSources(availableAggregators, state.defaultAvailableAggregators);
      return {
        ...state,
        availableAggregators: availableAggregators,
        canRemoveSelected: canRemoveSelected(availableAggregators.at(0), state.defaultAvailableAggregators),
      };
    }
    default:
      throw new Error("Unknown action");
  }
}

module.exports = {
  aggregatorListReducer: reducer,
  initAggregatorList: init
}