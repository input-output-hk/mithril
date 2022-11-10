import { render, screen } from '@testing-library/react'
import '@testing-library/jest-dom'
import AggregatorSetter from "../components/AggregatorSetter";
import {initStore} from "./helpers";
import {Provider} from "react-redux";
import default_available_aggregators from "../aggregators-list";
import {settingsSlice} from "../store/settingsSlice";

function renderAggregatorSetter(default_state = undefined) {
  const store = initStore(default_state);
  return [
    render(
      <Provider store={store}>
        <AggregatorSetter/>
      </Provider>
    ),
    store
  ];
}

describe('AggregatorSetter', () => {
  it ('Load with data from the store', () => {
    const [_, store] = renderAggregatorSetter();
    const settingsState = store.getState().settings;
    
    expect(screen.getByRole('option', { name: settingsState.selectedAggregator }).selected).toBe(true);
    expect(screen.getAllByRole('option').map(o => o.value)).toEqual(settingsState.availableAggregators);
  });
  
  it ('Load custom aggregators', () => {
    const customAggregator = "http://aggregator.test";
    renderAggregatorSetter({
      settings: {
        ...settingsSlice.getInitialState(),
        selectedAggregator: customAggregator,
        availableAggregators: [...default_available_aggregators, customAggregator],
      }
    });

    expect(screen.getByRole('option', { name: customAggregator }).selected).toBe(true);
    expect(screen.getAllByRole('option').map(o => o.value)).toContain(customAggregator);
  });
});