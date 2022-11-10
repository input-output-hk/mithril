import {saveToLocalStorage, storeBuilder} from "../store/store";

const baseLocation = 'http://localhost';

function initStore(default_state = undefined) {
  if (default_state) {
    saveToLocalStorage(default_state);
  }
  return storeBuilder();
}

/**
 * Reset the windows location api to `http://localhost`
 */
function resetLocation() {
  setLocation(new URL(baseLocation));
}

/**
 * Set the window.location to the given url
 *
 * If you use it define a beforeEach with resetLocation else the new location will persist between tests.
 * @param url The new location
 */
function setLocation(url) {
  Object.defineProperty(window, 'location', {
    set(v) {
      this._href = v;
    },
    get() {
      return this._href;
    }
  })

  window.location = url;
}

/**
 * Set the window.location search/query aggregator param to the given aggregator
 * 
 * If you use it define a beforeEach with resetLocation else the new location will persist between tests.
 * @param aggregatorUrl The target aggregator
 */
function setLocationToAggregator(aggregatorUrl) {
  setLocation(new URL(`?aggregator=${aggregatorUrl}`, baseLocation));
}

module.exports = {
  initStore,
  setLocation,
  setLocationToAggregator,
  resetLocation,
}