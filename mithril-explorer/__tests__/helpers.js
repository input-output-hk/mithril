import {saveToLocalStorage, storeBuilder} from "../store/store";

function initStore(default_state = undefined) {
  if (default_state) {
    saveToLocalStorage(default_state);
  }
  return storeBuilder();
}

module.exports = {
  initStore
}