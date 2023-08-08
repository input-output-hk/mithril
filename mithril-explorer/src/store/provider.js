"use client";

import {saveToLocalStorage, storeBuilder} from "./store";
import {Provider} from "react-redux";

export function Providers({ children }) {
  let store = storeBuilder();
  store.subscribe(() => saveToLocalStorage(store.getState()));
  
  return <Provider store={store}>{children}</Provider>;
}
