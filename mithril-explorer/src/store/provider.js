"use client";

import {saveToLocalStorage, storeBuilder} from "./store";
import {Provider} from "react-redux";
import {useSearchParams} from "next/navigation";
import {useState} from "react";

export function Providers({children}) {
  const searchParams = useSearchParams();
  const initialAggregator = searchParams.get('aggregator');
  const [store, initializeStore] = useState(storeBuilder(initialAggregator));

  store.subscribe(() => saveToLocalStorage(store.getState()));

  return <Provider store={store}>{children}</Provider>;
}
