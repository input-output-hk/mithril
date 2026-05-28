"use client";

import { aggregatorSearchParam } from "@/constants";
import {
  getPreloadedStateFromLocalStorage,
  saveToLocalStorage,
  storeBuilder,
  getEmptyPreloadedState,
} from "./store";
import { Provider } from "react-redux";
import { useSearchParams } from "next/navigation";
import { useEffect, useState } from "react";

export function Providers({ children }) {
  const searchParams = useSearchParams();
  const initialAggregator = searchParams.get(aggregatorSearchParam);
  const initialAggregatorGenesisVerificationKey = searchParams.get("genesisVerificationKey");
  const initialState = getPreloadedStateFromLocalStorage({
    url: initialAggregator,
    genesisVerificationKey: initialAggregatorGenesisVerificationKey ?? "",
  });

  const [store] = useState(storeBuilder(initialState));

  useEffect(() => {
    return store.subscribe(() => saveToLocalStorage(store.getState()));
  }, [store]);

  return (
    <Provider store={store} serverState={getEmptyPreloadedState()}>
      {children}
    </Provider>
  );
}
