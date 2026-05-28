"use client";

import { aggregatorSearchParam } from "@/constants";
import { getPreloadedStateFromLocalStorage, saveToLocalStorage, storeBuilder } from "./store";
import { Provider } from "react-redux";
import { useSearchParams } from "next/navigation";
import { useEffect, useState } from "react";

export function Providers({ children, fallback }) {
  const [hydrated, setHydrated] = useState(false);
  const searchParams = useSearchParams();

  const [store, setStore] = useState(undefined);

  useEffect(() => {
    function loadStoreFromLocalStorage(initialAggregator, initialAggregatorGenesisVerificationKey) {
      const initialState = getPreloadedStateFromLocalStorage({
        url: initialAggregator,
        genesisVerificationKey: initialAggregatorGenesisVerificationKey ?? "",
      });
      const store = storeBuilder(initialState);
      store.subscribe(() => saveToLocalStorage(store.getState()));

      setStore(store);
      setHydrated(true);
    }

    if (!hydrated) {
      const initialAggregator = searchParams.get(aggregatorSearchParam);
      const initialAggregatorGenesisVerificationKey = searchParams.get("genesisVerificationKey");
      loadStoreFromLocalStorage(initialAggregator, initialAggregatorGenesisVerificationKey);
    }
  }, [hydrated, searchParams]);

  return hydrated ? <Provider store={store}>{children}</Provider> : <>{fallback}</>;
}
