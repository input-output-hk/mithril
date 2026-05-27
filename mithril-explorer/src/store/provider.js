"use client";

import { aggregatorSearchParam } from "@/constants";
import { saveToLocalStorage, storeBuilder } from "./store";
import { Provider } from "react-redux";
import { useSearchParams } from "next/navigation";
import { useState } from "react";

export function Providers({ children }) {
  const searchParams = useSearchParams();
  const initialAggregator = searchParams.get(aggregatorSearchParam);
  const initialAggregatorGenesisVerificationKey = searchParams.get("genesisVerificationKey");
  const [store] = useState(
    storeBuilder({
      url: initialAggregator,
      genesisVerificationKey: initialAggregatorGenesisVerificationKey ?? "",
    }),
  );

  store.subscribe(() => saveToLocalStorage(store.getState()));

  return <Provider store={store}>{children}</Provider>;
}
