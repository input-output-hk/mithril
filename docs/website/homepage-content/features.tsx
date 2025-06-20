import React from "react";
import { translate } from "@docusaurus/Translate";
import Security from "../src/components/icons/Security";
import Speed from "../src/components/icons/Speed";
import Verification from "../src/components/icons/Verification";
import Scalability from "../src/components/icons/Scalability";

export const FeatureList = [
  {
    title: translate({
      id: "homepage.featureList.security.title",
      message: "Secure data exchange",
    }),
    icon: <Security />,
    description: translate({
      id: "homepage.featureList.security.description",
      message:
        "Fast and secure data synchronization with layer 2 solutions – including bridges, sidechains, and rollups – as well as applications like light wallets.",
    }),
  },
  {
    title: translate({
      id: "homepage.featureList.speed.title",
      message: "Faster node bootstrapping",
    }),
    icon: <Speed />,
    description: translate({
      id: "homepage.featureList.speed.description",
      message:
        "Mithril's certified snapshots allow nodes to bootstrap quickly while maintaining the same level of security.",
    }),
  },
  {
    title: translate({
      id: "homepage.featureList.verification.title",
      message: "Trustless state verification",
    }),
    icon: <Verification />,
    description: translate({
      id: "homepage.featureList.verification.description",
      message:
        "Blockchain data is validated and certified by a minimum share of the stake, enabling secure data exchange between decentralized applications.",
    }),
  },
  {
    title: translate({
      id: "homepage.featureList.scalability.title",
      message: "Scalability",
    }),
    icon: <Scalability />,
    description: translate({
      id: "homepage.featureList.scalability.description",
      message:
        "Stake-based threshold multi-signatures efficiently aggregate individual signatures, ensuring scalability even as the number of participants grows.",
    }),
  },
];
