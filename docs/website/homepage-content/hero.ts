import { translate } from "@docusaurus/Translate";

export const hero = {
  title: translate({
    id: "homepage.hero.title",
    message: "Powering Cardano state proofs.",
  }),
  standfirst: translate({
    id: "homepage.hero.standfirst",
    message: `Mithril enables lightweight, trustless access to the Cardano blockchain state. Built on stake-based threshold multisignatures, it lets clients verify chain data without running a full node.`,
  }),
};
