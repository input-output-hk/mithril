import { translate } from "@docusaurus/Translate";

export const hero = {
  title: translate({
    id: "homepage.hero.title",
    message: "Power Cardano's Light Clients.",
  }),
  standfirst: translate({
    id: "homepage.hero.standfirst",
    message: `Mithril enables lightweight, trustless access to Cardano blockchain state. Built on stake-based threshold multisignatures, it lets clients verify chain data without running a full node.`,
  }),
};
