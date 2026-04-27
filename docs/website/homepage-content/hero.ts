import { translate } from "@docusaurus/Translate";

export const hero = {
  title: translate({
    id: "homepage.hero.title",
    message: "Powering Cardano state proofs",
  }),
  standfirst: translate({
    id: "homepage.hero.standfirst",
    message: `Built on stake-based threshold multisignatures, Mithril lets clients verify Cardano chain data without running a full node — ideal for wallets, exchanges, bridges, and any app that needs verified chain data with minimal overhead.`,
  }),
};
