import { translate } from "@docusaurus/Translate";

export const useCases = {
  nonScrollingText: translate({
    id: "homepage.useCases.nonScrollingText",
    message: "Use cases include:",
  }),
  scrollingText: {
    id: "homepage.useCases.scrollingText",
    items: [
      "Full node clients",
      "Light clients",
      "Light wallets",
      "Mobile applications",
      "Sidechains",
      "Bridges",
      "Rollups",
      "State channels",
    ],
  },
};
