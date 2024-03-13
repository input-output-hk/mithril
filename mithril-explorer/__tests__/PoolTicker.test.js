import { render, screen } from "@testing-library/react";
import "@testing-library/jest-dom";
import { initStore } from "./helpers";
import { Provider } from "react-redux";
import PoolTicker from "#/PoolTicker";
import { settingsSlice } from "@/store/settingsSlice";
import { poolsSlice } from "@/store/poolsSlice";
import { getCExplorerUrl } from "@/utils";

function renderPoolTickerComponent(partyId, default_state = undefined) {
  const store = initStore(default_state);
  return [
    render(
      <Provider store={store}>
        <PoolTicker partyId={partyId} />
      </Provider>,
    ),
    store,
  ];
}

function getCExplorerUrlForPool(network, partyId) {
  const cExplorerUrl = getCExplorerUrl(network);
  return cExplorerUrl ? `${cExplorerUrl}/pool/${partyId}` : undefined;
}

describe("PoolTicker", () => {
  it("Pool ticker not on the three main network doesn't show link to cexplorer", () => {
    const partyId = "pool1zmtm8yef33z2n7x4nn0kvv9xpzjuj7725p9y9m5t960g5qy51ua";
    const poolTicker = "[MITHRIL] Mithril Signer";
    renderPoolTickerComponent(partyId, {
      settings: {
        ...settingsSlice.getInitialState(),
        selectedAggregator: "myaggregator",
      },
      pools: {
        ...poolsSlice.getInitialState(),
        list: [
          {
            aggregator: "myaggregator",
            network: "devnet",
            pools: [
              {
                party_id: partyId,
                pool_ticker: poolTicker,
                has_registered: true,
              },
            ],
          },
        ],
      },
    });

    expect(screen.getByText(poolTicker));
    expect(screen.queryByRole("link")).toBe(null);
  });

  it.each(["mainnet", "preprod", "preview"])(
    "Pool ticker on %s network link to cexplorer",
    (network) => {
      const partyId = "pool1zmtm8yef33z2n7x4nn0kvv9xpzjuj7725p9y9m5t960g5qy51ua";
      const poolTicker = "[MITHRIL] Mithril Signer";
      renderPoolTickerComponent(partyId, {
        settings: {
          ...settingsSlice.getInitialState(),
          selectedAggregator: "myaggregator",
        },
        pools: {
          ...poolsSlice.getInitialState(),
          list: [
            {
              aggregator: "myaggregator",
              network: network,
              pools: [
                {
                  party_id: partyId,
                  pool_ticker: poolTicker,
                  has_registered: true,
                },
              ],
            },
          ],
        },
      });

      expect(screen.getByText(poolTicker));
      expect(screen.getByRole("link")).toHaveAttribute(
        "href",
        getCExplorerUrlForPool(network, partyId),
      );
    },
  );

  it.each(["mainnet", "preprod", "preview"])(
    "Not available Pool ticker on %s network still show link to cexplorer",
    (network) => {
      const partyId = "pool1zmtm8yef33z2n7x4nn0kvv9xpzjuj7725p9y9m5t960g5qy51ua";
      renderPoolTickerComponent(partyId, {
        settings: {
          ...settingsSlice.getInitialState(),
          selectedAggregator: "myaggregator",
        },
        pools: {
          ...poolsSlice.getInitialState(),
          list: [
            {
              aggregator: "myaggregator",
              network: network,
              pools: [
                {
                  party_id: partyId,
                  has_registered: true,
                },
              ],
            },
          ],
        },
      });

      expect(screen.getByText("Not available"));
      expect(screen.getByRole("link")).toHaveAttribute(
        "href",
        getCExplorerUrlForPool(network, partyId),
      );
    },
  );

  it.each(["mainnet", "preprod", "preview"])(
    "Not available Pool ticker on %s network still show link to cexplorer even without pools data",
    (network) => {
      const partyId = "pool1zmtm8yef33z2n7x4nn0kvv9xpzjuj7725p9y9m5t960g5qy51ua";
      renderPoolTickerComponent(partyId, {
        settings: {
          ...settingsSlice.getInitialState(),
          selectedAggregator: "myaggregator",
        },
        pools: {
          ...poolsSlice.getInitialState(),
          list: [
            {
              aggregator: "myaggregator",
              network: network,
              pools: [],
            },
          ],
        },
      });

      expect(screen.getByText("Not available"));
      expect(screen.getByRole("link")).toHaveAttribute(
        "href",
        getCExplorerUrlForPool(network, partyId),
      );
    },
  );
});
