import { saveToLocalStorage, storeBuilder } from "@/store/store";
import * as mockRouter from "next-router-mock";

const baseLocation = "http://localhost";

function initStore(default_state = undefined) {
  if (default_state) {
    saveToLocalStorage(default_state);
  }
  return storeBuilder();
}

const mockNextNavigation = {
  ...mockRouter,
  notFound: jest.fn(),
  redirect: jest.fn().mockImplementation((url) => {
    mockRouter.memoryRouter.setCurrentUrl(url);
  }),
  usePathname: () => {
    const router = mockRouter.useRouter();
    return router.asPath;
  },
  useSearchParams: () => {
    const router = mockRouter.useRouter();
    return new URLSearchParams(router.query);
  },
};

/**
 * Reset the windows location api to `http://localhost`
 */
function resetLocation() {
  setLocation(new URL(baseLocation));
  mockRouter.memoryRouter.setCurrentUrl("/");
}

/**
 * Set the window.location to the given url
 *
 * If you use it define a beforeEach with resetLocation else the new location will persist between tests.
 * @param url The new location
 */
function setLocation(url) {
  Object.defineProperty(window, "location", {
    set(v) {
      this._href = v;
    },
    get() {
      return this._href;
    },
  });

  window.location = url;
}

/**
 * Set the window.location search/query aggregator param to the given aggregator
 *
 * If you use it define a beforeEach with resetLocation else the new location will persist between tests.
 * @param aggregatorUrl The target aggregator
 */
function setLocationToAggregator(aggregatorUrl) {
  setLocation(new URL(`?aggregator=${aggregatorUrl}`, baseLocation));
}

/**
 * Helper function to create a signer registration as returned in the `signers/registered/{epoch}` API
 */
function reg(party_id, stake) {
  return { party_id, stake };
}

module.exports = {
  initStore,
  mockNextNavigation,
  setLocation,
  setLocationToAggregator,
  resetLocation,
  reg,
};
