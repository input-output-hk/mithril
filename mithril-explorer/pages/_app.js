import 'bootstrap/dist/css/bootstrap.min.css';
import 'bootstrap-icons/font/bootstrap-icons.css';
import '../styles/globals.css'
import dynamic from "next/dynamic";
import Script from "next/script";
import {Provider} from "react-redux";
import {saveToLocalStorage, storeWrapper} from "../store/store";

function MithrilExplorer({Component, ...rest}) {
  const {store, pageProps} = storeWrapper.useWrappedStore(rest);
  store.subscribe(() => saveToLocalStorage(store.getState()));

  return (
    <>
      <Script id="plausible"
              strategy="afterInteractive"
              src="https://plausible.io/js/script.js"
              data-domain="mithril.network"/>
      <Provider store={store}>
        <Component {...pageProps} />
      </Provider>
    </>
  );
}

// Disable Server Side Rendering, this as SEO implication since the site won't be rendered by bots without
// them running client javascript.
// If enabled this leads to NextJs/React hydration errors since the server side rendered page won't
// have access to the saved local storage settings.
export default dynamic(() => Promise.resolve(MithrilExplorer), {
  ssr: false
});
