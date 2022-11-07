import 'bootstrap/dist/css/bootstrap.min.css';
import 'bootstrap-icons/font/bootstrap-icons.css';
import '../styles/globals.css'
import Script from "next/script";
import {Provider} from "react-redux";
import {saveToLocalStorage, storeWrapper} from "../store/store";

function MithrilExplorer({ Component, ...rest }) {
  const {store, pageProps} = storeWrapper.useWrappedStore(rest);
  store.subscribe(() => saveToLocalStorage(store.getState()));

  return (
    <>
      <Script id="plausible"
          strategy="afterInteractive"
          src="https://plausible.io/js/script.js"
          data-domain="mithril.network" />
      <Provider store={store}>
        <Component {...pageProps} />
      </Provider>
    </>
  );
}

export default MithrilExplorer;
