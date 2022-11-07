import 'bootstrap/dist/css/bootstrap.min.css';
import 'bootstrap-icons/font/bootstrap-icons.css';
import '../styles/globals.css'
import Script from "next/script";
import { storeWrapper } from "../store/store";
import {Provider} from "react-redux";

function MithrilExplorer({ Component, ...rest }) {
  const {store, pageProps} = storeWrapper.useWrappedStore(rest);

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
