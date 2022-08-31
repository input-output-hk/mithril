import 'bootstrap/dist/css/bootstrap.min.css';
import '../styles/globals.css'
import Script from "next/script";

function MithrilExplorer({ Component, pageProps }) {
  return (
    <>
      <Script id="plausible"
              strategy="afterInteractive"
              src="https://plausible.io/js/script.js"
              data-domain="mithril.network" />
      <Component {...pageProps} />
    </>
  )
}

export default MithrilExplorer
