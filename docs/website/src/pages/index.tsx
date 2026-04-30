import React from "react";
import Head from "@docusaurus/Head";
import Layout from "@theme/Layout";
import { PageContext, PageType } from "../context/PageContext";
import HomepageHero from "../components/HomepageHero";
import Features from "../components/Features";
import AnimatedText from "../components/AnimatedText";
import UseCases from "../components/UseCases";
import WhyMithril from "../components/WhyMithril";

export default function HomePage() {
  return (
    <PageContext.Provider value={{ page: PageType.Landing }}>
      <div style={{ zIndex: 1000 }}>
        <Layout description="Mithril is a stake-based threshold multisignature protocol for Cardano, enabling trustless, lightweight access to verified blockchain state without requiring a full node.">
          <Head>
            <title>Mithril | Trustless state proofs for Cardano</title>
          </Head>
          <HomepageHero />
          <main>
            <WhyMithril />
            <AnimatedText />
            <Features />
            <UseCases />
          </main>
        </Layout>
      </div>
    </PageContext.Provider>
  );
}
