import React from "react";
import BrowserOnly from "@docusaurus/BrowserOnly";
import Layout from "@theme/Layout";
import useDocusaurusContext from "@docusaurus/useDocusaurusContext";
import { PageContext, PageType } from "../context/PageContext";
import HomepageHero from "../components/HomepageHero";
import Features from "../components/Features";
import WhyMithril from "../components/WhyMithril";
import AnimatedText from "../components/AnimatedText";
import UseCases from "../components/UseCases";

export default function HomePage() {
  return (
    <BrowserOnly fallback={<div />}>
      {() => (
        <PageContext.Provider value={{ page: PageType.Landing }}>
          <div style={{ zIndex: 1000 }}>
            <Layout>
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
      )}
    </BrowserOnly>
  );
}
