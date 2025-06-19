import React from "react";
import Layout from "@theme/Layout";
import useDocusaurusContext from "@docusaurus/useDocusaurusContext";
import { PageContext, PageType } from "../context/PageContext";
import HomepageHero from "../components/HomepageHero";
import Features from "../components/Features";
import WhyMithril from "../components/WhyMithril";
import AnimatedText from "../components/AnimatedText";
import UseCases from "../components/UseCases";

export default function Home() {
  const { siteConfig } = useDocusaurusContext();
  return (
    <PageContext.Provider value={{ page: PageType.Landing }}>
      <div className="z-index:1000">
        <Layout>
          <HomepageHero />
          <main>
            <WhyMithril />
            <AnimatedText />
            <Features />
            <UseCases />
            {/* <div className="pageContainer">
              <AnimatedText />
              <WhyHydraHead />
            </div>
            {isLaptopUp ? <Carousel /> : <ResponsiveCarousel />}
            <Properties />
            <CaseStudies /> */}
          </main>
        </Layout>
      </div>
    </PageContext.Provider>
  );
}
