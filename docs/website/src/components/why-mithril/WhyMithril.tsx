import React, { FC, useRef, useEffect, useState } from "react";
import { motion, useScroll } from "framer-motion";
import Link from "@docusaurus/Link";
import AnimatedTimelineItem from "./AnimatedTimelineItem";
import { WhyMithrilContents } from "../../../homepage-content/why-mithril";

const WhyMithril: FC = () => {
  const containerRef = useRef(null);
  const [isMounted, setIsMounted] = useState(false);

  useEffect(() => {
    setIsMounted(true);
  }, []);

  const { scrollYProgress } = useScroll({
    target: containerRef,
    offset: ["start end", "end start"],
  });

  return (
    <section className="component bg-blue-light">
      <div className="pageContainer">
        <motion.h5
          className="text-base text-black pb-12"
          initial="hidden"
          whileInView="visible"
          viewport={{ once: true }}
          transition={{ duration: 0.35, delay: 0.25 }}
          variants={{
            visible: { opacity: 1, y: 0 },
            hidden: { opacity: 0, y: 100 },
          }}
        >
          {WhyMithrilContents.title}
        </motion.h5>

        <div className="flex tablet:flex-row flex-col justify-between gap-[1.625rem] text-primary">
          <div
            ref={containerRef}
            className="text-3xl text-center flex justify-center text-blue items-center basis-1/2 stuck-grid flex-col gap-4"
          >
            {isMounted &&
              WhyMithrilContents.timeline.map((item, index) => (
                <AnimatedTimelineItem
                  key={index}
                  item={item}
                  index={index}
                  scrollYProgress={scrollYProgress}
                />
              ))}
          </div>

          <motion.div
            className="flex flex-col basis-1/2 gap-10"
            initial="hidden"
            whileInView="visible"
            viewport={{ once: true }}
            transition={{ duration: 0.35, delay: 0.75 }}
            variants={{
              visible: { opacity: 1, y: 0 },
              hidden: { opacity: 0, y: 100 },
            }}
          >
            {WhyMithrilContents.descriptionParagraph.message}
            <Link
              className="inline-block px-4 py-3 font-bold text-sm text-white rounded-lg border-[0.5px] border-gray-border no-underline bg-secondary hover:bg-blue-extralight hover:no-underline hover:text-primary hover:scale-105 transition-all w-fit hover:text-white"
              to="/mithril/beginner/why-use-mithril"
            >
              Learn more
            </Link>
          </motion.div>
        </div>
      </div>
    </section>
  );
};

export default WhyMithril;
