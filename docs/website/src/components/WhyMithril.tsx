import React, { FC, useRef } from "react";
import {
  motion,
  useScroll,
  useTransform,
  useSpring,
  useMotionTemplate,
} from "framer-motion";
import { WhyMithrilContents } from "../../homepage-content/why-mithril";
import { cx } from "cva";
import Link from "@docusaurus/Link";

const WhyMithril: FC = () => {
  const containerRef = useRef(null);

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
            {WhyMithrilContents.timeline.map((item, index) => {
              const start = 0.07 + index * 0.07;
              const end = start + 0.3;

              const z = useTransform(
                scrollYProgress,
                [start, (start + end) / 2, end],
                [-200, 0, 200],
              );
              const opacity = useTransform(
                scrollYProgress,
                [start, (start + end) / 2, end],
                [0, 1, 0],
              );
              const blur = useTransform(
                scrollYProgress,
                [start, (start + end) / 1.8, end],
                [5, 0, 100],
              );

              const springZ = useSpring(z, { stiffness: 100, damping: 30 });
              const springOpacity = useSpring(opacity, {
                stiffness: 100,
                damping: 30,
              });
              const springBlur = useSpring(blur, {
                stiffness: 100,
                damping: 30,
              });

              const transform = useMotionTemplate`translateZ(${springZ}px)`;
              const filter = useMotionTemplate`blur(${springBlur}px)`;

              const alignments = [
                "flex-start",
                "flex-end",
                "flex-start",
                "center",
                "flex-end",
              ];

              return (
                <motion.div
                  key={index}
                  className={cx(
                    "grid-item",
                    item === "Basho" && "text-blue-highlight",
                  )}
                  style={{
                    opacity: springOpacity,
                    transform,
                    filter,
                    transformStyle: "preserve-3d",
                    willChange: "transform, opacity, filter",
                    alignSelf: alignments[index % alignments.length],
                  }}
                >
                  {item}
                </motion.div>
              );
            })}
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
