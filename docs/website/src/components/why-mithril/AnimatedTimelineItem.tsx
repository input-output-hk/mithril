import React from "react";
import {
  motion,
  useTransform,
  useSpring,
  useMotionTemplate,
  MotionValue,
} from "framer-motion";
import { cx } from "cva";
import { forTablet } from "../../../helpers/media-queries";
import useMediaQuery from "../../hooks/useMediaQuery";

interface Props {
  scrollYProgress: MotionValue<number>;
  index: number;
  item: string;
}

const AnimatedTimelineItem: React.FC<Props> = ({
  scrollYProgress,
  index,
  item,
}) => {
  const isTabletUp = useMediaQuery(forTablet);
  const start = isTabletUp ? 0.02 + index * 0.06 : 0.02 + index * 0.025;
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
  const springOpacity = useSpring(opacity, { stiffness: 100, damping: 30 });
  const springBlur = useSpring(blur, { stiffness: 100, damping: 30 });

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
      className={cx("grid-item", item === "Basho" && "text-blue-highlight")}
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
};

export default AnimatedTimelineItem;
