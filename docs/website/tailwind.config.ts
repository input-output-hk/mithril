import type { Config } from "tailwindcss";

const config: Config = {
  content: ["./src/**/*.{js,jsx,ts,tsx}"],
  theme: {
    backgroundImage: {},
    screens: {
      phablet: "640px",
      // => @media (min-width: 640px) { ... }

      tablet: "768px",
      // => @media (min-width: 768px) { ... }

      laptop: "996px",
      // => @media (min-width: 996px) { ... }

      desktop: "1280px",
      // => @media (min-width: 1280px) { ... }
    },
    fontFamily: {
      display: [
        "Bw Gradual",
        "ui-sans-serif",
        "system-ui",
        "sans-serif",
        '"Apple Color Emoji"',
        '"Segoe UI Emoji"',
        '"Segoe UI Symbol"',
        '"Noto Color Emoji"',
      ],
      body: [
        "var(--font-lexend)",
        "ui-sans-serif",
        "system-ui",
        "sans-serif",
        '"Apple Color Emoji"',
        '"Segoe UI Emoji"',
        '"Segoe UI Symbol"',
        '"Noto Color Emoji"',
      ],
    },
    colors: {
      transparent: "transparent",
      current: "currentColor",
      blue: {
        DEFAULT: "#01193C",
        light: "#ECF9FD",
        highlight: "#2634AD",
      },
      secondary: "#3c9ec8",
      black: "#000000",
      gray: {
        DEFAULT: "#7C8080",
        dark: "#99a3b1",
        light: "#808C9D",
        "extra-light": "#F3F4F4",
        border: "#B3BAC5",
      },
      white: "#FFFFFF",
      cyan: {
        DEFAULT: "#65CFE1",
        dark: "#397589",
      },
    },
  },
  plugins: [],
};
export default config;
