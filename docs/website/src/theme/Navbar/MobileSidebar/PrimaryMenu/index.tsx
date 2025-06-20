import React, { type ReactNode } from "react";
import { useThemeConfig } from "@docusaurus/theme-common";
import { useNavbarMobileSidebar } from "@docusaurus/theme-common/internal";
import NavbarItem, { type Props as NavbarItemConfig } from "@theme/NavbarItem";
import Discord from "../../../../components/icons/Discord";
import Github from "../../../../components/icons/Github";

function useNavbarItems() {
  // TODO temporary casting until ThemeConfig type is improved
  return useThemeConfig().navbar.items as NavbarItemConfig[];
}

// The primary menu displays the navbar items
export default function NavbarMobilePrimaryMenu(): ReactNode {
  const mobileSidebar = useNavbarMobileSidebar();

  // TODO how can the order be defined for mobile?
  // Should we allow providing a different list of items?
  const items = useNavbarItems();

  return (
    <ul className="menu__list">
      {items.map((item, i) => (
        <NavbarItem
          mobile
          {...item}
          onClick={() => mobileSidebar.toggle()}
          key={i}
        />
      ))}
      <a
        href="https://github.com/input-output-hk/mithril/"
        target="_blank"
        rel="noopener noreferrer"
        className="hover:text-[var(--ifm-navbar-link-hover-color)] mx-3 py-1 laptop:flex hidden"
      >
        <Github />
      </a>
      <a
        href="https://discord.gg/5kaErDKDRq"
        target="_blank"
        rel="noopener noreferrer"
        className="hover:text-[var(--ifm-navbar-link-hover-color)] mx-3 py-1 laptop:flex hidden"
      >
        <Discord />
      </a>
    </ul>
  );
}
