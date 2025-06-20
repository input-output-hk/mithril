import React, { type ReactNode } from "react";
import type { Props } from "@theme/Footer/Layout";
import clsx from "clsx";

export default function FooterLayout({
  links,
  logo,
  copyright,
}: Props): ReactNode {
  return (
    <footer className="bg-blue">
      <div className="pageContainer py-14 text-gray-extra-light">
        {links}
        {(logo || copyright) && (
          <div className="flex tablet:items-center gap-2 tablet:flex-row flex-col text-gray-dark tablet:justify-between tablet:pt-4 tablet:border-t border-t-gray-dark">
            {logo && <div className="margin-bottom--sm">{logo}</div>}
            {copyright}
          </div>
        )}
      </div>
    </footer>
  );
}
