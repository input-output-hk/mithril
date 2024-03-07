import React from "react";
import { CopyButton } from "./CopyButton";

export default function CopyableHash({ hash }) {
  return (
    <span className="text-break">
      {hash} <CopyButton textToCopy={hash} />
    </span>
  );
}
