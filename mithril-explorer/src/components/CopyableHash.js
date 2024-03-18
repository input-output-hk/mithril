import React from "react";
import CopyButton from "#/CopyButton";

export default function CopyableHash({ hash, className, ...props }) {
  return (
    <span className={`text-break ${className}`} {...props}>
      {hash} <CopyButton textToCopy={hash} />
    </span>
  );
}
