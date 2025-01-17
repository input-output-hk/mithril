import React from "react";

export default function ArtifactListItem({
  label,
  children,
  className,
  wordBreak = false,
  vertical = false,
  ...props
}) {
  let direction = `${vertical ? "flex-column" : "flex-row"}`;
  let additionalChildrenClasses = `${wordBreak ? "text-break" : ""}`;

  return (
    <>
      <div className={`mx-2 d-flex ${direction} justify-content-between ${className}`} {...props}>
        <div className="me-2 flex-fill">
          <em>{label}:</em>
        </div>
        <div className={`test-end ${additionalChildrenClasses}`}>{children}</div>
      </div>
      <hr className="my-2" />
    </>
  );
}
