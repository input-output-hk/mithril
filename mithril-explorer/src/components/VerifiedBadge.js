import React from "react";
import IconBadge from "./IconBadge";

export default function VerifiedBadge({ tooltip = "Verified Signer" }) {
  return <IconBadge tooltip={tooltip} icon="shield-lock" variant="success" />;
}
