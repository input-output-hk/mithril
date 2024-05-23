import React from "react";
import Link from "next/link";

export default function LinkButton({
  href,
  children,
  className,
  disabled,
  variant = "primary",
  ...props
}) {
  if (disabled === true) {
    className = `${className} disabled`;
  } else {
    disabled = false;
  }

  return (
    <Link
      href={href}
      aria-disabled={disabled}
      className={`btn btn-${variant} link-underline-opacity-0 ${className}`}
      {...props}>
      {children}
    </Link>
  );
}
