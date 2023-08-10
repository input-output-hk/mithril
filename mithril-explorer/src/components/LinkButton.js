import React from 'react';
import Link from "next/link";

export default function RawJsonButton({href, children, className, disabled, ...props}) {
  if (disabled === true) {
    className = `${className} disabled`;
  } else {
    disabled = false;
  }

  return (
    <Link href={href}
          aria-disabled={disabled}
          className={`btn btn-primary link-underline-opacity-0 link-light ${className}`}
          {...props}>
      {children}
    </Link>
  );
}
