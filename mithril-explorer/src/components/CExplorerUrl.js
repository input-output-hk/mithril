import React from "react";
import { OverlayTrigger, Tooltip } from "react-bootstrap";
import Image from "next/image";
import CopyButton from "./CopyButton";

export function CExplorerUrl({ text, url, textClass, showCopyButton = false }) {
  return (
    <>
      {url !== undefined ? (
        <a href={url} target="_blank" className={`link-dark link-underline-light ${textClass}`}>
          <OverlayTrigger overlay={<Tooltip>See in CExplorer</Tooltip>}>
            <span>
              <Image
                src="/explorer/cexplorer_logo.png"
                alt="CExplorer Logo"
                style={{ verticalAlign: "text-top" }}
                width={20}
                height={20}
              />
              <> </>
              {text ?? "Not available"}
            </span>
          </OverlayTrigger>
        </a>
      ) : (
        <span className={textClass}>{text}</span>
      )}
      {text && showCopyButton && (
        <>
          {" "}
          <CopyButton textToCopy={text} />
        </>
      )}
    </>
  );
}
