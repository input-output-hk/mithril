import React from 'react';
import {OverlayTrigger, Tooltip} from "react-bootstrap";
import {formatCurrency, formatStake, toAda} from "../utils";

export default function Stake({lovelace}) {
  return (
    <OverlayTrigger overlay={<Tooltip>{formatCurrency(toAda(lovelace), 20)}₳</Tooltip>}>
      <span>{formatStake(lovelace)}</span>
    </OverlayTrigger>
  );
}
