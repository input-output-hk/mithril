import React from 'react';
import {Button, OverlayTrigger, Tooltip} from "react-bootstrap";
import {formatPartyId} from "../utils";

export default function PartyId({partyId}) {
    function copyToClipboard() {
        if (window.isSecureContext && partyId) {
            navigator.clipboard.writeText(partyId).then(() => {
            });
        }
    }

    return (
        <span className="text-break">
            {partyId}<> </>
            <OverlayTrigger overlay={<Tooltip>Copy</Tooltip>}>
                <Button variant="link" onClick={copyToClipboard} size="md" className="p-0">
                    <i className="bi bi-copy" style={{color: 'black'}}></i>
                </Button>
            </OverlayTrigger>
        </span>
    );
}
