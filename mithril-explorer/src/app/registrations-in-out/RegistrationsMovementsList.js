import React, { Fragment } from "react";
import { Button, OverlayTrigger, Tooltip } from "react-bootstrap";
import SignerTable from "#/SignerTable";

function DiscordModalButton({ onClick }) {
  return (
    <OverlayTrigger overlay={<Tooltip>Discord formatted code block</Tooltip>}>
      <Button variant="secondary" size="sm" onClick={onClick}>
        <i className="bi bi-discord"></i>
      </Button>
    </OverlayTrigger>
  );
}

export default function RegistrationsMovementsList({ mode, registrations, onDiscordButtonClick }) {
  const movementsForCurrentMode = (movements) => (mode === "out" ? movements.out : movements.in);
  const filteredRegistrations = Object.entries(registrations)
    .reverse()
    .filter(([_, movements]) => movementsForCurrentMode(movements).length > 0);

  return (
    <>
      <h3>
        {mode === "out" ? (
          <>
            <i className="bi bi-box-arrow-left"></i> Missing
          </>
        ) : (
          <>
            <i className="bi bi-box-arrow-in-right"></i> New
          </>
        )}{" "}
        Signers <DiscordModalButton onClick={onDiscordButtonClick} />
      </h3>
      {filteredRegistrations.map(([epoch, movements]) => (
        <Fragment key={epoch}>
          <h4>
            {mode === "out" ? "Missing" : "New"} since epoch{" "}
            <span className="text-secondary">#{epoch}</span>
          </h4>
          <SignerTable signers={movementsForCurrentMode(movements)} />
        </Fragment>
      ))}
    </>
  );
}
