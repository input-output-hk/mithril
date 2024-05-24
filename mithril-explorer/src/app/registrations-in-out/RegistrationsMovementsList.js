import React, { Fragment } from "react";
import { Button, OverlayTrigger, Stack, Tooltip } from "react-bootstrap";
import SignerTable from "#/SignerTable";

function MarkdownModalButton({ onClick }) {
  return (
    <OverlayTrigger overlay={<Tooltip>Markdown formatted code block</Tooltip>}>
      <Button variant="light" size="sm" className="border-dark" onClick={onClick}>
        <i className="bi bi-markdown"></i>
      </Button>
    </OverlayTrigger>
  );
}

export default function RegistrationsMovementsList({ mode, registrations, onMarkdownButtonClick }) {
  const movementsForCurrentMode = (movements) => (mode === "out" ? movements.out : movements.in);
  const filteredRegistrations = Object.entries(registrations)
    .reverse()
    .filter(([_, movements]) => movementsForCurrentMode(movements).length > 0);

  return (
    <>
      <h3>
        <Stack direction="horizontal">
          <div>
            {mode === "out" ? (
              <>
                <i className="bi bi-box-arrow-left"></i> De-registered
              </>
            ) : (
              <>
                <i className="bi bi-box-arrow-in-right"></i> Newly registered
              </>
            )}{" "}
            Signers
          </div>
          <div className="ms-auto">
            <MarkdownModalButton onClick={onMarkdownButtonClick} />
          </div>
        </Stack>
      </h3>
      {filteredRegistrations.map(([epoch, movements]) => (
        <Fragment key={epoch}>
          <h4>
            {mode === "out" ? "De-registered" : "Registered"} since epoch{" "}
            <span className="text-secondary">#{epoch}</span>
          </h4>
          <SignerTable signers={movementsForCurrentMode(movements)} />
        </Fragment>
      ))}
    </>
  );
}
