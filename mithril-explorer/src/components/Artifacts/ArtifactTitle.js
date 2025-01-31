import { Card, Stack } from "react-bootstrap";
import CopyableHash from "#/CopyableHash";
import React from "react";

export default function ArtifactTitle({ hash, index }) {
  return (
    <Stack direction="horizontal" gap={2}>
      <h6 className="text-secondary card-title">#{index + 1}.</h6>
      <Card.Title>
        <CopyableHash hash={hash} />
      </Card.Title>
    </Stack>
  );
}
