import React, { useState, useRef, useEffect } from "react";
import {
  Alert,
  Button,
  Popover,
  Overlay,
  Form,
  InputGroup,
  Container,
  Row,
  Col,
  Spinner,
  Stack,
} from "react-bootstrap";
import { getImmutableUrlFromTemplate } from "@/utils";

import styles from "./styles.module.css";

const DownloadIcon = () => <i className="bi bi-download"></i>;

const isDirectDownload = (location) =>
  location?.type === "cloud_storage" || location?.type === "aggregator";

function LocationsSelect({ ariaLabel, locations, value, onChange }) {
  function locationLabel(type) {
    switch (type) {
      case "cloud_storage":
        return "Cloud Storage";
      case "aggregator":
        return "Aggregator";
      default:
        return type;
    }
  }

  return (
    <Form.Select aria-label={ariaLabel} value={value} onChange={onChange}>
      {locations?.map((location, index) => (
        <option key={`digest-${location.type}-${index}`} value={index}>
          {locationLabel(location.type)}
        </option>
      ))}
    </Form.Select>
  );
}

export function DownloadImmutableFormInput({ max, value, onChange }) {
  return (
    <Form.Control type="number" value={value} onChange={onChange} required min={0} max={max} />
  );
}

export default function DownloadButton({ artifactUrl, ...props }) {
  const [show, setShow] = useState(false);
  const target = useRef(null);
  const [immutableFileNumber, setImmutableFileNumber] = useState(0);
  const [maxImmutableFileNumber, setMaxImmutableFileNumber] = useState(0);
  const [loaded, setLoaded] = useState(false);
  const [artifact, setArtifact] = useState(undefined);
  const [digestLocationIndex, setDigestLocationIndex] = useState(0);
  const [immutableLocationIndex, setImmutableLocationIndex] = useState(0);
  const [ancillaryLocationIndex, setAncillaryLocationIndex] = useState(0);
  const [showImmutableDownloadValidation, setShowImmutableDownloadValidation] = useState(false);

  const handleClose = () => setShow(false);

  useEffect(() => {
    if (!show || loaded) {
      return;
    }

    fetch(artifactUrl)
      .then((response) => response.json())
      .then((data) => {
        setArtifact(data);
        let maxImmutableFileNumber = data.beacon.immutable_file_number;
        setImmutableFileNumber(maxImmutableFileNumber);
        setMaxImmutableFileNumber(maxImmutableFileNumber);
      })
      .catch((error) => {
        setArtifact(undefined);
        setImmutableFileNumber(0);
        setMaxImmutableFileNumber(0);
        console.error("Fetch Cardano DB v2 artifact details error:", error);
      })
      .finally(() => setLoaded(true));
  }, [artifactUrl, loaded, show]);

  function directDownload(uri) {
    window.open(uri, "_blank", "noopener");
  }

  function downloadImmutable(event) {
    // Prevent page refresh
    event.preventDefault();
    const form = event.target;

    if (form.checkValidity() === true) {
      const location = artifact?.locations.immutables[immutableLocationIndex];

      if (location?.type === "cloud_storage") {
        directDownload(getImmutableUrlFromTemplate(location.uri.Template, immutableFileNumber));
      }
      setShowImmutableDownloadValidation(false);
    } else {
      setShowImmutableDownloadValidation(true);
    }
  }

  function downloadDigest() {
    const location = artifact?.locations.digests[digestLocationIndex];
    if (isDirectDownload(location)) {
      directDownload(location.uri);
    }
  }

  function downloadAncillary() {
    const location = artifact?.locations.ancillary[ancillaryLocationIndex];
    if (isDirectDownload(location)) {
      directDownload(location.uri);
    }
  }

  return (
    <>
      <Button ref={target} variant="outline-secondary" onClick={() => setShow(!show)} {...props}>
        Download <DownloadIcon />
      </Button>

      <Overlay
        target={target.current}
        show={show}
        rootClose
        onHide={handleClose}
        placement="top-end">
        <Popover className={styles.downloadPopover}>
          <Popover.Header>
            Download Options{" "}
            <Button variant="close" onClick={handleClose} aria-label="Close" size="sm" />
          </Popover.Header>
          <Popover.Body as={Container} className="p-3">
            {!loaded ? (
              <Row className="mb-2">
                <Col>
                  <Spinner animation="border" />
                </Col>
              </Row>
            ) : (
              <>
                <Stack gap={2}>
                  <Alert variant="danger" className="mb-1">
                    <Alert.Heading>
                      <i className="bi bi-exclamation-triangle"></i> No certification
                    </Alert.Heading>
                    To download certified artifacts you <strong>must</strong> use the{" "}
                    <Alert.Link
                      href="https://mithril.network/doc/next/manual/develop/nodes/mithril-client"
                      target="_blank">
                      Mithril client
                    </Alert.Link>{" "}
                    which will make sure they are correctly signed by the Mithril protocol.
                  </Alert>

                  <Form.Text>Immutable (max: {maxImmutableFileNumber})</Form.Text>
                  <Form
                    noValidate
                    onSubmit={downloadImmutable}
                    validated={showImmutableDownloadValidation}>
                    <InputGroup>
                      <DownloadImmutableFormInput
                        type="text"
                        max={maxImmutableFileNumber}
                        value={immutableFileNumber}
                        onChange={(e) => setImmutableFileNumber(e.target.value)}
                      />
                      <LocationsSelect
                        ariaLabel="Immutable locations"
                        locations={artifact?.locations.immutables}
                        onChange={(e) => setImmutableLocationIndex(e.target.value)}
                        value={immutableLocationIndex}
                      />
                      <Button variant="primary" type="submit">
                        <DownloadIcon />
                      </Button>
                    </InputGroup>
                  </Form>

                  <Form.Text>Digests</Form.Text>
                  <InputGroup>
                    <LocationsSelect
                      ariaLabel="Digests locations"
                      locations={artifact?.locations.digests}
                      onChange={(e) => setDigestLocationIndex(e.target.value)}
                      value={digestLocationIndex}
                    />
                    <Button variant="primary" onClick={downloadDigest}>
                      <DownloadIcon />
                    </Button>
                  </InputGroup>

                  <Form.Text>Ancillary</Form.Text>
                  <InputGroup>
                    <LocationsSelect
                      ariaLabel="Ancillary locations"
                      locations={artifact?.locations.ancillary}
                      onChange={(e) => setAncillaryLocationIndex(e.target.value)}
                      value={ancillaryLocationIndex}
                    />
                    <Button variant="primary" onClick={downloadAncillary}>
                      <DownloadIcon />
                    </Button>
                  </InputGroup>
                </Stack>
              </>
            )}
          </Popover.Body>
        </Popover>
      </Overlay>
    </>
  );
}
