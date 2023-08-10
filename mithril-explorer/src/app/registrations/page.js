"use client";

import {useSearchParams} from "next/navigation";
import {useCallback, useEffect, useState} from "react";
import {Alert, ButtonGroup, Col, Row, Spinner, Stack, Table} from "react-bootstrap";
import VerifiedBadge from "../../components/VerifiedBadge";
import {aggregatorSearchParam} from "../../constants";
import {checkUrl} from "../../utils";
import LinkButton from "../../components/LinkButton";
import RawJsonButton from "../../components/RawJsonButton";

export default function Registrations() {
  const searchParams = useSearchParams();
  const [isLoading, setIsLoading] = useState(true);
  const [currentError, setCurrentError] = useState(undefined);
  const [aggregator, setAggregator] = useState(undefined);
  const [registrationEpoch, setRegistrationEpoch] = useState(undefined);
  const [signingEpoch, setSigningEpoch] = useState(undefined);
  const [currentEpoch, setCurrentEpoch] = useState(undefined);
  const [registrations, setRegistrations] = useState([]);

  useEffect(() => {
    const aggregator = searchParams.get(aggregatorSearchParam);
    const epoch = Number(searchParams.get('epoch'));
    let error = undefined;
    setAggregator(aggregator);
    setRegistrationEpoch(epoch);

    if (!checkUrl(aggregator)) {
      error = "invalidAggregatorUrl";
    } else if (!Number.isInteger(epoch)) {
      error = "invalidEpoch";
    }

    if (error === undefined) {
      fetch(`${aggregator}/signers/registered/${epoch}`)
        .then(response => response.status === 200 ? response.json() : {})
        .then(data => {
          setSigningEpoch(data.signing_at);
          setRegistrations(data.registrations);
          setIsLoading(false);
        })
        .catch(error => {
          setSigningEpoch(undefined);
          setRegistrations([]);
          setIsLoading(false);
          console.error("Fetch registrations error:", error);
        });

      fetch(`${aggregator}/epoch-settings`)
        .then(response => response.status === 200 ? response.json() : {})
        .then(data => setCurrentEpoch(data?.epoch))
        .catch(error => {
          setCurrentEpoch(undefined);
          console.error("Fetch current epoch in epoch-settings error:", error);
        });
    } else {
      setCurrentError(error);
    }
  }, [searchParams]);

  function getNoRegistrationsMessage() {
    if (currentEpoch === registrationEpoch) {
      return "The aggregator did not receive registrations yet for the current epoch.";
    } else if (currentEpoch < registrationEpoch) {
      return "The epoch is in the future";
    } else {
      return "The aggregator may have pruned old registrations or wasn't running at this epoch.";
    }
  }

  const navigateToUrl = useCallback((epoch) => {
    const params = new URLSearchParams();
    params.set("aggregator", aggregator);
    params.set("epoch", epoch);

    return `/registrations?${params.toString()}`;
  }, [aggregator, router]);

  const navigateToPreviousUrl = navigateToUrl(registrationEpoch - 1);
  const navigateToCurrentUrl = navigateToUrl(currentEpoch);
  const navigateToNextUrl = navigateToUrl(registrationEpoch + 1);

  if (currentError !== undefined) {
    let errorDescription = "";
    switch (currentError) {
      case 'invalidEpoch':
        errorDescription = "The given epoch isn't an integer, please correct it and try again.";
        break;
      case 'invalidAggregatorUrl':
        errorDescription = "The given aggregator isn't a valid url, please correct it and try again.";
        break;
      default:
        errorDescription = "Something went wrong";
        break;
    }

    return (
      <Stack gap={3}>
        <h2>Registrations</h2>
        <Alert variant="danger">
          <Alert.Heading>Oh snap! You got an error!</Alert.Heading>
          <p>{errorDescription}</p>
        </Alert>
      </Stack>
    );
  }

  return (
    <Stack gap={3}>
      <h2>
        Registrations {' '}
        <RawJsonButton
          href={`${aggregator}/signers/registered/${registrationEpoch}`}
          variant="outline-light"
          size="sm"/>
      </h2>
      <Row>
        <Table>
          <tbody>
          <tr>
            <td><strong>Aggregator:</strong></td>
            <td>{aggregator}</td>
          </tr>
          <tr>
            <td><strong>Registration epoch:</strong></td>
            <td>{registrationEpoch}</td>
          </tr>
          <tr>
            <td><strong>Signing at epoch:</strong></td>
            <td>{signingEpoch}</td>
          </tr>
          </tbody>
        </Table>
      </Row>
      <Row>
        <div>
          {Number.isInteger(registrationEpoch) &&
            <ButtonGroup>
              <LinkButton href={navigateToPreviousUrl}>
                Previous Epoch ({registrationEpoch - 1})
              </LinkButton>
              <LinkButton href={navigateToCurrentUrl}
                          disabled={currentEpoch === undefined || currentEpoch === registrationEpoch}>
                Current Epoch ({currentEpoch})
              </LinkButton>
              <LinkButton href={navigateToNextUrl}
                          disabled={currentEpoch <= registrationEpoch}>
                Next Epoch ({registrationEpoch + 1})
              </LinkButton>
            </ButtonGroup>
          }
        </div>
      </Row>
      {isLoading
        ? <Spinner animation="grow"/>
        : registrations === undefined || registrations.length === 0
          ?
          <Alert variant="info">
            <Alert.Heading>No registrations for epoch {registrationEpoch}</Alert.Heading>
            <p>{getNoRegistrationsMessage()}</p>
          </Alert>
          :
          <Row>
            <Col xs={12} sm={12} md={7}>
              <Table responsive striped>
                <thead>
                <tr>
                  <th>#</th>
                  <th>Party id</th>
                  <th>Stake</th>
                </tr>
                </thead>
                <tbody>
                {registrations.map((signer, index) =>
                  <tr key={signer.party_id}>
                    <td>{index}</td>
                    <td><VerifiedBadge tooltip="Verified Signer"/>{' '}{signer.party_id}</td>
                    <td>{signer.stake}</td>
                  </tr>
                )}
                </tbody>
              </Table>
            </Col>
            <Col xs={12} sm={12} md={5}>
              <Stack gap={3}>
                <div></div>
                <div></div>
              </Stack>
            </Col>
          </Row>
      }
    </Stack>
  );
}
