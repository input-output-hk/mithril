"use client";

import {useRouter, useSearchParams} from "next/navigation";
import {useCallback, useEffect, useState} from "react";
import {Alert, Button, ButtonGroup, Col, Row, Stack, Table} from "react-bootstrap";
import VerifiedBadge from "../../components/VerifiedBadge";
import {aggregatorSearchParam} from "../../constants";
import {checkUrl} from "../../utils";
import RawJsonButton from "../../components/RawJsonButton";

export default function Registrations() {
  const router = useRouter();
  const searchParams = useSearchParams();
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
        })
        .catch(error => {
          setSigningEpoch(undefined);
          setRegistrations([]);
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

  function getErrorDescription() {
    let description = "";

    if (currentError) {
      switch (currentError) {
        case 'invalidEpoch':
          description = "The given epoch isn't an integer, please correct it and try again.";
          break;
        case 'invalidAggregatorUrl':
          description = "The given aggregator isn't a valid url, please correct it and try again.";
          break;
        default:
          description = "Something went wrong";
          break;
      }
    }

    return description;
  }

  const navigateTo = useCallback((epoch) => {
    const params = new URLSearchParams();
    params.set("aggregator", aggregator);
    params.set("epoch", epoch);

    router.push(`/registrations?${params.toString()}`)
  }, [aggregator, router]);

  const navigateToPrevious = () => navigateTo(registrationEpoch - 1);
  const navigateToCurrent = () => navigateTo(currentEpoch);
  const navigateToNext = () => navigateTo(registrationEpoch + 1);

  return (
    <Stack gap={3}>
      <h2>
        Registrations {' '}
        {currentError === undefined &&
          <RawJsonButton
            href={`${aggregator}/signers/registered/${registrationEpoch}`}
            variant="outline-light"
            size="sm"/>
        }
      </h2>
      {currentError === undefined
        ? <>
          <Row>
            <Col xs={12} sm={10}>
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
            </Col>
            <Col className="d-flex align-content-center justify-content-center">
              <ButtonGroup xs={12} sm="auto" vertical>
                <Button onClick={navigateToPrevious}>
                  Previous Epoch ({registrationEpoch - 1})
                </Button>
                <Button onClick={navigateToCurrent}
                        disabled={currentEpoch === undefined || currentEpoch === registrationEpoch}>
                  Current Epoch ({currentEpoch})
                </Button>
                <Button onClick={navigateToNext}>
                  Next Epoch ({registrationEpoch + 1})
                </Button>
              </ButtonGroup>
            </Col>
          </Row>
          {registrations === undefined || registrations.length === 0
            ?
            <Alert variant="info">
              <Alert.Heading>No registrations for epoch {registrationEpoch}</Alert.Heading>
              <div>
                Either:
                <ul>
                  <li>It's the current epoch and the aggregator did not receive registrations yet.</li>
                  <li>the epoch is in the future.</li>
                  <li>the epoch is in the past and the aggregator have pruned old registrations.</li>
                </ul>
              </div>
            </Alert>
            :
            <Row>
              <Col xs={12} sm={12} md={5}>
                <Stack gap={3}>
                  <div></div>
                  <div></div>
                </Stack>
              </Col>
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
            </Row>
          }
        </>
        : <>
          <Alert variant="danger">
            <Alert.Heading>Oh snap! You got an error!</Alert.Heading>
            <p>{getErrorDescription()}</p>
          </Alert>
        </>
      }
    </Stack>
  );
}
