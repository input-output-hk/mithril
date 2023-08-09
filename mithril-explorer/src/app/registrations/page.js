"use client";

import {useSearchParams} from "next/navigation";
import {useEffect, useState} from "react";
import {aggregatorSearchParam} from "../../constants";
import {checkUrl} from "../../utils";
import {Alert, Stack} from "react-bootstrap";

export default function Registrations() {
  const searchParams = useSearchParams();
  const [currentError, setCurrentError] = useState(undefined);
  const [aggregator, setAggregator] = useState(undefined);
  const [registrationEpoch, setRegistrationEpoch] = useState(undefined);
  const [signingEpoch, setSigningEpoch] = useState(undefined);
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
          setRegistrations([]);
          console.error("Fetch registrations error:", error);
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

  return (
    <Stack>
      {currentError === undefined
        ? <code>{JSON.stringify(registrations)}</code>
        :
        <Alert variant="danger">
          <Alert.Heading>Oh snap! You got an error!</Alert.Heading>
          <p>{getErrorDescription()}</p>
        </Alert>
      }
    </Stack>
  );
}
