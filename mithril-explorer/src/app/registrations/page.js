"use client";

import {useSearchParams} from "next/navigation";
import {useEffect, useState} from "react";
import {aggregatorSearchParam} from "../../constants";
import {checkUrl} from "../../utils";
import {Alert} from "react-bootstrap";

export default function Registrations() {
  const searchParams = useSearchParams();
  const [currentError, setCurrentError] = useState(undefined);
  const [aggregator, setAggregator] = useState(undefined);
  const [registrationEpoch, setRegistrationEpoch] = useState(undefined);
  const [signingEpoch, setSigningEpoch] = useState(undefined);
  const [registrations, setRegistrations] = useState([]);

  useEffect(() => {
    const aggregator = searchParams.get(aggregatorSearchParam);
    const epoch = searchParams.get('epoch');
    setAggregator(aggregator);
    setRegistrationEpoch(epoch);

    if (checkUrl(aggregator)) {
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
      setCurrentError("The aggregator endpoint isn't a valid url.");
    }
  }, [searchParams]);

  return (
    <>
      {currentError === undefined
        ? <code>{JSON.stringify(registrations)}</code>
        :
        <Alert variant="danger">
          <Alert.Heading>Oh snap! You got an error!</Alert.Heading>
          <p>{currentError}</p>
        </Alert>
      }
    </>
  );
}
