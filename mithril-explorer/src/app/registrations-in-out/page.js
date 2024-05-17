"use client";

import { useSearchParams } from "next/navigation";
import { useEffect, useState } from "react";
import { useDispatch } from "react-redux";
import { checkUrl, computeInOutRegistrations, dedupInOutRegistrations } from "@/utils";
import { Alert, Col, Row, Spinner, Stack } from "react-bootstrap";
import { aggregatorSearchParam } from "@/constants";
import { updatePoolsForAggregator } from "@/store/poolsSlice";
import { fetchRegistrations } from "@/aggregator-api";

export default function RegistrationsChanges() {
  const dispatch = useDispatch();
  const searchParams = useSearchParams();
  const [isLoading, setIsLoading] = useState(true);
  const [currentError, setCurrentError] = useState(undefined);
  const [aggregator, setAggregator] = useState(undefined);
  const [currentEpoch, setCurrentEpoch] = useState(undefined);
  const [completeDiff, setCompleteDiff] = useState(undefined);
  const [dedupDiff, setDedupDiff] = useState(undefined);

  useEffect(() => {
    const aggregator = searchParams.get(aggregatorSearchParam);
    let error = undefined;
    setAggregator(aggregator);

    if (!checkUrl(aggregator)) {
      error = "invalidAggregatorUrl";
    }

    if (error === undefined) {
      setIsLoading(true);

      fetch(`${aggregator}/epoch-settings`)
        .then((response) => (response.status === 200 ? response.json() : {}))
        .then((data) => {
          let epoch = data?.epoch;
          setCurrentEpoch(epoch);

          if (epoch) {
            return fetchInOutRegistrations(aggregator, epoch);
          }
        })
        .then((inOutRegistrations) => {
          setCompleteDiff(inOutRegistrations);
          setDedupDiff(dedupInOutRegistrations(inOutRegistrations));
        })
        .then(() => setIsLoading(false))
        .catch((error) => {
          setCurrentEpoch(undefined);
          console.error("Fetch current epoch in epoch-settings error:", error);
        });

      dispatch(updatePoolsForAggregator(aggregator));
    } else {
      setCurrentError(error);
    }
  }, [searchParams]); // eslint-disable-line react-hooks/exhaustive-deps

  function fetchInOutRegistrations(aggregator, fromEpoch) {
    return Promise.all([
      fetchRegistrations(aggregator, fromEpoch),
      fetchRegistrations(aggregator, fromEpoch - 1),
      fetchRegistrations(aggregator, fromEpoch - 2),
      fetchRegistrations(aggregator, fromEpoch - 3),
    ]).then((registrations) => {
      const [latest, ...previous] = registrations;
      return computeInOutRegistrations(latest, ...previous);
    });
  }

  if (currentError !== undefined) {
    let errorDescription = "";
    switch (currentError) {
      case "invalidAggregatorUrl":
        errorDescription =
          "The given aggregator isn't a valid url, please correct it and try again.";
        break;
      default:
        errorDescription = "Something went wrong";
        break;
    }

    return (
      <Stack gap={3}>
        <h2>In/Out Registrations</h2>
        <Alert variant="danger">
          <Alert.Heading>Oh snap! You got an error!</Alert.Heading>
          <p>{errorDescription}</p>
        </Alert>
      </Stack>
    );
  }

  return (
    <Stack gap={3}>
      <h2>In/Out Registrations</h2>
      {isLoading ? (
        <Spinner animation="grow" />
      ) : (
        <Row>
          <Col xs={12} sm={12} md={7}>
            <h3>Signers</h3>
            <h4>Dedup</h4>
            {JSON.stringify(dedupDiff)}
            <h4>Complete</h4>
            {JSON.stringify(completeDiff)}
          </Col>
        </Row>
      )}
    </Stack>
  );
}
