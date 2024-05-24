"use client";

import { useSearchParams } from "next/navigation";
import React, { Fragment, useEffect, useState } from "react";
import { useDispatch } from "react-redux";
import { checkUrl, computeInOutRegistrations, dedupInOutRegistrations } from "@/utils";
import { Col, Alert, Row, Spinner, Stack, Table } from "react-bootstrap";
import { aggregatorSearchParam } from "@/constants";
import { updatePoolsForAggregator } from "@/store/poolsSlice";
import { fetchEpochSettings, fetchRegistrations } from "@/aggregator-api";
import RegistrationMarkdownFormatModal from "#/RegistrationMarkdownFormatModal";
import RegistrationsMovementsList from "@/app/registrations-in-out/RegistrationsMovementsList";

export default function RegistrationsChanges() {
  const dispatch = useDispatch();
  const searchParams = useSearchParams();
  const [isLoading, setIsLoading] = useState(true);
  const [currentError, setCurrentError] = useState(undefined);
  const [aggregator, setAggregator] = useState(undefined);
  const [currentEpoch, setCurrentEpoch] = useState(undefined);
  const [completeDiff, setCompleteDiff] = useState(undefined);
  const [dedupDiff, setDedupDiff] = useState(undefined);
  const [markdownFormatModalMode, setMarkdownFormatModalMode] = useState(undefined);

  useEffect(() => {
    const aggregator = searchParams.get(aggregatorSearchParam);
    let error = undefined;
    setAggregator(aggregator);

    if (!checkUrl(aggregator)) {
      error = "invalidAggregatorUrl";
    }

    if (error === undefined) {
      setIsLoading(true);

      fetchEpochSettings(aggregator)
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
        .catch(() => {
          setCurrentEpoch(undefined);
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

  function handleRegistrationsMarkdownFormatClose() {
    setMarkdownFormatModalMode(undefined);
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
        <>
          <RegistrationMarkdownFormatModal
            registrations={dedupDiff}
            onClose={handleRegistrationsMarkdownFormatClose}
            mode={markdownFormatModalMode}
          />

          <Row>
            <Table>
              <tbody>
                <tr>
                  <td>
                    <strong>Aggregator:</strong>
                  </td>
                  <td>{aggregator}</td>
                </tr>
                <tr>
                  <td>
                    <strong>Current epoch:</strong>
                  </td>
                  <td>{currentEpoch}</td>
                </tr>
              </tbody>
            </Table>
          </Row>
          <Row>
            <Col xs={12} sm={12} md={12} lg={6}>
              <div className="p-2 mb-2 border border-danger rounded">
                <RegistrationsMovementsList
                  registrations={dedupDiff}
                  mode="out"
                  onMarkdownButtonClick={() => setMarkdownFormatModalMode("out")}
                />
              </div>
            </Col>
            <Col xs={12} sm={12} md={12} lg={6}>
              <div className="p-2 border border-success rounded">
                <RegistrationsMovementsList
                  registrations={dedupDiff}
                  mode="in"
                  onMarkdownButtonClick={() => setMarkdownFormatModalMode("in")}
                />
              </div>
            </Col>
          </Row>
        </>
      )}
    </Stack>
  );
}
