"use client";

import { useSearchParams } from "next/navigation";
import React, { Fragment, useEffect, useState } from "react";
import { useDispatch } from "react-redux";
import { checkUrl, computeInOutRegistrations, dedupInOutRegistrations } from "@/utils";
import { Col, Alert, Row, Spinner, Stack, Table } from "react-bootstrap";
import { aggregatorSearchParam } from "@/constants";
import { updatePoolsForAggregator } from "@/store/poolsSlice";
import { fetchAggregatorStatus, fetchRegistrations } from "@/aggregator-api";
import RegistrationMarkdownFormatModal from "#/RegistrationMarkdownFormatModal";
import RegistrationsMovementsList from "@/app/registrations-in-out/RegistrationsMovementsList";

export default function RegistrationsChanges() {
  const searchParams = useSearchParams();
  const dispatch = useDispatch();
  const aggregator = searchParams.get(aggregatorSearchParam);
  const initialError = checkUrl(aggregator) ? undefined : "invalidAggregatorUrl";
  const [currentError, setCurrentError] = useState(initialError);
  const [isLoading, setIsLoading] = useState(true);
  const [currentEpoch, setCurrentEpoch] = useState(undefined);
  const [completeDiff, setCompleteDiff] = useState(undefined);
  const [dedupDiff, setDedupDiff] = useState({});
  const [markdownFormatModalMode, setMarkdownFormatModalMode] = useState(undefined);

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

  useEffect(() => {
    if (initialError !== undefined) {
      return;
    }

    fetchAggregatorStatus(aggregator)
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
        setCurrentError("fetchError");
      });

    dispatch(updatePoolsForAggregator(aggregator));
  }, [aggregator, initialError, dispatch]);

  if (currentError !== undefined) {
    let errorDescription = "";
    switch (currentError) {
      case "invalidAggregatorUrl":
        errorDescription = (
          <p>The given aggregator isn&apos;t a valid url, please correct it and try again.</p>
        );
        break;
      case "fetchError":
        errorDescription = (
          <>
            <p>Something went wrong while fetching data from the aggregator.</p>
            <p>
              Aggregator url: <code>{aggregator}</code>
            </p>
          </>
        );
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
          <hr />
          <div>{errorDescription}</div>
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
