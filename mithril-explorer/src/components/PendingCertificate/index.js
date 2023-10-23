import React, { useEffect, useState } from "react";
import { Accordion, Card, CardGroup, ListGroup } from "react-bootstrap";
import { useSelector } from "react-redux";
import PartyId from "../PartyId";
import PoolTicker from "../PoolTicker";
import RawJsonButton from "../RawJsonButton";
import SignedEntityType from "../SignedEntityType";
import VerifiedBadge from "../VerifiedBadge";
import { selectedAggregator } from "../../store/settingsSlice";

export default function PendingCertificate(props) {
  const [pendingCertificate, setPendingCertificate] = useState({});
  const pendingCertificateEndpoint = useSelector(
    (state) => `${selectedAggregator(state)}/certificate-pending`,
  );
  const aggregator = useSelector(selectedAggregator);
  const autoUpdate = useSelector((state) => state.settings.autoUpdate);
  const updateInterval = useSelector((state) => state.settings.updateInterval);

  useEffect(() => {
    if (!autoUpdate) {
      return;
    }

    let fetchPendingCertificate = () => {
      fetch(pendingCertificateEndpoint)
        .then((response) => (response.status === 200 ? response.json() : {}))
        .then((data) => setPendingCertificate(data))
        .catch((error) => {
          setPendingCertificate({});
          console.error("Fetch certificate-pending error:", error);
        });
    };

    // Fetch it once without waiting
    fetchPendingCertificate();

    const interval = setInterval(fetchPendingCertificate, updateInterval);
    return () => clearInterval(interval);
  }, [pendingCertificateEndpoint, updateInterval, autoUpdate]);

  return (
    <div className={props.className}>
      <h2>
        Pending Certificate
        {Object.entries(pendingCertificate).length !== 0 && (
          <RawJsonButton href={pendingCertificateEndpoint} variant="outline-light" size="sm" />
        )}
      </h2>

      {Object.entries(pendingCertificate).length === 0 ? (
        <p>No pending certificate available</p>
      ) : (
        <CardGroup>
          <Card>
            <Card.Body>
              <Card.Title>Beacon</Card.Title>
              <ListGroup className="mb-2" variant="flush">
                <ListGroup.Item>Network: {pendingCertificate.beacon.network}</ListGroup.Item>
                <ListGroup.Item>Epoch: {pendingCertificate.beacon.epoch}</ListGroup.Item>
              </ListGroup>
              <Card.Title>Entity Type</Card.Title>
              <SignedEntityType signedEntityType={pendingCertificate.entity_type} />
            </Card.Body>
          </Card>
          <Card>
            <Card.Body>
              {pendingCertificate.signers.length === 0 ? (
                <div>No Signers registered</div>
              ) : (
                <Accordion>
                  <Accordion.Item eventKey="signers">
                    <Accordion.Header>
                      <h4>Signers ({pendingCertificate.signers.length})</h4>
                    </Accordion.Header>
                    <Accordion.Body>
                      <ListGroup variant="flush">
                        <ListGroup.Item>
                          <b>Pools</b>
                        </ListGroup.Item>
                        {pendingCertificate.signers.map((signer) => (
                          <ListGroup.Item key={signer.party_id}>
                            <PoolTicker partyId={signer.party_id} aggregator={aggregator} />
                            <br />
                            <PartyId partyId={signer.party_id} />
                            {signer.verification_key_signature && (
                              <div className="float-end">
                                <VerifiedBadge tooltip="Verified Signer" />
                              </div>
                            )}
                          </ListGroup.Item>
                        ))}
                      </ListGroup>
                    </Accordion.Body>
                  </Accordion.Item>
                  <Accordion.Item eventKey="next-signers">
                    <Accordion.Header>
                      <h4>Next Signers ({pendingCertificate.next_signers.length})</h4>
                    </Accordion.Header>
                    <Accordion.Body>
                      <ListGroup variant="flush">
                        <ListGroup.Item>
                          <b>Pools</b>
                        </ListGroup.Item>
                        {pendingCertificate.next_signers.map((signer) => (
                          <ListGroup.Item key={signer.party_id}>
                            <PoolTicker partyId={signer.party_id} aggregator={aggregator} />
                            <br />
                            <PartyId partyId={signer.party_id} />
                            {signer.verification_key_signature && (
                              <div className="float-end">
                                <VerifiedBadge tooltip="Verified Signer" />
                              </div>
                            )}
                          </ListGroup.Item>
                        ))}
                      </ListGroup>
                    </Accordion.Body>
                  </Accordion.Item>
                </Accordion>
              )}
            </Card.Body>
          </Card>
        </CardGroup>
      )}
    </div>
  );
}
