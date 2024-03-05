import React, { useEffect, useState } from "react";
import { Container, Modal } from "react-bootstrap";

export default function CertifyCardanoTransactionsModal({ transactionHashes, ...props }) {
  useEffect(() => {
    if (!transactionHashes) {
      return;
    }
  }, [transactionHashes]);

  function close() {
    props.onHashesChange([]);
  }

  return (
    <Modal
      show={transactionHashes !== undefined && transactionHashes.length > 0}
      onHide={close}
      size="xl"
      aria-labelledby="contained-modal-title-vcenter"
      centered>
      <Modal.Header className="text-break" closeButton>
        <Modal.Title id="contained-modal-title-vcenter">
          Cardano transaction certification
        </Modal.Title>
      </Modal.Header>

      <Modal.Body>
        <Container></Container>
      </Modal.Body>
      <Modal.Footer></Modal.Footer>
    </Modal>
  );
}
