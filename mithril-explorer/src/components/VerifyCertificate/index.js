import { Modal } from "react-bootstrap";
import { MithrilClient } from "@mithril-dev/mithril-client-wasm";

export default function VerifyCertificate({ show, onClose, certificateHash }) {
    let aggregatorEdpoint = "https://aggregator.testing-preview.api.mithril.network/aggregator";
    let genesisVerificationKey = "5b3132372c37332c3132342c3136312c362c3133372c3133312c3231332c3230372c3131372c3139382c38352c3137362c3139392c3136322c3234312c36382c3132332c3131392c3134352c31332c3233322c3234332c34392c3232392c322c3234392c3230352c3230352c33392c3233352c34345d";

    let client = new MithrilClient(aggregatorEdpoint, genesisVerificationKey);
    console.log(client);

    return (
    <Modal show={show} onHide={onClose} size="xl" aria-labelledby="contained-modal-title-vcenter" centered>
      <Modal.Header closeButton>
        <Modal.Title>Verify Mithril certificate</Modal.Title>
      </Modal.Header>
      <Modal.Body>
      </Modal.Body>
      <Modal.Footer>
      </Modal.Footer>
    </Modal>
  );
}
