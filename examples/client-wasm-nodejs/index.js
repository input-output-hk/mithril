import { MithrilClient } from "@mithril-dev/mithril-client-wasm";

let aggregator_endpoint = "https://aggregator.release-preprod.api.mithril.network/aggregator";
let genesis_verification_key =
  "5b3132372c37332c3132342c3136312c362c3133372c3133312c3231332c3230372c3131372c3139382c38352c3137362c3139392c3136322c3234312c36382c3132332c3131392c3134352c31332c3233322c3234332c34392c3232392c322c3234392c3230352c3230352c33392c3233352c34345d";
let certificate_chain_validated_occurs = false;

const broadcast_channel = new BroadcastChannel("mithril-client");
broadcast_channel.onmessage = (e) => {
  let event = e.data;
  if (event.type === "CertificateChainValidationStarted") {
    console.log(
      `The certificate chain validation has started, event_id: ${event.payload.certificate_chain_validation_id}`,
    );
  } else if (event.type === "CertificateValidated") {
    console.log(
      `A certificate has been validated, certificate_hash: ${event.payload.certificate_hash}, event_id: ${event.payload.certificate_chain_validation_id}`,
    );
  } else if (event.type === "CertificateChainValidated") {
    certificate_chain_validated_occurs = true;
    console.log(
      `The certificate chain is valid, event_id: ${event.payload.certificate_chain_validation_id}`,
    );
  } else {
    console.log(event);
  }
};

async function main() {
  async function waitUntilCertificateChainValidatedOccursOrTimeout() {
    for (let i = 0; i < 100 && !certificate_chain_validated_occurs; i++) {
      await new Promise((r) => setTimeout(r, 100));
    }
    certificate_chain_validated_occurs = false;
  }

  let client = new MithrilClient(aggregator_endpoint, genesis_verification_key, {
    // The following header is set as an example.
    // It's used to demonstrate how to add headers.
    http_headers: new Map([["Content-Type", "application/json"]]),
    // The following option activates the unstable features of the client.
    // Unstable features will trigger an error if this option is not set.
    unstable: true,
  });

  console.log(1, "Getting stake distributions list...");
  let mithril_stake_distributions_list = await client.list_mithril_stake_distributions();
  console.log(
    "Result",
    "got " + mithril_stake_distributions_list.length + " stake distributions ✔️",
  );
  console.log("stake distributions:", mithril_stake_distributions_list);
  let last_mithril_stake_distribution = mithril_stake_distributions_list[0];

  console.log(
    2,
    "Getting last stake distribution with hash: " + last_mithril_stake_distribution.hash + "...",
  );
  let last_stake_distribution = await client.get_mithril_stake_distribution(
    last_mithril_stake_distribution.hash,
  );
  console.log("Result", "got last stake distribution ✔️");
  console.log("last_stake_distribution:", {
    epoch: last_stake_distribution.epoch,
    signers: last_stake_distribution.signers.map((s) => `${s.party_id} - ${s.stake} lovelace`),
  });

  console.log(
    3,
    "Getting Mithril certificate from certificate hash: " +
      last_stake_distribution.certificate_hash +
      "...",
  );
  let certificate = await client.get_mithril_certificate(last_stake_distribution.certificate_hash);
  console.log("Result", "got Mithril certificate ✔️");
  console.log(
    "certificate:",
    certificate.hash,
    "epoch:",
    certificate.epoch,
    "signed_entity:",
    certificate.signed_entity_type,
  );

  console.log(4, "Verifying certificate chain...");
  let last_certificate_from_chain = await client.verify_certificate_chain(certificate.hash);
  await waitUntilCertificateChainValidatedOccursOrTimeout();
  console.log("Result", "certificate chain verified ✔️");
  console.log(
    "verify_certificate_chain OK, last_certificate_from_chain:",
    last_certificate_from_chain.hash,
  );

  console.log(5, "Computing the Mithril stake distribution message...");
  let mithril_stake_distributions_message = await client.compute_mithril_stake_distribution_message(
    last_stake_distribution,
    last_certificate_from_chain,
  );
  console.log("Result", "Mithril stake distribution message computed ✔️");
  console.log("mithril_stake_distributions_message:", mithril_stake_distributions_message);

  console.log(6, "Validating Mithril stake distribution message...");
  let valid_stake_distribution_message = await client.verify_message_match_certificate(
    mithril_stake_distributions_message,
    last_certificate_from_chain,
  );
  console.log("Result", "Mithril stake distribution message validated ✔️");
  console.log("valid_stake_distribution_message:", valid_stake_distribution_message);

  console.log(7, "Getting transaction proof...");
  const proof = await client.get_cardano_transaction_proofs([
    "0ea207ab71493f012faab0d1f8151eaf931901141c1482ce6e9a501498076484",
    "326b5b67d926937bf19c6113d0957a39f2eae9df94875ce8a96eff5c8521303b",
    "320c13f4a3e51f6f4f66fcd9007e02bf658aa4ee9a88a509028d867d3b8a8e9a",
  ]);
  console.log("Certificate hash of the returned proof", proof.certificate_hash);
  console.log("Transactions hashes included in the proof:", proof.transactions_hashes);
  console.log("Transactions hashes not included in the proof:", proof.non_certified_transactions);

  console.log(9, "Verifying certificate chain...");
  let proof_certificate = await client.verify_certificate_chain(proof.certificate_hash);
  await waitUntilCertificateChainValidatedOccursOrTimeout();
  console.log("Result", "certificate chain verified ✔️");
  console.log("verify_certificate_chain OK, last_certificate_from_chain:", proof_certificate?.hash);

  console.log(10, "Validating Cardano transaction proof message...");
  let protocol_message = await client.verify_cardano_transaction_proof_then_compute_message(
    proof,
    proof_certificate,
  );
  console.log("Ensure that the proof is indeed signed in the associated certificate");
  if (
    (await client.verify_message_match_certificate(protocol_message, proof_certificate)) === true
  ) {
    console.log("Result", "The proof is signed in the associated certificate ✔️");
  } else {
    console.log("Result", "Proof and certificate don't match ❌");
  }
  console.log("Transactions hashes certified", proof.transactions_hashes);
  console.log("Transactions hashes not certified", proof.non_certified_transactions);

  process.exit(0);
}

await main();
