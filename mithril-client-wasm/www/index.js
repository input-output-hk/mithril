// TODO: improve the management of function returned values by adding an event mechanism
// Errors should be handled too.

import { MithrilClient } from "@mithril-dev/mithril-client-wasm"

let aggregator_endpoint =
  "https://aggregator.testing-preview.api.mithril.network/aggregator"
let genesis_verification_key =
  "5b3132372c37332c3132342c3136312c362c3133372c3133312c3231332c3230372c3131372c3139382c38352c3137362c3139392c3136322c3234312c36382c3132332c3131392c3134352c31332c3233322c3234332c34392c3232392c322c3234392c3230352c3230352c33392c3233352c34345d"

const broadcast_channel = new BroadcastChannel("mithril-client");
broadcast_channel.onmessage = (e) => {
  let event = e.data;
  if (event.type == "CertificateChainValidationStarted") {
    displayMessageInDOM(event.type, "The certificate chain validation has started");
  } else if (event.type == "CertificateValidated") {
    displayMessageInDOM(event.type, "A certificate has been validated, certificate_hash: " + event.payload.certificate_hash);
  } else if (event.type == "CertificateChainValidated") {
    displayMessageInDOM(event.type, "The certificate chain is valid");
  } else {
    displayMessageInDOM(event);
  }
};

function displayMessageInDOM(subject, message) {
  let div = document.createElement('div');
  div.innerHTML = '<strong>' + subject + ': </strong>' + message;
  document.body.appendChild(div);
  window.scrollTo(0, document.body.scrollHeight);
  console.log(message);
}

function displayStepInDOM(number, content) {
  let div = document.createElement('div');
  div.innerHTML = '<h2><span style="color: green;"> Step n°' + number + ': </span></h2>' + '<i>' + content + '<i>';
  document.body.appendChild(div);
}

let client = await new MithrilClient(
  aggregator_endpoint,
  genesis_verification_key
)

displayStepInDOM(1, "Getting stake distributions list...");
let mithril_stake_distributions_list = await client.list_mithril_stake_distributions();
displayMessageInDOM("Result", "got " + mithril_stake_distributions_list.length + " stake distributions &#x2713;");
console.log("stake distributions:", mithril_stake_distributions_list);

let last_mithril_stake_distribution = mithril_stake_distributions_list[0];
console.log("last_mithril_stake_distribution:", last_mithril_stake_distribution);

displayStepInDOM(2, "Getting last stake distribution with hash: " + last_mithril_stake_distribution.hash + "...");
let last_stake_distribution = await client.get_mithril_stake_distribution(last_mithril_stake_distribution.hash);
displayMessageInDOM("Result", "got last stake distribution &#x2713;");
console.log("last_stake_distribution:", last_stake_distribution);

displayStepInDOM(3, "Getting Mithril certificate from certificate hash: " + last_stake_distribution.certificate_hash + "...");
let certificate = await client.get_mithril_certificate(last_stake_distribution.certificate_hash);
displayMessageInDOM("Result", "got Mithril certificate &#x2713;");
console.log("certificate:", certificate);

displayStepInDOM(4, "Verifying certificate chain...");
let last_certificate_from_chain = await client.verify_certificate_chain(certificate.hash);
displayMessageInDOM("Result", "certificate chain verified &#x2713;");
console.log("verify_certificate_chain OK, last_certificate_from_chain:", last_certificate_from_chain);

displayStepInDOM(5, "Computing the Mithril stake distribution message...");
let mithril_stake_distributions_message = await client.compute_mithril_stake_distribution_message(last_stake_distribution);
displayMessageInDOM("Result", "Mithril stake distribution message computed &#x2713;");
console.log("mithril_stake_distributions_message:", mithril_stake_distributions_message);

displayStepInDOM(6, "Validating Mithril stake distribution message...");
let valid_stake_distribution_message = await client.verify_message_match_certificate(mithril_stake_distributions_message, last_certificate_from_chain);
displayMessageInDOM("Result", "Mithril stake distribution message validated &#x2713;");
console.log("valid_stake_distribution_message:", valid_stake_distribution_message);