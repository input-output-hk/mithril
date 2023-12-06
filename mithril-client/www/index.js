import { Client as MithrilClient } from "mithril-client/mithril_client"

let aggregator_endpoint =
  "https://aggregator.testing-preview.api.mithril.network/aggregator"
let genesis_verification_key =
  "5b3132372c37332c3132342c3136312c362c3133372c3133312c3231332c3230372c3131372c3139382c38352c3137362c3139392c3136322c3234312c36382c3132332c3131392c3134352c31332c3233322c3234332c34392c3232392c322c3234392c3230352c3230352c33392c3233352c34345d"

const broadcast_channel = new BroadcastChannel("mithril-client");
broadcast_channel.onmessage = (event) => {
  objectToString(event.data ?? event, '')
};

function objectToString(obj, stack) {
  for (var property in obj) {
    if (obj.hasOwnProperty(property)) {
      if (typeof obj[property] == "object") {
        objectToString(obj[property], stack + '.' + property);
      } else {
        console.log(property + ": " + obj[property]);
        document.body.appendChild(document.createElement('div')).innerHTML = "<b>" + property + "</b>: " + obj[property];
      }
    }
  }
}

let client = await new MithrilClient(
  aggregator_endpoint,
  genesis_verification_key
)
let mithril_stake_distributions_list = await client.list_mithril_stake_distributions();
console.log("stake distributions:", mithril_stake_distributions_list);

let last_mithril_stake_distribution = mithril_stake_distributions_list[0];
console.log("last_mithril_stake_distribution:", last_mithril_stake_distribution);

let last_stake_distribution = await client.get_mithril_stake_distribution(last_mithril_stake_distribution.hash);
console.log("last_stake_distribution:", last_stake_distribution);

let certificate = await client.get_mithril_certificate(last_stake_distribution.certificate_hash);
console.log("certificate:", certificate);

let last_certificate_from_chain = await client.verify_certificate_chain(certificate.hash);
console.log("verify certificate chain OK, last_certificate_from_chain:", last_certificate_from_chain);

let mithril_stake_distributions_message = await client.compute_mithril_stake_distribution_message(last_stake_distribution);
console.log("mithril_stake_distributions_message:", mithril_stake_distributions_message);

let valid_stake_distribution_message = await client.verify_message_match_certificate(mithril_stake_distributions_message, last_certificate_from_chain);
console.log("valid_stake_distribution_message:", valid_stake_distribution_message);