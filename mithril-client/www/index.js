import { Client as MithrilClient } from "mithril-client/mithril_client"

let aggregator_endpoint =
  "https://aggregator.testing-preview.api.mithril.network/aggregator"
let genesis_verification_key =
  "5b3139312c36362c3134302c3138352c3133382c31312c3233372c3230372c3235302c3134342c32372c322c3138382c33302c31322c38312c3135352c3230342c31302c3137392c37352c32332c3133382c3139362c3231372c352c31342c32302c35372c37392c33392c3137365d"

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

let certificate2 = await client.verify_certificate_chain(certificate.hash);
console.log("certificate2:", certificate2);


/* let msd = result[0]
console.log(`Checking ${msd.hash} validity ...`)
await client
  .mithril_stake_distribution_validate(msd.hash, genesis_verification_key)
  .then(() => console.log("success"))
  .catch((error) => {
    console.error("Validate:", error)
  }) */
