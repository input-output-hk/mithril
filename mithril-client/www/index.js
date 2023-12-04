import { Client as MithrilClient } from "mithril-client/mithril_client"

let aggregator_endpoint =
  "https://aggregator.testing-preview.api.mithril.network/aggregator"
let genesis_verification_key =
  "5b3139312c36362c3134302c3138352c3133382c31312c3233372c3230372c3235302c3134342c32372c322c3138382c33302c31322c38312c3135352c3230342c31302c3137392c37352c32332c3133382c3139362c3231372c352c31342c32302c35372c37392c33392c3137365d"

let client = await new MithrilClient(
  aggregator_endpoint,
  genesis_verification_key
)
let result = await client.list_mithril_stake_distributions()
console.log("stake distributions:", result)

/* let msd = result[0]
console.log(`Checking ${msd.hash} validity ...`)
await client
  .mithril_stake_distribution_validate(msd.hash, genesis_verification_key)
  .then(() => console.log("success"))
  .catch((error) => {
    console.error("Validate:", error)
  }) */
