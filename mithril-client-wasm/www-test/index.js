import initMithrilClient, {
  MithrilClient,
} from "@mithril-dev/mithril-client-wasm"

function display_test_result_in_dom(step_number, step_name, result, error) {
  let div = document.createElement("div")
  div.id = step_name
  div.title = result
  div.innerHTML = `Result test n°${step_number}: ${result}; function_name: ${step_name}${
    error ? `; reason: ${error}` : ""
  }`
  document.body.appendChild(div)
}

function handle_error(step_number, step_name, error) {
  display_test_result_in_dom(step_number, step_name, "FAILED", error)
  console.error(`Error at step ${step_number} (${step_name}):`, error)
  throw new Error(
    `Stopping script due to error at step ${step_number}: ${error}`
  )
}

await initMithrilClient()
const aggregator_endpoint = process.env.AGGREGATOR_ENDPOINT
const genesis_verification_key = process.env.GENESIS_VERIFICATION_KEY
let client
let test_name
let test_number = 1

// Test 'MithrilClient constructor'
try {
  test_name = "constructor"
  client = new MithrilClient(aggregator_endpoint, genesis_verification_key)
  display_test_result_in_dom(test_number, test_name, "OK")
} catch (error) {
  handle_error(test_number, test_name, error)
}

// Test 'list_snapshots' function
let snapshots
try {
  test_number++
  test_name = "list_snapshots"
  snapshots = await client.list_snapshots()
  display_test_result_in_dom(test_number, test_name, "OK")
  console.log("snapshots", snapshots)
} catch (error) {
  handle_error(test_number, test_name, error)
}

// Test 'get_snapshot' function
try {
  test_number++
  test_name = "get_snapshot"
  const snapshot = await client.get_snapshot(snapshots[0].digest)
  display_test_result_in_dom(test_number, test_name, "OK")
  console.log("snapshot", snapshot)
} catch (error) {
  handle_error(test_number, test_name, error)
}

// Test 'list_mithril_stake_distributions' function
let mithril_stake_distributions
try {
  test_number++
  test_name = "list_mithril_stake_distributions"
  mithril_stake_distributions = await client.list_mithril_stake_distributions()
  display_test_result_in_dom(test_number, test_name, "OK")
  console.log("mithril_stake_distributions", mithril_stake_distributions)
} catch (error) {
  handle_error(test_number, test_name, error)
}

// Test 'get_mithril_stake_distribution' function
let mithril_stake_distribution
try {
  test_number++
  test_name = "get_mithril_stake_distribution"
  mithril_stake_distribution = await client.get_mithril_stake_distribution(
    mithril_stake_distributions[0].hash
  )
  display_test_result_in_dom(test_number, test_name, "OK")
  console.log("mithril_stake_distribution", mithril_stake_distribution)
} catch (error) {
  handle_error(test_number, test_name, error)
}

// Test 'get_mithril_certificate' function
let certificate
try {
  test_number++
  test_name = "get_mithril_certificate"
  certificate = await client.get_mithril_certificate(
    mithril_stake_distribution.certificate_hash
  )
  display_test_result_in_dom(test_number, test_name, "OK")
  console.log("certificate", certificate)
} catch (error) {
  handle_error(test_number, test_name, error)
}

// Test 'verify_certificate_chain' function
let last_certificate_from_chain
try {
  test_number++
  test_name = "verify_certificate_chain"
  last_certificate_from_chain = await client.verify_certificate_chain(
    certificate.hash
  )
  display_test_result_in_dom(test_number, test_name, "OK")
  console.log("last_certificate_from_chain", last_certificate_from_chain)
} catch (error) {
  handle_error(test_number, test_name, error)
}

// Test 'compute_mithril_stake_distribution_message' function
let mithril_stake_distribution_message
try {
  test_number++
  test_name = "compute_mithril_stake_distribution_message"
  mithril_stake_distribution_message =
    await client.compute_mithril_stake_distribution_message(
      mithril_stake_distribution
    )
  display_test_result_in_dom(test_number, test_name, "OK")
  console.log(
    "mithril_stake_distribution_message",
    mithril_stake_distribution_message
  )
} catch (error) {
  handle_error(test_number, test_name, error)
}

// Test 'verify_message_match_certificate' function
try {
  test_number++
  test_name = "verify_message_match_certificate"
  const valid_stake_distribution_message =
    await client.verify_message_match_certificate(
      mithril_stake_distribution_message,
      last_certificate_from_chain
    )
  display_test_result_in_dom(test_number, test_name, "OK")
  console.log(
    "valid_stake_distribution_message",
    valid_stake_distribution_message
  )
} catch (error) {
  handle_error(test_number, test_name, error)
}
