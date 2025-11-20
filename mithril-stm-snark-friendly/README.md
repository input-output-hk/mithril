## SNARK-friendly STM

Proposed implementation plan for a SNARK-friendly version of the Mithril STM protocol:

> **Note**: This should not introduce backward compatibility with existing protocol artifacts (signatures, keys, proofs, ...)

- **Step 1**: Code re-organization
  - Create a `commitment_scheme` module and move `merkle_tree` in it
  - Create a `signature_scheme` module and move `bls_multi_signature` and `schnorr_signature` in it
  - Create a `proof_system` module and move `concatenation_proof` in it
  - Create a `core` or `protocol` module and move all the other modules in it
- **Step 2**: Code simplification
  - Move errors to their belonging modules
  - Remove the `future_proof_system` feature and replace it with the existing `future_snark` feature
  - Limit usage of `<D: Digest>` generics to `merkle_tree` module only _(to be rechallenged)_
  - Remove `BasicVerifier` implementation (and create a ticket for supporting this feature in a cleaner way in the future?) _(to be rechallenged)_
- **Step 3**: Implement SNARK-friendly changes for concatenation proof
  - Update the Merkle tree to support multiple leaves (for concatenation and following proof systems)
  - Update the key registration to support the new Merkle tree structure
  - Move responsibility of creating single signature artifacts to the concatenation proof system (in the `proof_system` module)
- **Step 4**: Implement SNARK pre-aggregation primitives (i.e. not the circuit(s) themselves)
  - Create a `halo2` proof system sub-module in the `proof_system` module (it will embed all the halo2 circuits -non-recursive, recursive, etc.- in the future)
  - Implement the creation of the single signature artifacts using schnorr signatures
  - Adapt the key registration to support the new `halo2` proof system
