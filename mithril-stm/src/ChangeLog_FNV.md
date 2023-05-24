# Full Node Verifier Progress log

## Method

### Full node verifier struct

```rust
/// FullNodeVerifier structure, that contains keys of all eligible signers.
pub struct FullNodeVerifier {
    /// Ordered list of registered parties.
    pub elligible_parties: Vec<RegParty>,
    // Maybe indexed HashMap
    /// Total stake of the registered parties.
    pub total_stake: Stake,
}
```

### Verification for a full node:

For each sig in in_sigs do:

- Check lottery
- Get PK from given index
- aggregate sigs to accumulator
- aggregate pks to accumulator
- verify aggregated sig wrt aggregated pk.

## Progress

### 2023-05-23

* First decision was to have a different rust file for FNV. However, there is some uncertainties about how to get AVK.
  So, for now the implementation is moved to `stm.rs`.
* **Update:** Full node verifier does not use merkle tree, commitment, and registration information. So, we will
  have `FullNodeSigner`, `FullNodeSignature` and their implementations. Stm signer and stm signature will depend on them
  respectively.

    * The struct `FullNodeSigner` is added, it does not include closed register.
    * The struct `FullNodeSignature` is added, it does not include signer index.

    * The implementation of `StmInitializer`:
        * `check_initializer()`: Checks whether initializer is actually registered and returns the index of the signer
          if registered.
        * `new_signer()`: Runs `check_initializer()`, if check is done, gets the index and
          returns the `StmSigner`.
        * `new_full_node_signer()`: Runs `check_initializer()`, if check is done, gets the index and returns
          the `FullNodeSigner`.

    * The implementation of `FullNodeSigner()`:
        * `check_lottery()`: Checks whether any of the indices won the lottery for given parameters, message, total
          stake, sigma and signer stake. Returns the winning indices.
        * `sign()`: Generates `sigma` for `msg` and `sk`. Calls `FullNodeSigner::check_lottery` if indices are not 
          empty, returns the `FullNodeSignature`.

    * The implementation of `StmSigner()`:
        * `sign()`: Computes `msgp` by concatenating commitment with message. Generates `sigma` for `msgp` and `sk`. 
          Calls `FullNodeSigner::check_lottery` if indices are not empty, returns the `StmSig`.

    * The implementation of `FullNodeSignature`:
        * `check_indices()`: Checks the indices of the `FullNodeSignature` for given total_stake, msg, parameters and
          signer stake.
        * `verify()`: Verifies sigma and checks the indices.
        * `from_stm_sig()`: Returns `FullNodeSignature` for given `StmSig`.

    * The implementation of `StmSig`:
        * `check_indices()`: Will be removed
        * `verify()`: Computes `msgp` by concatenating commitment with message. Verifies `sigma`, checks indices by 
          calling `FullNodeSignature::check_indices`.
