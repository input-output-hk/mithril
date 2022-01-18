#include <stdarg.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>

/**
 * Structure generated out of a closed registration. One can only get a global `avk` out of
 * a closed key registration.
 */
typedef struct ClosedKeyReg_C__H ClosedKeyReg_C__H;

/**
 * Simple struct that collects pubkeys and stakes of parties. Placeholder for a more
 * realistic distributed key registration protocol.
 */
typedef struct KeyReg_C KeyReg_C;

/**
 * Tree of hashes, providing a commitment of data and its ordering.
 */
typedef struct MerkleTree_MTValue_C_____H MerkleTree_MTValue_C_____H;

/**
 * MSP public key, contains the verification key and proof of posession.
 */
typedef struct MspPk_C MspPk_C;

/**
 * MSP secret key.
 */
typedef struct MspSk_C MspSk_C;

/**
 * `StmClerk` can verify and aggregate `StmSig`s and verify `StmMultiSig`s. Clerks can only be
 * generated with the registration closed. This avoids that a Merkle Tree is computed before
 * all parties have registered.
 */
typedef struct StmClerk_H__C__TrivialEnv StmClerk_H__C__TrivialEnv;

/**
 * Initializer for `StmSigner`. This is the data that is used during the key registration
 * procedure. One the latter is finished, this instance is consumed into an `StmSigner`.
 */
typedef struct StmInitializer_C StmInitializer_C;

/**
 * Aggregated signature of many parties.
 * Contains proof that it is well-formed.
 */
typedef struct StmMultiSig_C__ConcatProof_C__H__F StmMultiSig_C__ConcatProof_C__H__F;

/**
 * Signature created by a single party who has won the lottery.
 */
typedef struct StmSig_C__F StmSig_C__F;

/**
 * Participant in the protocol. Can sign messages. This instance can only be generated out of
 * an `StmInitializer` and a closed `KeyReg`. This ensures that a `MerkleTree` root is not
 * computed before all participants have registered.
 */
typedef struct StmSigner_H__C StmSigner_H__C;

typedef KeyReg_C *KeyRegPtr;

typedef ClosedKeyReg_C__H *ClosedKeyRegPtr;

typedef StmMultiSig_C__ConcatProof_C__H__F *MultiSigPtr;

typedef StmSig_C__F *SigPtr;

typedef StmClerk_H__C__TrivialEnv *StmClerkPtr;

typedef StmInitializer_C *StmInitializerPtr;

typedef StmSigner_H__C *StmSignerPtr;

typedef MerkleTree_MTValue_C_____H *MerkleTreePtr;

/**
 * Party identifier, unique for each participant in the protocol.
 */
typedef uintptr_t PartyId;

/**
 * The quantity of stake held by a party, represented as a `u64`.
 */
typedef uint64_t Stake;

typedef MspSk_C *MspSkPtr;

typedef MspPk_C *MspPkPtr;

/**
 * Quorum index for signatures.
 * An aggregate signature (`StmMultiSig`) must have at least `k` unique indices.
 */
typedef uint64_t Index;

/**
 * Used to set protocol parameters.
 */
typedef struct {
  /**
   * Security parameter, upper bound on indices
   */
  uint64_t m;
  /**
   * Quorum parameter
   */
  uint64_t k;
  /**
   * `f` in phi(w) = 1 - (1 - f)^w, where w is the stake of a participant.
   */
  double phi_f;
} StmParameters;

int64_t close_registration(KeyRegPtr key_reg, ClosedKeyRegPtr *closed_reg);

/**
 * Frees a multi signature pointer
 */
int64_t free_multi_sig(MultiSigPtr p);

/**
 * Frees a signature pointer
 */
int64_t free_sig(SigPtr p);

/**
 * Frees an STM Clerk pointer
 */
int64_t free_stm_clerk(StmClerkPtr p);

/**
 * Frees an STM initialiser pointer
 */
int64_t free_stm_initializer(StmInitializerPtr p);

/**
 * Frees an STM signer pointer
 */
int64_t free_stm_signer(StmSignerPtr p);

int64_t generate_avk(ClosedKeyRegPtr key_reg, MerkleTreePtr *mk_tree);

int64_t key_registration(uintptr_t n_parties,
                         const PartyId *party_ids,
                         const Stake *party_stakes,
                         KeyRegPtr *key_ref);

/**
 * Given a pointer and its size, deserialize into a MSP secret key
 */
int64_t msp_deserialize_secret_key(uintptr_t key_size, uint8_t *key_bytes, MspSkPtr *output_struct);

/**
 * Given a pointer and its size, deserialize into a MSP verification/public key
 */
int64_t msp_deserialize_verification_key(uintptr_t key_size,
                                         uint8_t *key_bytes,
                                         MspPkPtr *output_struct);

/**
 * Sets *key_bytes to the serialization
 * Sets *key_size to the size of the buffer
 * The caller is responsible for freeing this buffer
 */
int64_t msp_serialize_secret_key(MspSkPtr kptr, uintptr_t *key_size, uint8_t **key_bytes);

/**
 * Sets *key_bytes to the serialization
 * Sets *key_size to the size of the buffer
 * The caller is responsible for freeing this buffer
 */
int64_t msp_serialize_verification_key(MspPkPtr kptr, uintptr_t *key_size, uint8_t **key_bytes);

/**
 * Register the party. If registration is succesful, returns 0, otherwise returns the
 * following depending on the received error:
 *  -1 if the key is already registered,
 *  -2 if the key is invalid
 *  -3 if the `party_id` is unknown
 *  -4 is unexpected behaviour
 *  NULLPOINTERERR if invalid pointers
 */
int64_t register_party(KeyRegPtr key_reg, PartyId party_id, MspPkPtr party_key);

/**
 * Try to aggregate n_sigs signatures.
 * Sets *sig if successful and returns 0.
 * returns -1 if verification failed
 * returns k > n >= 0 if only n signatures were received (when we needed k).
 */
int64_t stm_clerk_aggregate(StmClerkPtr me,
                            uintptr_t n_sigs,
                            const SigPtr *sigs,
                            const Index *indices,
                            const char *msg,
                            MultiSigPtr *sig);

/**
 * A clerk can only be generated out of a `ClosedKeyReg` instance, or out of an `StmSigner`.
 * This function initialises a `Clerk` out of a `ClosedKeyReg`.
 */
int64_t stm_clerk_from_reg(StmParameters params, ClosedKeyRegPtr closed_reg, StmClerkPtr *clerk);

int64_t stm_clerk_from_signer(StmSignerPtr signer, StmClerkPtr *clerk);

/**
 * Try to verify a multisignature.
 * returns 0 if the signature is valid
 * returns -1 if the aggregation is invalid
 * returns n > 0 if the proof verification failed, where n is the error number
 * from the proof system.
 */
int64_t stm_clerk_verify_msig(StmClerkPtr me, MultiSigPtr msig, const char *msg);

/**
 * Try to verify a signature.
 * returns:
 *  0 if the signature is valid
 *  -1 if the lottery win is false
 *  -2 if the Merkle Tree is invalid
 *  -3 if the MSP signature is invalid
 *  NULLPOINTERERR if invalid pointers
 */
int64_t stm_clerk_verify_sig(StmClerkPtr me, SigPtr sig, Index index, const char *msg);

/**
 * Given a pointer and its size, deserialize into an STM initializer
 */
int64_t stm_deserialize_initializer(uintptr_t init_size,
                                    uint8_t *init_bytes,
                                    StmInitializerPtr *output_struct);

/**
 * Given a pointer and its size, deserialize into an STM multi signature
 */
int64_t stm_deserialize_multi_sig(uintptr_t sig_size,
                                  uint8_t *sig_bytes,
                                  MultiSigPtr *output_struct);

/**
 * Given a pointer and its size, deserialize into an STM signature
 */
int64_t stm_deserialize_sig(uintptr_t sig_size, uint8_t *sig_bytes, SigPtr *output_struct);

int64_t stm_initailizer_generate_new_key(StmInitializerPtr me);

/**
 * This function consumes the `StmInitializer`. This ensures that after the registration is
 * closed, there is no more mangling of the data of the registered party (such as stake or
 * keys). The closed registration is consumed, to minimise the possibilities of misusing
 * it to initialise a new signer.
 */
int64_t stm_initializer_new_signer(StmInitializerPtr me,
                                   ClosedKeyRegPtr closed_reg,
                                   StmSignerPtr *signer);

int64_t stm_initializer_params(StmInitializerPtr me, StmParameters *params);

int64_t stm_initializer_party_id(StmInitializerPtr me, PartyId *party_id);

int64_t stm_initializer_secret_key(StmInitializerPtr me, MspSkPtr *sk);

int64_t stm_initializer_set_keys(StmInitializerPtr me, MspSkPtr key);

int64_t stm_initializer_set_params(StmInitializerPtr me, StmParameters params);

int64_t stm_initializer_set_stake(StmInitializerPtr me, Stake stake);

int64_t stm_initializer_stake(StmInitializerPtr me, Stake *stake);

int64_t stm_initializer_verification_key(StmInitializerPtr me, MspPkPtr *pk);

int64_t stm_intializer_setup(StmParameters params,
                             PartyId party_id,
                             Stake stake,
                             StmInitializerPtr *stm_initializer);

/**
 * Sets *init_bytes to the serialization
 * Sets *init_size to the size of the buffer
 * The caller is responsible for freeing this buffer
 */
int64_t stm_serialize_initializer(StmInitializerPtr init_ptr,
                                  uintptr_t *init_size,
                                  uint8_t **init_bytes);

/**
 * Sets *msig_bytes to the serialization
 * Sets *msig_size to the size of the buffer
 * The caller is responsible for freeing this buffer
 */
int64_t stm_serialize_multi_sig(MultiSigPtr msig_ptr, uintptr_t *msig_size, uint8_t **msig_bytes);

/**
 * Sets *sig_bytes to the serialization
 * Sets *sig_size to the size of the buffer
 * The caller is responsible for freeing this buffer
 */
int64_t stm_serialize_sig(SigPtr sptr, uintptr_t *sig_size, uint8_t **sig_bytes);

/**
 * Checks if a signer is eligible to sign. Returns 0 on success, -1 on failure, or -99 if
 * pointers are invalid.
 */
int64_t stm_signer_eligibility_check(StmSignerPtr me, const char *msg, Index index);

/**
 * Move to a new epoch. This happens when the parameters (such as signers or stake
 * distribution) change. Returns a new StmInitializer with the updated stake of the signer in
 * question, and consumes the StmSigner.
 */
int64_t stm_signer_new_epoch(StmSignerPtr me, Stake new_stake, StmInitializerPtr *stm_initializer);

/**
 * Try to sign a message. Sets *out to point to the signature if successful.
 */
int64_t stm_signer_sign(StmSignerPtr me, const char *msg, Index index, SigPtr *out);
