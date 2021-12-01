#include <stdarg.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>

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
 * `StmClerk` can verify and aggregate `StmSig`s and verify `StmMultiSig`s.
 */
typedef struct StmClerk_H__C__TrivialEnv StmClerk_H__C__TrivialEnv;

/**
 * Initializer for `StmSigner`.
 */
typedef struct StmInitializer_C StmInitializer_C;

/**
 * Aggregated signature of many parties.
 * Contains proof that it is well-formed.
 */
typedef struct StmMultiSig_C__ConcatProof_C__H StmMultiSig_C__ConcatProof_C__H;

/**
 * Signature created by a single party who has won the lottery.
 */
typedef struct StmSig_C__F StmSig_C__F;

/**
 * Participant in the protocol. Can sign messages.
 */
typedef struct StmSigner_H__C StmSigner_H__C;

typedef StmMultiSig_C__ConcatProof_C__H *MultiSigPtr;

typedef StmSig_C__F *SigPtr;

typedef StmClerk_H__C__TrivialEnv *StmClerkPtr;

typedef StmInitializer_C *StmInitializerPtr;

typedef StmSigner_H__C *StmSignerPtr;

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

typedef MerkleTree_MTValue_C_____H *MerkleTreePtr;

/**
 * The quantity of stake held by a party, represented as a `u64`.
 */
typedef uint64_t Stake;

/**
 * Party identifier, unique for each participant in the protocol.
 */
typedef uintptr_t PartyId;

/**
 * Frees a multi signature pointer
 */
void free_multi_sig(MultiSigPtr p);

/**
 * Frees a signature pointer
 */
void free_sig(SigPtr p);

/**
 * Frees an STM Clerk pointer
 */
void free_stm_clerk(StmClerkPtr p);

/**
 * Frees an STM initialiser pointer
 */
void free_stm_initializer(StmInitializerPtr p);

/**
 * Frees an STM signer pointer
 */
void free_stm_signer(StmSignerPtr p);

/**
 * Given a pointer and its size, deserialize into a MSP secret key
 */
MspSkPtr msp_deserialize_secret_key(uintptr_t key_size, uint8_t *key_bytes);

/**
 * Given a pointer and its size, deserialize into a MSP verification/public key
 */
MspPkPtr msp_deserialize_verification_key(uintptr_t key_size, uint8_t *key_bytes);

/**
 * Sets *key_bytes to the serialization
 * Sets *key_size to the size of the buffer
 * The caller is responsible for freeing this buffer
 */
void msp_serialize_secret_key(MspSkPtr kptr, uintptr_t *key_size, uint8_t **key_bytes);

/**
 * Sets *key_bytes to the serialization
 * Sets *key_size to the size of the buffer
 * The caller is responsible for freeing this buffer
 */
void msp_serialize_verification_key(MspPkPtr kptr, uintptr_t *key_size, uint8_t **key_bytes);

/**
 * Try to aggregate n_sigs signatures.
 * Sets *sig if successful and returns 0.
 * returns -1 if verification failed
 * returns k > n >= 0 if only n signatures were received (when we needed k)
 */
int64_t stm_clerk_aggregate(StmClerkPtr me,
                            uintptr_t n_sigs,
                            const SigPtr *sigs,
                            const Index *indices,
                            const char *msg,
                            MultiSigPtr *sig);

StmClerkPtr stm_clerk_from_signer(StmSignerPtr signer);

StmClerkPtr stm_clerk_new(StmParameters params, MerkleTreePtr avk, Stake total_stake);

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
 * returns 0 if the signature is valid
 * returns -1 if the lottery win is false
 * returns -2 if the Merkle Tree is invalid
 * returns -3 if the MSP signature is invalid
 */
int64_t stm_clerk_verify_sig(StmClerkPtr me, SigPtr sig, Index index, const char *msg);

/**
 * Given a pointer and its size, deserialize into an STM initializer
 */
StmInitializerPtr stm_deserialize_initializer(uintptr_t init_size, uint8_t *init_bytes);

/**
 * Given a pointer and its size, deserialize into an STM multi signature
 */
MultiSigPtr stm_deserialize_multi_sig(uintptr_t sig_size, uint8_t *sig_bytes);

/**
 * Given a pointer and its size, deserialize into an STM signature
 */
SigPtr stm_deserialize_sig(uintptr_t sig_size, uint8_t *sig_bytes);

void stm_initailizer_generate_new_key(StmInitializerPtr me);

StmSignerPtr stm_initializer_new_signer(StmInitializerPtr me,
                                        uintptr_t n_parties,
                                        const PartyId *party_ids,
                                        const Stake *party_stakes,
                                        const MspPkPtr *party_keys);

StmParameters stm_initializer_params(StmInitializerPtr me);

PartyId stm_initializer_party_id(StmInitializerPtr me);

MspSkPtr stm_initializer_secret_key(StmInitializerPtr me);

void stm_initializer_set_keys(StmInitializerPtr me, MspSkPtr key_ptr);

void stm_initializer_set_params(StmInitializerPtr me, StmParameters params);

void stm_initializer_set_stake(StmInitializerPtr me, Stake stake);

Stake stm_initializer_stake(StmInitializerPtr me);

MspPkPtr stm_initializer_verification_key(StmInitializerPtr me);

StmInitializerPtr stm_intializer_setup(StmParameters params, PartyId party_id, Stake stake);

/**
 * Sets *init_bytes to the serialization
 * Sets *init_size to the size of the buffer
 * The caller is responsible for freeing this buffer
 */
void stm_serialize_initializer(StmInitializerPtr init_ptr,
                               uintptr_t *init_size,
                               uint8_t **init_bytes);

/**
 * Sets *msig_bytes to the serialization
 * Sets *msig_size to the size of the buffer
 * The caller is responsible for freeing this buffer
 */
void stm_serialize_multi_sig(MultiSigPtr msig_ptr, uintptr_t *msig_size, uint8_t **msig_bytes);

/**
 * Sets *sig_bytes to the serialization
 * Sets *sig_size to the size of the buffer
 * The caller is responsible for freeing this buffer
 */
void stm_serialize_sig(SigPtr sptr, uintptr_t *sig_size, uint8_t **sig_bytes);

bool stm_signer_eligibility_check(StmSignerPtr me, const char *msg, Index index);

/**
 * Try to sign a message. Sets *out to point to the signature if successful.
 */
void stm_signer_sign(StmSignerPtr me, const char *msg, Index index, SigPtr *out);
