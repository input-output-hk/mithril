#include <stdarg.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>

typedef struct KeyReg_C KeyReg_C;

typedef struct MerkleTree_MTValue_C_____H MerkleTree_MTValue_C_____H;

/**
 * `StmClerk` can verify and aggregate `StmSig`s and verify `StmMultiSig`s.
 */
typedef struct StmClerk_H__C__TrivialEnv StmClerk_H__C__TrivialEnv;

/**
 * Initializer for `StmSigner`.
 */
typedef struct StmInitializer_H__C StmInitializer_H__C;

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

typedef KeyReg_C *KeyRegPtr;

typedef StmMultiSig_C__ConcatProof_C__H *MultiSigPtr;

typedef StmSig_C__F *SigPtr;

typedef StmClerk_H__C__TrivialEnv *StmClerkPtr;

typedef StmInitializer_H__C *StmInitializerPtr;

typedef StmSigner_H__C *StmSignerPtr;

/**
 * Party identifier, unique for each participant in the protocol.
 */
typedef uintptr_t PartyId;

/**
 * The quantity of stake held by a party, represented as a `u64`.
 */
typedef uint64_t Stake;

typedef struct {
  PartyId party_id;
  Stake stake;
} Participant;

typedef const StmSig_C__F *SigConstPtr;

/**
 * Quorum index for signatures.
 * An aggregate signature (`StmMultiSig`) must have at least `k` unique indices.
 */
typedef uint64_t Index;

typedef const StmMultiSig_C__ConcatProof_C__H *MultiSigConstPtr;

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
   * `f` in phi(w) = 1 - (1 - f)^w
   */
  double phi_f;
} StmParameters;

typedef MerkleTree_MTValue_C_____H *MerkleTreePtr;

void free_keyreg(KeyRegPtr p);

void free_multi_sig(MultiSigPtr p);

void free_sig(SigPtr p);

void free_stm_clerk(StmClerkPtr p);

void free_stm_initializer(StmInitializerPtr p);

void free_stm_signer(StmSignerPtr p);

KeyRegPtr key_reg_new(uintptr_t n_participants, const Participant *participants);

/**
 * Try to aggregate n_sigs signatures.
 * Sets *sig if successful and returns 0.
 * returns -1 if verification failed
 * returns k > n >= 0 if only n signatures were received (when we needed k)
 */
int64_t stm_clerk_aggregate(StmClerkPtr me,
                            uintptr_t n_sigs,
                            SigConstPtr sigs,
                            const Index *indices,
                            const char *msg,
                            MultiSigConstPtr *sig);

StmClerkPtr stm_clerk_from_signer(StmSignerPtr signer);

StmClerkPtr stm_clerk_new(StmParameters params, MerkleTreePtr avk, Stake total_stake);

bool stm_clerk_verify_msig(StmClerkPtr me, MultiSigConstPtr msig, const char *msg);

bool stm_clerk_verify_sig(StmClerkPtr me, SigConstPtr sig, Index index, const char *msg);

void stm_initializer_build_avk(StmInitializerPtr me, KeyRegPtr kr);

/**
 * Construct an StmSigner. Frees the StmInitializerPtr.
 */
StmSignerPtr stm_initializer_finish(StmInitializerPtr me);

void stm_initializer_register(StmInitializerPtr me, KeyRegPtr kr);

StmInitializerPtr stm_intializer_setup(StmParameters params, PartyId party_id, Stake stake);

bool stm_signer_eligibility_check(StmSignerPtr me, const char *msg, Index index);

/**
 * Try to sign a message. Sets *out to point to the signature if successful.
 */
void stm_signer_sign(StmSignerPtr me, const char *msg, Index index, SigPtr *out);
