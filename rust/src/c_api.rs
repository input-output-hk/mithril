//! C api
use crate::atms::{Asig, Avk};
use crate::key_reg::{ClosedKeyReg, KeyReg};
use crate::msp::{Msp, MspSig};
use crate::{
    merkle_tree::{MTHashLeaf, MerkleTree},
    mithril_proof::concat_proofs::{ConcatProof, TrivialEnv},
    msp::{MspPk, MspSk},
    stm::*,
};
use rand_core::OsRng;
use std::ffi::CStr;
use std::os::raw::c_char;

type C = ark_bls12_377::Bls12_377;
type H = blake2::Blake2b;
type F = <H as MTHashLeaf<MTValue<C>>>::F;
type MspSkPtr = *mut MspSk<C>;
type MspPkPtr = *mut MspPk<C>;
type MspSigPtr = *mut MspSig<C>;
type SigPtr = *mut StmSig<C, F>;
type MultiSigPtr = *mut StmMultiSig<C, ConcatProof<C, H, F>>;
type StmInitializerPtr = *mut StmInitializer<C>;
type StmSignerPtr = *mut StmSigner<H, C>;
type StmClerkPtr = *mut StmClerk<H, C, TrivialEnv>;
type MerkleTreePtr = *mut MerkleTree<MTValue<C>, H>;
type KeyRegPtr = *mut KeyReg<C>;
type ClosedKeyRegPtr = *mut ClosedKeyReg<C, H>;
type AvkPtr = *mut Avk<Msp<C>, H>;
type AsigPtr = *mut Asig<Msp<C>, F>;

// A macro would be nice for the below, but macros do not
// seem to work properly with cbindgen:
/// Frees a signature pointer
#[no_mangle]
pub extern "C" fn free_sig(p: SigPtr) {
    assert!(!p.is_null());
    unsafe {
        Box::from_raw(p);
    }
}
/// Frees a multi signature pointer
#[no_mangle]
pub extern "C" fn free_multi_sig(p: MultiSigPtr) {
    assert!(!p.is_null());
    unsafe {
        Box::from_raw(p);
    }
}
/// Frees an STM initialiser pointer
#[no_mangle]
pub extern "C" fn free_stm_initializer(p: StmInitializerPtr) {
    assert!(!p.is_null());
    unsafe {
        Box::from_raw(p);
    }
}
/// Frees an STM signer pointer
#[no_mangle]
pub extern "C" fn free_stm_signer(p: StmSignerPtr) {
    assert!(!p.is_null());
    unsafe {
        Box::from_raw(p);
    }
}
#[no_mangle]
/// Frees an STM Clerk pointer
pub extern "C" fn free_stm_clerk(p: StmClerkPtr) {
    assert!(!p.is_null());
    unsafe {
        Box::from_raw(p);
    }
}

pub mod serialize {
    //! Serialisation functions
    use super::*;
    use ark_ff::{FromBytes, ToBytes};
    use std::{intrinsics::copy_nonoverlapping, slice};

    /// Sets *key_bytes to the serialization
    /// Sets *key_size to the size of the buffer
    /// The caller is responsible for freeing this buffer
    #[no_mangle]
    pub extern "C" fn msp_serialize_verification_key(
        kptr: MspPkPtr,
        key_size: *mut usize,
        key_bytes: *mut *mut u8,
    ) {
        c_serialize(kptr, key_size, key_bytes);
    }
    /// Given a pointer and its size, deserialize into a MSP verification/public key
    #[no_mangle]
    pub extern "C" fn msp_deserialize_verification_key(
        key_size: usize,
        key_bytes: *mut u8,
    ) -> MspPkPtr {
        c_deserialize(key_size, key_bytes)
    }
    /// Sets *key_bytes to the serialization
    /// Sets *key_size to the size of the buffer
    /// The caller is responsible for freeing this buffer
    #[no_mangle]
    pub extern "C" fn msp_serialize_secret_key(
        kptr: MspSkPtr,
        key_size: *mut usize,
        key_bytes: *mut *mut u8,
    ) {
        c_serialize(kptr, key_size, key_bytes);
    }
    /// Given a pointer and its size, deserialize into a MSP secret key
    #[no_mangle]
    pub extern "C" fn msp_deserialize_secret_key(key_size: usize, key_bytes: *mut u8) -> MspSkPtr {
        c_deserialize(key_size, key_bytes)
    }
    /// Sets *sig_bytes to the serialization
    /// Sets *sig_size to the size of the buffer
    /// The caller is responsible for freeing this buffer
    #[no_mangle]
    pub extern "C" fn stm_serialize_sig(
        sptr: SigPtr,
        sig_size: *mut usize,
        sig_bytes: *mut *mut u8,
    ) {
        c_serialize(sptr, sig_size, sig_bytes);
    }
    /// Given a pointer and its size, deserialize into an STM signature
    #[no_mangle]
    pub extern "C" fn stm_deserialize_sig(sig_size: usize, sig_bytes: *mut u8) -> SigPtr {
        c_deserialize(sig_size, sig_bytes)
    }
    /// Sets *msig_bytes to the serialization
    /// Sets *msig_size to the size of the buffer
    /// The caller is responsible for freeing this buffer
    #[no_mangle]
    pub extern "C" fn stm_serialize_multi_sig(
        msig_ptr: MultiSigPtr,
        msig_size: *mut usize,
        msig_bytes: *mut *mut u8,
    ) {
        c_serialize(msig_ptr, msig_size, msig_bytes)
    }
    /// Given a pointer and its size, deserialize into an STM multi signature
    #[no_mangle]
    pub extern "C" fn stm_deserialize_multi_sig(
        sig_size: usize,
        sig_bytes: *mut u8,
    ) -> MultiSigPtr {
        c_deserialize(sig_size, sig_bytes)
    }

    /// Sets *init_bytes to the serialization
    /// Sets *init_size to the size of the buffer
    /// The caller is responsible for freeing this buffer
    #[no_mangle]
    pub extern "C" fn stm_serialize_initializer(
        init_ptr: StmInitializerPtr,
        init_size: *mut usize,
        init_bytes: *mut *mut u8,
    ) {
        c_serialize(init_ptr, init_size, init_bytes)
    }

    /// Given a pointer and its size, deserialize into an STM initializer
    #[no_mangle]
    pub extern "C" fn stm_deserialize_initializer(
        init_size: usize,
        init_bytes: *mut u8,
    ) -> StmInitializerPtr {
        c_deserialize(init_size, init_bytes)
    }

    fn c_serialize<T: ToBytes>(ptr: *mut T, size: *mut usize, out_bytes: *mut *mut u8) {
        unsafe {
            assert!(!ptr.is_null());
            let v = &*ptr;
            let bytes = ark_ff::to_bytes!(v).unwrap();
            let len = bytes.len();
            *size = len;
            let dst = libc::malloc(len) as *mut u8;
            assert!(!dst.is_null());
            copy_nonoverlapping(bytes.as_ptr(), dst, len);
            *out_bytes = dst;
        }
    }

    fn c_deserialize<T: FromBytes>(size: usize, bytes: *const u8) -> *mut T {
        unsafe {
            let val = T::read(slice::from_raw_parts(bytes, size)).unwrap();
            Box::into_raw(Box::new(val))
        }
    }
}

mod initializer {
    use super::*;

    #[no_mangle]
    pub extern "C" fn stm_intializer_setup(
        params: StmParameters,
        party_id: PartyId,
        stake: Stake,
    ) -> StmInitializerPtr {
        let mut rng = OsRng::default();
        Box::into_raw(Box::new(StmInitializer::setup(
            params, party_id, stake, &mut rng,
        )))
    }

    #[no_mangle]
    pub extern "C" fn stm_initailizer_generate_new_key(me: StmInitializerPtr) {
        let mut rng = OsRng::default();
        unsafe {
            assert!(!me.is_null());
            let ref_me = &mut *me;
            ref_me.generate_new_key(&mut rng);
        }
    }

    #[no_mangle]
    pub extern "C" fn stm_initializer_party_id(me: StmInitializerPtr) -> PartyId {
        unsafe {
            assert!(!me.is_null());
            let ref_me = &*me;
            ref_me.party_id()
        }
    }

    #[no_mangle]
    pub extern "C" fn stm_initializer_stake(me: StmInitializerPtr) -> Stake {
        unsafe {
            assert!(!me.is_null());
            let ref_me = &*me;
            ref_me.stake()
        }
    }

    #[no_mangle]
    pub extern "C" fn stm_initializer_params(me: StmInitializerPtr) -> StmParameters {
        unsafe {
            assert!(!me.is_null());
            let ref_me = &mut *me;
            ref_me.params()
        }
    }

    #[no_mangle]
    pub extern "C" fn stm_initializer_secret_key(me: StmInitializerPtr) -> MspSkPtr {
        unsafe {
            assert!(!me.is_null());
            let ref_me = &mut *me;
            Box::into_raw(Box::new(ref_me.secret_key()))
        }
    }

    #[no_mangle]
    pub extern "C" fn stm_initializer_verification_key(me: StmInitializerPtr) -> MspPkPtr {
        unsafe {
            assert!(!me.is_null());
            let ref_me = &mut *me;
            Box::into_raw(Box::new(ref_me.verification_key()))
        }
    }

    #[no_mangle]
    pub extern "C" fn stm_initializer_set_stake(me: StmInitializerPtr, stake: Stake) {
        unsafe {
            assert!(!me.is_null());
            let ref_me = &mut *me;
            ref_me.set_stake(stake);
        }
    }

    #[no_mangle]
    pub extern "C" fn stm_initializer_set_params(me: StmInitializerPtr, params: StmParameters) {
        unsafe {
            assert!(!me.is_null());
            let ref_me = &mut *me;
            ref_me.set_params(params);
        }
    }

    #[no_mangle]
    pub extern "C" fn stm_initializer_set_keys(me: StmInitializerPtr, key_ptr: MspSkPtr) {
        unsafe {
            assert!(!me.is_null());
            assert!(!key_ptr.is_null());
            let key = &*key_ptr;
            let ref_me = &mut *me;
            ref_me.set_key(key);
        }
    }

    /// This function consumes the `StmInitializer`. This ensures that after the registration is
    /// closed, there is no more mangling of the data of the registered party (such as stake or
    /// keys).
    #[no_mangle]
    pub extern "C" fn stm_initializer_new_signer(
        me: StmInitializerPtr,
        closed_reg: ClosedKeyRegPtr,
    ) -> StmSignerPtr {
        unsafe {
            assert!(!me.is_null());
            assert!(!closed_reg.is_null());
            let ref_me = *Box::from_raw(me);
            let ref_reg = &*closed_reg;
            Box::into_raw(Box::new(ref_me.new_signer(ref_reg)))
        }
    }
}

mod signer {
    use super::*;

    #[no_mangle]
    pub extern "C" fn stm_signer_eligibility_check(
        me: StmSignerPtr,
        msg: *const c_char,
        index: Index,
    ) -> bool {
        unsafe {
            assert!(!me.is_null());
            assert!(!msg.is_null());
            let ref_me = &mut *me;
            let msg_str = CStr::from_ptr(msg);
            ref_me.eligibility_check(msg_str.to_bytes(), index)
        }
    }

    /// Try to sign a message. Sets *out to point to the signature if successful.
    #[no_mangle]
    pub extern "C" fn stm_signer_sign(
        me: StmSignerPtr,
        msg: *const c_char,
        index: Index,
        out: *mut SigPtr,
    ) {
        unsafe {
            assert!(!me.is_null());
            assert!(!msg.is_null());
            assert!(!out.is_null());
            let ref_me = &mut *me;
            let msg_str = CStr::from_ptr(msg);
            if let Some(s) = ref_me.sign(msg_str.to_bytes(), index) {
                *out = Box::into_raw(Box::new(s))
            }
        }
    }

    /// Move to a new epoch. This happens when the parameters (such as signers or stake
    /// distribution) change. It consumes the instance of the previous closed registration, and
    /// returns a new StmInitializer with the updated stake of the signer in question.
    #[no_mangle]
    pub extern "C" fn stm_signer_new_epoch(
        me: StmSignerPtr,
        closed_reg: ClosedKeyRegPtr,
        new_stake: Stake,
    ) -> StmInitializerPtr {
        unsafe {
            assert!(!me.is_null());
            assert!(!closed_reg.is_null());
            let me = *Box::from_raw(me);
            let closed_reg = *Box::from_raw(closed_reg);
            Box::into_raw(Box::new(me.new_epoch(closed_reg, Some(new_stake))))
        }
    }
}

mod key_reg {
    use crate::c_api::{ClosedKeyRegPtr, KeyRegPtr, MerkleTreePtr, MspPkPtr};
    use crate::key_reg::{KeyReg, RegisterError};
    use crate::stm::{PartyId, Stake};
    use std::slice;

    #[no_mangle]
    pub extern "C" fn key_registration(
        n_parties: usize,
        party_ids: *const PartyId,
        party_stakes: *const Stake,
    ) -> KeyRegPtr {
        unsafe {
            assert!(!party_ids.is_null());
            assert!(!party_stakes.is_null());
            let ids = slice::from_raw_parts(party_ids, n_parties);
            let stakes = slice::from_raw_parts(party_stakes, n_parties);

            let ids_stake = ids
                .iter()
                .zip(stakes.iter())
                .map(|(id, stake)| (*id, *stake))
                .collect::<Vec<_>>();
            Box::into_raw(Box::new(KeyReg::new(&ids_stake)))
        }
    }

    #[no_mangle]
    /// Register the party. If registration is succesful, returns 0, otherwise returns the
    /// following depending on the received error:
    /// * -1 if the key is already registered,
    /// * -2 if the key is invalid
    /// * -3 if the `party_id` is unknown
    /// * -4 is unexpected behaviour
    pub extern "C" fn register_party(
        key_reg: KeyRegPtr,
        party_id: PartyId,
        party_key: MspPkPtr,
    ) -> i64 {
        unsafe {
            assert!(!key_reg.is_null());
            assert!(!party_key.is_null());
            let ref_key_reg = &mut *key_reg;
            match ref_key_reg.register(party_id, *party_key) {
                Ok(()) => 0,
                Err(RegisterError::KeyRegistered(_)) => -1,
                Err(RegisterError::InvalidKey(_)) => -2,
                Err(RegisterError::UnknownPartyId(_)) => -3,
                _ => -4,
            }
        }
    }

    #[no_mangle]
    pub extern "C" fn generate_avk(key_reg: ClosedKeyRegPtr) -> MerkleTreePtr {
        unsafe {
            assert!(!key_reg.is_null());
            Box::into_raw(Box::new((*key_reg).avk.clone()))
        }
    }

    #[no_mangle]
    pub extern "C" fn close_registration(key_reg: KeyRegPtr) -> ClosedKeyRegPtr {
        unsafe {
            assert!(!key_reg.is_null());
            let ref_key_reg = *Box::from_raw(key_reg);
            Box::into_raw(Box::new(ref_key_reg.close()))
        }
    }
}

mod clerk {
    use super::*;
    use core::slice;

    /// A clerk can only be generated out of a `ClosedKeyReg` instance, or out of an `StmSigner`.
    /// This function initialises a `Clerk` out of a `ClosedKeyReg`.
    #[no_mangle]
    pub extern "C" fn stm_clerk_from_reg(
        params: StmParameters,
        closed_reg: ClosedKeyRegPtr,
    ) -> StmClerkPtr {
        unsafe {
            assert!(!closed_reg.is_null());
            Box::into_raw(Box::new(StmClerk::from_registration(
                params,
                TrivialEnv,
                &*closed_reg,
            )))
        }
    }

    #[no_mangle]
    pub extern "C" fn stm_clerk_from_signer(signer: StmSignerPtr) -> StmClerkPtr {
        unsafe {
            assert!(!signer.is_null());
            let ref_signer = &*signer;
            Box::into_raw(Box::new(StmClerk::from_signer(ref_signer, TrivialEnv)))
        }
    }

    /// Try to verify a signature.
    /// returns 0 if the signature is valid
    /// returns -1 if the lottery win is false
    /// returns -2 if the Merkle Tree is invalid
    /// returns -3 if the MSP signature is invalid
    #[no_mangle]
    pub extern "C" fn stm_clerk_verify_sig(
        me: StmClerkPtr,
        sig: SigPtr,
        index: Index,
        msg: *const c_char,
    ) -> i64 {
        unsafe {
            assert!(!me.is_null());
            assert!(!sig.is_null());
            assert!(!msg.is_null());
            let ref_me = &*me;
            let msg_str = CStr::from_ptr(msg);
            let ref_sig = &*sig;
            let out = ref_me.verify_sig(ref_sig, index, msg_str.to_bytes());
            match out {
                Ok(()) => 0,
                Err(VerificationFailure::LotteryLost) => -1,
                Err(VerificationFailure::InvalidMerkleTree(_)) => -2,
                Err(VerificationFailure::InvalidSignature(_)) => -3,
            }
        }
    }

    /// Try to aggregate n_sigs signatures.
    /// Sets *sig if successful and returns 0.
    /// returns -1 if verification failed
    /// returns k > n >= 0 if only n signatures were received (when we needed k)
    #[no_mangle]
    pub extern "C" fn stm_clerk_aggregate(
        me: StmClerkPtr,
        n_sigs: usize,
        sigs: *const SigPtr,
        indices: *const Index,
        msg: *const c_char,
        sig: *mut MultiSigPtr,
    ) -> i64 {
        unsafe {
            assert!(!me.is_null());
            assert!(!sigs.is_null());
            assert!(!indices.is_null());
            assert!(!msg.is_null());
            assert!(!sig.is_null());

            let ref_me = &*me;
            let sigs = slice::from_raw_parts(sigs, n_sigs)
                .iter()
                .map(|p| (**p).clone())
                .collect::<Vec<_>>();
            let indices = slice::from_raw_parts(indices, n_sigs);
            let msg_str = CStr::from_ptr(msg);
            let aggr = ref_me.aggregate(&sigs, indices, msg_str.to_bytes());
            match aggr {
                Ok(msig) => {
                    *sig = Box::into_raw(Box::new(msig));
                    0
                }
                Err(AggregationFailure::NotEnoughSignatures(n, m)) => n as i64 - m as i64,
            }
        }
    }

    /// Try to verify a multisignature.
    /// returns 0 if the signature is valid
    /// returns -1 if the aggregation is invalid
    /// returns n > 0 if the proof verification failed, where n is the error number
    /// from the proof system.
    #[no_mangle]
    pub extern "C" fn stm_clerk_verify_msig(
        me: StmClerkPtr,
        msig: MultiSigPtr,
        msg: *const c_char,
    ) -> i64 {
        unsafe {
            assert!(!me.is_null());
            assert!(!msig.is_null());
            assert!(!msg.is_null());

            let ref_me = &*me;
            let ref_msig = &*msig;
            let msg_str = CStr::from_ptr(msg);
            let out = ref_me.verify_msig(ref_msig, msg_str.to_bytes());
            match out {
                Ok(()) => 0,
                Err(MultiVerificationFailure::InvalidAggregate(_)) => -1,
                Err(MultiVerificationFailure::ProofError(e)) => e.into(),
            }
        }
    }
}

mod msp {
    use super::*;
    use crate::msp::Msp;

    #[no_mangle]
    pub extern "C" fn msp_generate_keypair(sk_ptr: *mut MspSkPtr, pk_ptr: *mut MspPkPtr) {
        let mut rng = OsRng::default();
        let (sk, pk) = Msp::gen(&mut rng);
        assert!(!sk_ptr.is_null());
        assert!(!pk_ptr.is_null());
        unsafe {
            *sk_ptr = Box::into_raw(Box::new(sk));
            *pk_ptr = Box::into_raw(Box::new(pk));
        }
    }

    #[no_mangle]
    pub extern "C" fn msp_sign(msg_ptr: *const c_char, key_ptr: MspSkPtr) -> MspSigPtr {
        unsafe {
            assert!(!key_ptr.is_null());
            assert!(!msg_ptr.is_null());
            let msg = CStr::from_ptr(msg_ptr);
            let key = *Box::from_raw(key_ptr);
            Box::into_raw(Box::new(Msp::sig(&key, msg.to_bytes())))
        }
    }

    #[no_mangle]
    pub extern "C" fn msp_verify(
        msg_ptr: *const c_char,
        key_ptr: MspPkPtr,
        sig_ptr: MspSigPtr,
    ) -> i64 {
        unsafe {
            assert!(!key_ptr.is_null());
            assert!(!sig_ptr.is_null());

            let msg = CStr::from_ptr(msg_ptr);
            let key = &*key_ptr;
            let sig = &*sig_ptr;
            match Msp::ver(msg.to_bytes(), &key.mvk, sig) {
                true => 0,
                false => -1,
            }
        }
    }
}
mod atms {
    use super::*;
    use crate::atms::AtmsError;
    use core::slice;

    #[no_mangle]
    /// Performs the key aggregation. Returns 0 upon success, or -1 if one of the keys included in
    /// the aggregation have an invalid proof of possession.
    pub extern "C" fn avk_key_aggregation(
        keys: *const MspPkPtr,
        stake: *const Stake,
        nr_signers: usize,
        threshold: usize,
        avk_key: *mut AvkPtr,
    ) -> i64 {
        unsafe {
            assert!(!keys.is_null());
            assert!(!stake.is_null());
            assert!(!avk_key.is_null());
            let stake = slice::from_raw_parts(stake, nr_signers);
            let pks = slice::from_raw_parts(keys, nr_signers)
                .iter()
                .zip(stake.iter())
                .map(|(p, s)| (**p, *s))
                .collect::<Vec<_>>();
            match Avk::new::<F>(&pks, threshold as u64) {
                Ok(k) => {
                    *avk_key = Box::into_raw(Box::new(k));
                    0
                }
                Err(_) => -1,
            }
        }
    }

    #[no_mangle]
    pub extern "C" fn atms_aggregate_sigs(
        sigs_ptr: *const MspSigPtr,
        pks_ptr: *const MspPkPtr,
        avk_ptr: AvkPtr,
        nr_signatures: usize,
    ) -> AsigPtr {
        unsafe {
            assert!(!sigs_ptr.is_null());
            assert!(!pks_ptr.is_null());
            assert!(!avk_ptr.is_null());
            let sigs = slice::from_raw_parts(sigs_ptr, nr_signatures)
                .iter()
                .zip(slice::from_raw_parts(pks_ptr, nr_signatures).iter())
                .map(|(p, k)| ((**k).mvk, **p))
                .collect::<Vec<_>>();
            let avk = &*avk_ptr;
            Box::into_raw(Box::new(Asig::new::<H>(avk, &sigs)))
        }
    }

    #[no_mangle]
    /// Verifies a signature `sig_ptr` under aggregated key `avk_ptr`. Returns:
    /// * 0 upon success
    /// * -1 if the threshold is not reached
    /// * -2 if there were duplicates keys in `self`
    /// * -3 if there is an invalid proof of Merkle Tree membership
    /// * -4 if a key in `self` is not found in `avk_ptr`
    /// * -5 if the signature is invalid
    pub extern "C" fn atms_verify_sig(
        msg_ptr: *const c_char,
        sig_ptr: AsigPtr,
        avk_ptr: AvkPtr,
    ) -> i64 {
        unsafe {
            assert!(!msg_ptr.is_null());
            assert!(!sig_ptr.is_null());
            assert!(!avk_ptr.is_null());
            let msg = CStr::from_ptr(msg_ptr);
            let sig = &*sig_ptr;
            let avk = &*avk_ptr;
            match sig.verify::<H>(msg.to_bytes(), avk) {
                Ok(_) => 0,
                Err(AtmsError::TooMuchOutstandingStake(_)) => -1,
                Err(AtmsError::FoundDuplicates(_)) => -2,
                Err(AtmsError::InvalidMerkleProof(_, _, _)) => -3,
                Err(AtmsError::UnknownKey(_)) => -4,
                Err(AtmsError::InvalidSignature(_)) => -5,
                _ => {
                    panic!("All errors than can happen from sig.verify are covered")
                }
            }
        }
    }
}
