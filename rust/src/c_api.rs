//! C api. All functions return an i64, with 0 upon success, and -99 if the returned pointer
//! is null. Other error codes are function dependent.
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

pub const NULLPOINTERERR: i64 = -99;

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
pub extern "C" fn free_sig(p: SigPtr) -> i64 {
    unsafe {
        if let Some(p) = p.as_mut() {
            Box::from_raw(p);
            return 0;
        }
        NULLPOINTERERR
    }
}
/// Frees a multi signature pointer
#[no_mangle]
pub extern "C" fn free_multi_sig(p: MultiSigPtr) -> i64 {
    unsafe {
        if let Some(p) = p.as_mut() {
            Box::from_raw(p);
            return 0;
        }
        NULLPOINTERERR
    }
}
/// Frees an STM initialiser pointer
#[no_mangle]
pub extern "C" fn free_stm_initializer(p: StmInitializerPtr) -> i64 {
    unsafe {
        if let Some(p) = p.as_mut() {
            Box::from_raw(p);
            return 0;
        }
        NULLPOINTERERR
    }
}
/// Frees an STM signer pointer
#[no_mangle]
pub extern "C" fn free_stm_signer(p: StmSignerPtr) -> i64 {
    unsafe {
        if let Some(p) = p.as_mut() {
            Box::from_raw(p);
            return 0;
        }
        NULLPOINTERERR
    }
}
#[no_mangle]
/// Frees an STM Clerk pointer
pub extern "C" fn free_stm_clerk(p: StmClerkPtr) -> i64 {
    unsafe {
        if let Some(p) = p.as_mut() {
            Box::from_raw(p);
            return 0;
        }
        NULLPOINTERERR
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
    ) -> i64 {
        c_serialize(kptr, key_size, key_bytes)
    }
    /// Given a pointer and its size, deserialize into a MSP verification/public key
    #[no_mangle]
    pub extern "C" fn msp_deserialize_verification_key(
        key_size: usize,
        key_bytes: *mut u8,
        output_struct: *mut MspPkPtr,
    ) -> i64 {
        c_deserialize(key_size, key_bytes, output_struct)
    }
    /// Sets *key_bytes to the serialization
    /// Sets *key_size to the size of the buffer
    /// The caller is responsible for freeing this buffer
    #[no_mangle]
    pub extern "C" fn msp_serialize_secret_key(
        kptr: MspSkPtr,
        key_size: *mut usize,
        key_bytes: *mut *mut u8,
    ) -> i64 {
        c_serialize(kptr, key_size, key_bytes)
    }
    /// Given a pointer and its size, deserialize into a MSP secret key
    #[no_mangle]
    pub extern "C" fn msp_deserialize_secret_key(
        key_size: usize,
        key_bytes: *mut u8,
        output_struct: *mut MspSkPtr,
    ) -> i64 {
        c_deserialize(key_size, key_bytes, output_struct)
    }
    /// Sets *sig_bytes to the serialization
    /// Sets *sig_size to the size of the buffer
    /// The caller is responsible for freeing this buffer
    #[no_mangle]
    pub extern "C" fn stm_serialize_sig(
        sptr: SigPtr,
        sig_size: *mut usize,
        sig_bytes: *mut *mut u8,
    ) -> i64 {
        c_serialize(sptr, sig_size, sig_bytes)
    }
    /// Given a pointer and its size, deserialize into an STM signature
    #[no_mangle]
    pub extern "C" fn stm_deserialize_sig(
        sig_size: usize,
        sig_bytes: *mut u8,
        output_struct: *mut SigPtr,
    ) -> i64 {
        c_deserialize(sig_size, sig_bytes, output_struct)
    }
    /// Sets *msig_bytes to the serialization
    /// Sets *msig_size to the size of the buffer
    /// The caller is responsible for freeing this buffer
    #[no_mangle]
    pub extern "C" fn stm_serialize_multi_sig(
        msig_ptr: MultiSigPtr,
        msig_size: *mut usize,
        msig_bytes: *mut *mut u8,
    ) -> i64 {
        c_serialize(msig_ptr, msig_size, msig_bytes)
    }
    /// Given a pointer and its size, deserialize into an STM multi signature
    #[no_mangle]
    pub extern "C" fn stm_deserialize_multi_sig(
        sig_size: usize,
        sig_bytes: *mut u8,
        output_struct: *mut MultiSigPtr,
    ) -> i64 {
        c_deserialize(sig_size, sig_bytes, output_struct)
    }

    /// Sets *init_bytes to the serialization
    /// Sets *init_size to the size of the buffer
    /// The caller is responsible for freeing this buffer
    #[no_mangle]
    pub extern "C" fn stm_serialize_initializer(
        init_ptr: StmInitializerPtr,
        init_size: *mut usize,
        init_bytes: *mut *mut u8,
    ) -> i64 {
        c_serialize(init_ptr, init_size, init_bytes)
    }

    /// Given a pointer and its size, deserialize into an STM initializer
    #[no_mangle]
    pub extern "C" fn stm_deserialize_initializer(
        init_size: usize,
        init_bytes: *mut u8,
        output_struct: *mut StmInitializerPtr,
    ) -> i64 {
        c_deserialize(init_size, init_bytes, output_struct)
    }

    fn c_serialize<T: ToBytes>(ptr: *mut T, size: *mut usize, out_bytes: *mut *mut u8) -> i64 {
        unsafe {
            if let (Some(v), Some(size_checked), Some(out_checked)) =
                (ptr.as_ref(), size.as_mut(), out_bytes.as_mut())
            {
                let bytes = ark_ff::to_bytes!(v).unwrap();
                let len = bytes.len();
                *size_checked = len;
                let dst = libc::malloc(len) as *mut u8;
                copy_nonoverlapping(bytes.as_ptr(), dst, len);
                *out_checked = dst;
                return 0;
            }
            NULLPOINTERERR
        }
    }

    fn c_deserialize<T: FromBytes>(size: usize, bytes: *const u8, result: *mut *mut T) -> i64 {
        unsafe {
            if let (Some(res), Some(bytes)) = (result.as_mut(), bytes.as_ref()) {
                let val = T::read(slice::from_raw_parts(bytes, size)).unwrap();
                *res = Box::into_raw(Box::new(val));
                return 0;
            }
            NULLPOINTERERR
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
        stm_initializer: *mut StmInitializerPtr,
    ) -> i64 {
        unsafe {
            let mut rng = OsRng::default();
            if let Some(stm) = stm_initializer.as_mut() {
                *stm = Box::into_raw(Box::new(StmInitializer::setup(
                    params, party_id, stake, &mut rng,
                )));
                return 0;
            }
            NULLPOINTERERR
        }
    }

    #[no_mangle]
    pub extern "C" fn stm_initailizer_generate_new_key(me: StmInitializerPtr) -> i64 {
        let mut rng = OsRng::default();
        unsafe {
            if let Some(ref_me) = me.as_mut() {
                ref_me.generate_new_key(&mut rng);
                return 0;
            }
        }
        NULLPOINTERERR
    }

    #[no_mangle]
    pub extern "C" fn stm_initializer_party_id(
        me: StmInitializerPtr,
        party_id: *mut PartyId,
    ) -> i64 {
        unsafe {
            if let (Some(ref_me), Some(party_id)) = (me.as_ref(), party_id.as_mut()) {
                *party_id = ref_me.party_id();
                return 0;
            }
            NULLPOINTERERR
        }
    }

    #[no_mangle]
    pub extern "C" fn stm_initializer_stake(me: StmInitializerPtr, stake: *mut Stake) -> i64 {
        unsafe {
            if let (Some(ref_me), Some(ref_stake)) = (me.as_ref(), stake.as_mut()) {
                *ref_stake = ref_me.stake();
                return 0;
            }
            NULLPOINTERERR
        }
    }

    #[no_mangle]
    pub extern "C" fn stm_initializer_params(
        me: StmInitializerPtr,
        params: *mut StmParameters,
    ) -> i64 {
        unsafe {
            if let (Some(ref_me), Some(ref_params)) = (me.as_ref(), params.as_mut()) {
                *ref_params = ref_me.params();
                return 0;
            }
            NULLPOINTERERR
        }
    }

    #[no_mangle]
    pub extern "C" fn stm_initializer_secret_key(me: StmInitializerPtr, sk: *mut MspSkPtr) -> i64 {
        unsafe {
            if let (Some(ref_me), Some(ref_sk)) = (me.as_ref(), sk.as_mut()) {
                *ref_sk = Box::into_raw(Box::new(ref_me.secret_key()));
                return 0;
            }
            NULLPOINTERERR
        }
    }

    #[no_mangle]
    pub extern "C" fn stm_initializer_verification_key(
        me: StmInitializerPtr,
        pk: *mut MspPkPtr,
    ) -> i64 {
        unsafe {
            if let (Some(ref_me), Some(ref_pk)) = (me.as_ref(), pk.as_mut()) {
                *ref_pk = Box::into_raw(Box::new(ref_me.verification_key()));
                return 0;
            }
            NULLPOINTERERR
        }
    }

    #[no_mangle]
    pub extern "C" fn stm_initializer_set_stake(me: StmInitializerPtr, stake: Stake) -> i64 {
        unsafe {
            if let Some(ref_me) = me.as_mut() {
                ref_me.set_stake(stake);
                return 0;
            }
            NULLPOINTERERR
        }
    }

    #[no_mangle]
    pub extern "C" fn stm_initializer_set_params(
        me: StmInitializerPtr,
        params: StmParameters,
    ) -> i64 {
        unsafe {
            if let Some(ref_me) = me.as_mut() {
                ref_me.set_params(params);
                return 0;
            }
            NULLPOINTERERR
        }
    }

    #[no_mangle]
    pub extern "C" fn stm_initializer_set_keys(me: StmInitializerPtr, key: MspSkPtr) -> i64 {
        unsafe {
            if let (Some(ref_me), Some(ref_key)) = (me.as_mut(), key.as_ref()) {
                ref_me.set_key(ref_key);
                return 0;
            }
            NULLPOINTERERR
        }
    }

    /// This function consumes the `StmInitializer`. This ensures that after the registration is
    /// closed, there is no more mangling of the data of the registered party (such as stake or
    /// keys). The closed registration is consumed, to minimise the possibilities of misusing
    /// it to initialise a new signer.
    #[no_mangle]
    pub extern "C" fn stm_initializer_new_signer(
        me: StmInitializerPtr,
        closed_reg: ClosedKeyRegPtr,
        signer: *mut StmSignerPtr,
    ) -> i64 {
        unsafe {
            if let (Some(ref_signer), Some(me), Some(closed_reg)) =
                (signer.as_mut(), me.as_mut(), closed_reg.as_mut())
            {
                let ref_me = *Box::from_raw(me);
                let ref_reg = *Box::from_raw(closed_reg);
                *ref_signer = Box::into_raw(Box::new(ref_me.new_signer(ref_reg)));
                return 0;
            }
            NULLPOINTERERR
        }
    }
}

mod signer {
    use super::*;

    /// Checks if a signer is eligible to sign. Returns 0 on success, -1 on failure, or -99 if
    /// pointers are invalid.
    #[no_mangle]
    pub extern "C" fn stm_signer_eligibility_check(
        me: StmSignerPtr,
        msg: *const c_char,
        index: Index,
    ) -> i64 {
        unsafe {
            if let (Some(ref_me), Some(msg)) = (me.as_ref(), msg.as_ref()) {
                let msg_str = CStr::from_ptr(msg);
                if ref_me.eligibility_check(msg_str.to_bytes(), index) {
                    return 0;
                }
                return -1;
            }
            NULLPOINTERERR
        }
    }

    /// Try to sign a message. Sets *out to point to the signature if successful.
    #[no_mangle]
    pub extern "C" fn stm_signer_sign(
        me: StmSignerPtr,
        msg: *const c_char,
        index: Index,
        out: *mut SigPtr,
    ) -> i64 {
        unsafe {
            if let (Some(ref_me), Some(ref_out), Some(msg)) =
                (me.as_ref(), out.as_mut(), msg.as_ref())
            {
                let msg_str = CStr::from_ptr(msg);
                if let Some(s) = ref_me.sign(msg_str.to_bytes(), index) {
                    *ref_out = Box::into_raw(Box::new(s));
                }
                return 0;
            }
            NULLPOINTERERR
        }
    }

    /// Move to a new epoch. This happens when the parameters (such as signers or stake
    /// distribution) change. Returns a new StmInitializer with the updated stake of the signer in
    /// question, and consumes the StmSigner.
    #[no_mangle]
    pub extern "C" fn stm_signer_new_epoch(
        me: StmSignerPtr,
        new_stake: Stake,
        stm_initializer: *mut StmInitializerPtr,
    ) -> i64 {
        unsafe {
            if let (Some(ref_me), Some(ref_stm_initializer)) =
                (me.as_mut(), stm_initializer.as_mut())
            {
                let me = *Box::from_raw(ref_me);
                *ref_stm_initializer = Box::into_raw(Box::new(me.new_epoch(Some(new_stake))));
                return 0;
            }
            NULLPOINTERERR
        }
    }
}

mod key_reg {
    use crate::c_api::{ClosedKeyRegPtr, KeyRegPtr, MerkleTreePtr, MspPkPtr, NULLPOINTERERR};
    use crate::key_reg::{KeyReg, RegisterError};
    use crate::stm::{PartyId, Stake};
    use std::slice;

    #[no_mangle]
    pub extern "C" fn key_registration(
        n_parties: usize,
        party_ids: *const PartyId,
        party_stakes: *const Stake,
        key_ref: *mut KeyRegPtr,
    ) -> i64 {
        unsafe {
            if let (Some(ids), Some(stakes), Some(key)) =
                (party_ids.as_ref(), party_stakes.as_ref(), key_ref.as_mut())
            {
                let ids = slice::from_raw_parts(ids, n_parties);
                let stakes = slice::from_raw_parts(stakes, n_parties);

                let ids_stake = ids
                    .iter()
                    .zip(stakes.iter())
                    .map(|(id, stake)| (*id, *stake))
                    .collect::<Vec<_>>();
                *key = Box::into_raw(Box::new(KeyReg::new(&ids_stake)));
                return 0;
            }
            NULLPOINTERERR
        }
    }

    #[no_mangle]
    /// Register the party. If registration is succesful, returns 0, otherwise returns the
    /// following depending on the received error:
    /// * -1 if the key is already registered,
    /// * -2 if the key is invalid
    /// * -3 if the `party_id` is unknown
    /// * -4 is unexpected behaviour
    /// * NULLPOINTERERR if invalid pointers
    pub extern "C" fn register_party(
        key_reg: KeyRegPtr,
        party_id: PartyId,
        party_key: MspPkPtr,
    ) -> i64 {
        unsafe {
            if let (Some(ref_key_reg), Some(party_key)) = (key_reg.as_mut(), party_key.as_ref()) {
                return match ref_key_reg.register(party_id, *party_key) {
                    Ok(()) => 0,
                    Err(RegisterError::KeyRegistered(_)) => -1,
                    Err(RegisterError::InvalidKey(_)) => -2,
                    Err(RegisterError::UnknownPartyId(_)) => -3,
                    _ => -4,
                };
            }
            NULLPOINTERERR
        }
    }

    #[no_mangle]
    pub extern "C" fn generate_avk(key_reg: ClosedKeyRegPtr, mk_tree: *mut MerkleTreePtr) -> i64 {
        unsafe {
            if let (Some(key_reg), Some(mk_tree)) = (key_reg.as_ref(), mk_tree.as_mut()) {
                *mk_tree = Box::into_raw(Box::new(key_reg.avk.clone()));
                return 0;
            }
            NULLPOINTERERR
        }
    }

    #[no_mangle]
    pub extern "C" fn close_registration(
        key_reg: KeyRegPtr,
        closed_reg: *mut ClosedKeyRegPtr,
    ) -> i64 {
        unsafe {
            if let (Some(key_reg), Some(ref_closed_reg)) = (key_reg.as_mut(), closed_reg.as_mut()) {
                let ref_key_reg = *Box::from_raw(key_reg);
                *ref_closed_reg = Box::into_raw(Box::new(ref_key_reg.close()));
                return 0;
            }
            NULLPOINTERERR
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
        clerk: *mut StmClerkPtr,
    ) -> i64 {
        unsafe {
            if let (Some(ref_closed_reg), Some(ref_clerk)) = (closed_reg.as_mut(), clerk.as_mut()) {
                let closed_reg = *Box::from_raw(ref_closed_reg);
                *ref_clerk = Box::into_raw(Box::new(StmClerk::from_registration(
                    params, TrivialEnv, closed_reg,
                )));
                return 0;
            }
            NULLPOINTERERR
        }
    }

    #[no_mangle]
    pub extern "C" fn stm_clerk_from_signer(signer: StmSignerPtr, clerk: *mut StmClerkPtr) -> i64 {
        unsafe {
            if let (Some(ref_signer), Some(ref_clerk)) = (signer.as_ref(), clerk.as_mut()) {
                *ref_clerk = Box::into_raw(Box::new(StmClerk::from_signer(ref_signer, TrivialEnv)));
                return 0;
            }
            NULLPOINTERERR
        }
    }

    /// Try to verify a signature.
    /// returns:
    /// * 0 if the signature is valid
    /// * -1 if the lottery win is false
    /// * -2 if the Merkle Tree is invalid
    /// * -3 if the MSP signature is invalid
    /// * NULLPOINTERERR if invalid pointers
    ///
    #[no_mangle]
    pub extern "C" fn stm_clerk_verify_sig(
        me: StmClerkPtr,
        sig: SigPtr,
        index: Index,
        msg: *const c_char,
    ) -> i64 {
        unsafe {
            if let (Some(ref_me), Some(msg), Some(ref_sig)) =
                (me.as_ref(), msg.as_ref(), sig.as_ref())
            {
                let msg_str = CStr::from_ptr(msg);
                let out = ref_me.verify_sig(ref_sig, index, msg_str.to_bytes());
                return match out {
                    Ok(()) => 0,
                    Err(VerificationFailure::LotteryLost) => -1,
                    Err(VerificationFailure::InvalidMerkleTree(_)) => -2,
                    Err(VerificationFailure::InvalidSignature(_)) => -3,
                };
            }
            NULLPOINTERERR
        }
    }

    /// Try to aggregate n_sigs signatures.
    /// Sets *sig if successful and returns 0.
    /// returns -1 if verification failed
    /// returns k > n >= 0 if only n signatures were received (when we needed k).
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
            if let (Some(ref_me), Some(sigs), Some(indices), Some(msg), Some(ref_sig)) = (
                me.as_ref(),
                sigs.as_ref(),
                indices.as_ref(),
                msg.as_ref(),
                sig.as_mut(),
            ) {
                let sigs = slice::from_raw_parts(sigs, n_sigs)
                    .iter()
                    .map(|p| (**p).clone())
                    .collect::<Vec<_>>();
                let indices = slice::from_raw_parts(indices, n_sigs);
                let msg_str = CStr::from_ptr(msg);
                let aggr = ref_me.aggregate(&sigs, indices, msg_str.to_bytes());
                return match aggr {
                    Ok(msig) => {
                        *ref_sig = Box::into_raw(Box::new(msig));
                        0
                    }
                    Err(AggregationFailure::NotEnoughSignatures(n, m)) => n as i64 - m as i64,
                };
            }
            NULLPOINTERERR
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
            if let (Some(ref_me), Some(ref_msig), Some(msg)) =
                (me.as_ref(), msig.as_ref(), msg.as_ref())
            {
                let msg_str = CStr::from_ptr(msg);
                let out = ref_me.verify_msig(ref_msig, msg_str.to_bytes());
                return match out {
                    Ok(()) => 0,
                    Err(MultiVerificationFailure::InvalidAggregate(_)) => -1,
                    Err(MultiVerificationFailure::ProofError(e)) => e.into(),
                };
            }
            NULLPOINTERERR
        }
    }
}

mod msp {
    use super::*;
    use crate::msp::Msp;

    #[no_mangle]
    pub extern "C" fn msp_generate_keypair(sk_ptr: *mut MspSkPtr, pk_ptr: *mut MspPkPtr) -> i64 {
        let mut rng = OsRng::default();
        let (sk, pk) = Msp::gen(&mut rng);
        unsafe {
            if let (Some(ref_sk), Some(ref_pk)) = (sk_ptr.as_mut(), pk_ptr.as_mut()) {
                *ref_sk = Box::into_raw(Box::new(sk));
                *ref_pk = Box::into_raw(Box::new(pk));
                return 0;
            }
            NULLPOINTERERR
        }
    }

    #[no_mangle]
    pub extern "C" fn msp_sign(
        msg_ptr: *const c_char,
        key_ptr: MspSkPtr,
        signature_pts: *mut MspSigPtr,
    ) -> i64 {
        unsafe {
            if let (Some(ref_key), Some(ref_msg), Some(ref_sig)) =
                (key_ptr.as_ref(), msg_ptr.as_ref(), signature_pts.as_mut())
            {
                let msg = CStr::from_ptr(ref_msg);
                *ref_sig = Box::into_raw(Box::new(Msp::sig(ref_key, msg.to_bytes())));
                return 0;
            }
            NULLPOINTERERR
        }
    }

    #[no_mangle]
    pub extern "C" fn msp_verify(
        msg_ptr: *const c_char,
        key_ptr: MspPkPtr,
        sig_ptr: MspSigPtr,
    ) -> i64 {
        unsafe {
            if let (Some(ref_msg), Some(ref_key), Some(ref_sig)) =
                (msg_ptr.as_ref(), key_ptr.as_ref(), sig_ptr.as_ref())
            {
                let msg = CStr::from_ptr(ref_msg);
                return match Msp::ver(msg.to_bytes(), &ref_key.mvk, ref_sig) {
                    true => 0,
                    false => -1,
                };
            }
            NULLPOINTERERR
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
            if let (Some(ref_key), Some(ref_stake), Some(ref_avk_key)) =
                (keys.as_ref(), stake.as_ref(), avk_key.as_mut())
            {
                let stake = slice::from_raw_parts(ref_stake, nr_signers);
                let pks = slice::from_raw_parts(ref_key, nr_signers)
                    .iter()
                    .zip(stake.iter())
                    .map(|(p, s)| (**p, *s))
                    .collect::<Vec<_>>();
                return match Avk::new(&pks, threshold as u64) {
                    Ok(k) => {
                        *ref_avk_key = Box::into_raw(Box::new(k));
                        0
                    }
                    Err(_) => -1,
                };
            }
            NULLPOINTERERR
        }
    }

    #[no_mangle]
    pub extern "C" fn atms_aggregate_sigs(
        sigs_ptr: *const MspSigPtr,
        pks_ptr: *const MspPkPtr,
        avk_ptr: AvkPtr,
        nr_signatures: usize,
        aggr_sig: *mut AsigPtr,
    ) -> i64 {
        unsafe {
            if let (Some(ref_sigs), Some(ref_pks), Some(ref_avk), Some(ref_aggr_sig)) = (
                sigs_ptr.as_ref(),
                pks_ptr.as_ref(),
                avk_ptr.as_ref(),
                aggr_sig.as_mut(),
            ) {
                let sigs = slice::from_raw_parts(ref_sigs, nr_signatures)
                    .iter()
                    .zip(slice::from_raw_parts(ref_pks, nr_signatures).iter())
                    .map(|(p, k)| ((**k).mvk, **p))
                    .collect::<Vec<_>>();
                *ref_aggr_sig = Box::into_raw(Box::new(Asig::new::<H>(ref_avk, &sigs)));
                return 0;
            }
            NULLPOINTERERR
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
            if let (Some(ref_msg), Some(ref_sig), Some(ref_avk)) =
                (msg_ptr.as_ref(), sig_ptr.as_ref(), avk_ptr.as_ref())
            {
                let msg = CStr::from_ptr(ref_msg);
                return match ref_sig.verify::<H>(msg.to_bytes(), ref_avk) {
                    Ok(_) => 0,
                    Err(AtmsError::TooMuchOutstandingStake(_)) => -1,
                    Err(AtmsError::FoundDuplicates(_)) => -2,
                    Err(AtmsError::InvalidMerkleProof(_, _, _)) => -3,
                    Err(AtmsError::UnknownKey(_)) => -4,
                    Err(AtmsError::InvalidSignature(_)) => -5,
                    _ => {
                        panic!("All errors than can happen from sig.verify are covered");
                    }
                };
            }
            NULLPOINTERERR
        }
    }
}
