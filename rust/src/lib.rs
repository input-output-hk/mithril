#![allow(dead_code, unused_variables)]

pub mod hashutils;
pub mod key_reg;
pub mod merkle_tree;
pub mod mithril_curves;
pub mod mithril_proof;
pub mod models;
pub mod msp;
pub mod proof;
pub mod stm;

use crate::merkle_tree::{MTHashLeaf, MerkleTree};

/// The quantity of stake held by a party, represented as a `u64`.
pub type Stake = u64;

/// Party identifier, unique for each participant in the protocol.
pub type PartyId = usize;

/// Quorum index for signatures.
/// An aggregate signature (`StmMultiSig`) must have at least `k` unique indices.
pub type Index = u64;

/// Path of hashes from root to leaf in a Merkle Tree.
/// Used to verify the credentials of users and signatures.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Path<F>(Vec<F>);

/// Compares the output of `phi` (a real) to the output of `ev` (a hash).
/// Used to determine winning lottery tickets.
pub fn ev_lt_phi(phi_f: f64, ev: u64, stake: Stake, total_stake: Stake) -> bool {
    //TODO: Fix this, casting to f64 isn't safe
    let w = (stake as f64) / (total_stake as f64);
    let phi = 1.0 - (1.0 - phi_f).powf(w);
    let ev_as_f64 = ev as f64 / 2_f64.powf(64.0);
    // println!("{} {}", phi, ev_as_f64);
    ev_as_f64 < phi
}

pub fn concat_avk_with_msg<L, H>(avk: &MerkleTree<L, H>, msg: &[u8]) -> Vec<u8>
where
    H: MTHashLeaf<L>,
{
    let mut msgp = msg.to_vec();
    let mut bytes = avk.root_to_bytes();
    msgp.append(&mut bytes);

    msgp
}

mod c_api {
    use crate::Index;
    use crate::{
        key_reg::KeyReg,
        merkle_tree::{MTHashLeaf, MerkleTree},
        mithril_proof::concat_proofs::{ConcatProof, TrivialEnv},
        stm::*,
        PartyId, Stake,
    };
    use rand::rngs::OsRng;
    use std::ffi::CStr;
    use std::os::raw::c_char;

    #[repr(C)]
    pub struct Participant {
        party_id: PartyId,
        stake: Stake,
    }

    type C = ark_bls12_377::Bls12_377;
    type H = blake2::Blake2b;
    type F = <H as MTHashLeaf<MTValue<C>>>::F;
    type KeyRegPtr = *mut KeyReg<C>;
    type SigPtr = *mut StmSig<C, F>;
    type SigConstPtr = *const StmSig<C, F>;
    type MultiSigPtr = *mut StmMultiSig<C, ConcatProof<C, H>>;
    type MultiSigConstPtr = *const StmMultiSig<C, ConcatProof<C, H>>;
    type StmInitializerPtr = *mut StmInitializer<H, C>;
    type StmSignerPtr = *mut StmSigner<H, C>;
    type StmClerkPtr = *mut StmClerk<H, C, TrivialEnv>;
    type MerkleTreePtr = *mut MerkleTree<MTValue<C>, H>;
    type ParticipantPtr = *const Participant;

    // A macro would be nice for the below, but macros do not
    // seem to work properly with cbindgen:
    #[no_mangle]
    pub extern "C" fn free_keyreg(p: KeyRegPtr) {
        assert!(!p.is_null());
        unsafe {
            Box::from_raw(p);
        }
    }
    #[no_mangle]
    pub extern "C" fn free_sig(p: SigPtr) {
        assert!(!p.is_null());
        unsafe {
            Box::from_raw(p);
        }
    }
    #[no_mangle]
    pub extern "C" fn free_multi_sig(p: MultiSigPtr) {
        assert!(!p.is_null());
        unsafe {
            Box::from_raw(p);
        }
    }
    #[no_mangle]
    pub extern "C" fn free_stm_initializer(p: StmInitializerPtr) {
        assert!(!p.is_null());
        unsafe {
            Box::from_raw(p);
        }
    }
    #[no_mangle]
    pub extern "C" fn free_stm_signer(p: StmSignerPtr) {
        assert!(!p.is_null());
        unsafe {
            Box::from_raw(p);
        }
    }
    #[no_mangle]
    pub extern "C" fn free_stm_clerk(p: StmClerkPtr) {
        assert!(!p.is_null());
        unsafe {
            Box::from_raw(p);
        }
    }

    mod key_reg {
        use super::*;
        use core::slice;

        #[no_mangle]
        pub extern "C" fn key_reg_new(
            n_participants: usize,
            participants: *const Participant,
        ) -> KeyRegPtr {
            unsafe {
                let ps = slice::from_raw_parts(participants, n_participants)
                    .iter()
                    .map(|p| (p.party_id, p.stake))
                    .collect::<Vec<_>>();
                Box::into_raw(Box::new(KeyReg::new(&ps)))
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
            Box::into_raw(Box::new(StmInitializer::setup(params, party_id, stake)))
        }

        #[no_mangle]
        pub extern "C" fn stm_initializer_register(me: StmInitializerPtr, kr: KeyRegPtr) {
            let mut rng = OsRng::default();
            unsafe {
                let ref_me = &mut *me;
                let ref_kr = &mut *kr;
                ref_me.register(&mut rng, ref_kr);
            }
        }

        #[no_mangle]
        pub extern "C" fn stm_initializer_build_avk(me: StmInitializerPtr, kr: KeyRegPtr) {
            unsafe {
                let ref_me = &mut *me;
                let ref_kr = &mut *kr;
                ref_me.build_avk(ref_kr);
            }
        }

        #[no_mangle]
        /// Construct an StmSigner. Frees the StmInitializerPtr.
        pub extern "C" fn stm_initializer_finish(me: StmInitializerPtr) -> StmSignerPtr {
            unsafe {
                let init = Box::from_raw(me);
                Box::into_raw(Box::new(init.finish()))
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
                let ref_me = &mut *me;
                let msg_str = CStr::from_ptr(msg);
                if let Some(s) = ref_me.sign(msg_str.to_bytes(), index) {
                    *out = Box::into_raw(Box::new(s))
                }
            }
        }
    }

    mod clerk {
        use super::*;
        use core::slice;
        use std::convert::TryInto;

        #[no_mangle]
        pub extern "C" fn stm_clerk_new(
            params: StmParameters,
            avk: MerkleTreePtr,
            total_stake: Stake,
        ) -> StmClerkPtr {
            unsafe {
                Box::into_raw(Box::new(StmClerk::new(
                    params,
                    TrivialEnv,
                    *Box::from_raw(avk),
                    total_stake,
                )))
            }
        }

        #[no_mangle]
        pub extern "C" fn stm_clerk_from_signer(signer: StmSignerPtr) -> StmClerkPtr {
            unsafe {
                let ref_signer = &*signer;
                Box::into_raw(Box::new(StmClerk::from_signer(ref_signer, TrivialEnv)))
            }
        }

        #[no_mangle]
        pub extern "C" fn stm_clerk_verify_sig(
            me: StmClerkPtr,
            sig: SigConstPtr,
            index: Index,
            msg: *const c_char,
        ) -> bool {
            unsafe {
                let ref_me = &*me;
                let msg_str = CStr::from_ptr(msg);
                let ref_sig = &*sig;
                ref_me.verify_sig(ref_sig, index, msg_str.to_bytes())
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
            sigs: SigConstPtr,
            indices: *const Index,
            msg: *const c_char,
            sig: *mut MultiSigConstPtr,
        ) -> i64 {
            unsafe {
                let ref_me = &*me;
                let sigs = slice::from_raw_parts(sigs, n_sigs);
                let indices = slice::from_raw_parts(indices, n_sigs);
                let msg_str = CStr::from_ptr(msg);
                let aggr = ref_me.aggregate(sigs, indices, msg_str.to_bytes());
                match aggr {
                    Ok(msig) => {
                        *sig = Box::into_raw(Box::new(msig));
                        0
                    }
                    Err(AggregationFailure::VerifyFailed) => -1,
                    Err(AggregationFailure::NotEnoughSignatures(n)) => n.try_into().unwrap(),
                }
            }
        }

        #[no_mangle]
        pub extern "C" fn stm_clerk_verify_msig(
            me: StmClerkPtr,
            msig: MultiSigConstPtr,
            msg: *const c_char,
        ) -> bool {
            unsafe {
                let ref_me = &*me;
                let ref_msig = &*msig;
                let msg_str = CStr::from_ptr(msg);
                ref_me.verify_msig(ref_msig, msg_str.to_bytes())
            }
        }
    }
}
