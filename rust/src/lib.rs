pub mod key_reg;
pub mod merkle_tree;
pub mod mithril_curves;
pub mod mithril_proof;
pub mod models;
pub mod msp;
pub mod proof;
pub mod stm;

use ark_ff::{FromBytes, ToBytes};
use std::{
    convert::TryInto,
    io::{Read, Write},
};

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

impl<F: ToBytes> ToBytes for Path<F> {
    fn write<W: Write>(&self, mut writer: W) -> std::io::Result<()> {
        let n: u64 = self.0.len().try_into().unwrap();
        n.write(&mut writer)?;
        for pi in &self.0 {
            pi.write(&mut writer)?;
        }

        Ok(())
    }
}
impl<F: FromBytes> FromBytes for Path<F> {
    fn read<R: Read>(mut reader: R) -> std::io::Result<Self> {
        let n = u64::read(&mut reader)?;
        let mut p = Vec::with_capacity(n as usize);
        for _ in 0..n {
            let pi = F::read(&mut reader)?;
            p.push(pi);
        }

        Ok(Path(p))
    }
}

mod c_api {
    use crate::{
        merkle_tree::{MTHashLeaf, MerkleTree},
        mithril_proof::concat_proofs::{ConcatProof, TrivialEnv},
        msp::{MspPk, MspSk},
        stm::*,
        Index, PartyId, Stake,
    };
    use rand_core::OsRng;
    use std::ffi::CStr;
    use std::os::raw::c_char;

    type C = ark_bls12_377::Bls12_377;
    type H = blake2::Blake2b;
    type F = <H as MTHashLeaf<MTValue<C>>>::F;
    type MspSkPtr = *mut MspSk<C>;
    type MspPkPtr = *mut MspPk<C>;
    type SigPtr = *mut StmSig<C, F>;
    type SigConstPtr = *const StmSig<C, F>;
    type MultiSigPtr = *mut StmMultiSig<C, ConcatProof<C, H>>;
    type MultiSigConstPtr = *const StmMultiSig<C, ConcatProof<C, H>>;
    type StmInitializerPtr = *mut StmInitializer<C>;
    type StmSignerPtr = *mut StmSigner<H, C>;
    type StmClerkPtr = *mut StmClerk<H, C, TrivialEnv>;
    type MerkleTreePtr = *mut MerkleTree<MTValue<C>, H>;

    // A macro would be nice for the below, but macros do not
    // seem to work properly with cbindgen:
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

    pub mod serialize {
        use super::*;
        use ark_ff::{FromBytes, ToBytes};
        use std::slice;

        /// Sets *key_bytes to the serialization
        /// Sets *key_size to the size of the buffer
        #[no_mangle]
        pub extern "C" fn msp_serialize_verification_key(
            kptr: MspPkPtr,
            key_size: *mut usize,
            key_bytes: *mut *const u8,
        ) {
            c_serialize(kptr, key_size, key_bytes);
        }
        #[no_mangle]
        pub extern "C" fn msp_deserialize_verification_key(
            key_size: usize,
            key_bytes: *const u8,
        ) -> MspPkPtr {
            c_deserialize(key_size, key_bytes)
        }
        /// Sets *key_bytes to the serialization
        /// Sets *key_size to the size of the buffer
        #[no_mangle]
        pub extern "C" fn msp_serialize_secret_key(
            kptr: MspSkPtr,
            key_size: *mut usize,
            key_bytes: *mut *const u8,
        ) {
            c_serialize(kptr, key_size, key_bytes);
        }
        #[no_mangle]
        pub extern "C" fn msp_deserialize_secret_key(
            key_size: usize,
            key_bytes: *const u8,
        ) -> MspPkPtr {
            c_deserialize(key_size, key_bytes)
        }
        /// Sets *sig_bytes to the serialization
        /// Sets *sig_size to the size of the buffer
        #[no_mangle]
        pub extern "C" fn stm_serialize_sig(
            sptr: SigPtr,
            sig_size: *mut usize,
            sig_bytes: *mut *const u8,
        ) {
            c_serialize(sptr, sig_size, sig_bytes);
        }
        #[no_mangle]
        pub extern "C" fn stm_deserialize_sig(sig_size: usize, sig_bytes: *const u8) -> SigPtr {
            c_deserialize(sig_size, sig_bytes)
        }
        /// Sets *msig_bytes to the serialization
        /// Sets *msig_size to the size of the buffer
        #[no_mangle]
        pub extern "C" fn stm_serialize_multi_sig(
            msig_ptr: MultiSigPtr,
            msig_size: *mut usize,
            msig_bytes: *mut *const u8,
        ) {
            c_serialize(msig_ptr, msig_size, msig_bytes)
        }

        #[no_mangle]
        pub extern "C" fn stm_deserialize_multi_sig(
            sig_size: usize,
            sig_bytes: *const u8,
        ) -> MultiSigPtr {
            c_deserialize(sig_size, sig_bytes)
        }

        #[no_mangle]
        pub extern "C" fn stm_serialize_initializer(
            init_ptr: StmInitializerPtr,
            init_size: *mut usize,
            init_bytes: *mut *const u8,
        ) {
            c_serialize(init_ptr, init_size, init_bytes)
        }

        #[no_mangle]
        pub extern "C" fn stm_deserialize_initializer(
            init_size: usize,
            init_bytes: *const u8,
        ) -> StmInitializerPtr {
            c_deserialize(init_size, init_bytes)
        }

        fn c_serialize<T: ToBytes>(ptr: *mut T, size: *mut usize, out_bytes: *mut *const u8) {
            unsafe {
                let v = *Box::from_raw(ptr);
                let bytes: Box<Vec<u8>> = Box::new(ark_ff::to_bytes!(v).unwrap());
                *size = bytes.len();
                *out_bytes = bytes.as_ptr();
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
        use crate::key_reg::RegParty;
        use std::slice;

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
                let ref_me = &mut *me;
                ref_me.generate_new_key(&mut rng);
            }
        }

        #[no_mangle]
        pub extern "C" fn stm_initializer_party_id(me: StmInitializerPtr) -> PartyId {
            unsafe {
                let ref_me = &*me;
                ref_me.party_id()
            }
        }

        #[no_mangle]
        pub extern "C" fn stm_initializer_stake(me: StmInitializerPtr) -> Stake {
            unsafe {
                let ref_me = &*me;
                ref_me.stake()
            }
        }

        #[no_mangle]
        pub extern "C" fn stm_initializer_secret_key(me: StmInitializerPtr) -> MspSkPtr {
            unsafe {
                let ref_me = &mut *me;
                Box::into_raw(Box::new(ref_me.secret_key()))
            }
        }

        #[no_mangle]
        pub extern "C" fn stm_initializer_verification_key(me: StmInitializerPtr) -> MspPkPtr {
            unsafe {
                let ref_me = &mut *me;
                Box::into_raw(Box::new(ref_me.verification_key()))
            }
        }

        #[no_mangle]
        pub extern "C" fn stm_initializer_new_signer(
            me: StmInitializerPtr,
            n_parties: usize,
            party_ids: *const PartyId,
            party_stakes: *const Stake,
            party_keys: *const MspPkPtr,
        ) -> StmSignerPtr {
            unsafe {
                let ref_me = &mut *me;
                let ids = slice::from_raw_parts(party_ids, n_parties);
                let stakes = slice::from_raw_parts(party_stakes, n_parties);
                let keys = slice::from_raw_parts(party_keys, n_parties);
                let reg = ids
                    .iter()
                    .zip(stakes)
                    .zip(keys)
                    .map(|((party_id, stake), k)| RegParty {
                        party_id: *party_id,
                        stake: *stake,
                        pk: **k,
                    })
                    .collect::<Vec<_>>();

                Box::into_raw(Box::new(ref_me.new_signer(&reg)))
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
