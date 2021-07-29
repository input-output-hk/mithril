use super::{Unknown, Stake, PartyId, scaling_function};
use crate::key_reg::KeyReg;
use crate::msp::{MSP, SK};
use crate::merkle_tree::MerkleTree;

pub struct Party {
    party_id: PartyId,
    stake: Stake,
    rs: crate::ref_str::ReferenceString,
    avk: Option<MerkleTree>,
    sk: Option<SK>,
}

impl Party {
    //////////////////////////
    // Initialization phase //
    //////////////////////////
    pub fn setup(party_id: PartyId, stake: Stake) -> Self {
        Self {
            party_id,
            stake,
            rs: crate::ref_str::get_reference_string(),
            avk: None,
            sk: None,
        }
    }

    pub fn register(&mut self, kr: &mut KeyReg) {
        // (msk_i, mvk_i, k_i) <- MSP.Gen(Param)
        // (vk_i, sk_i) := ((mvk_i, k_i), msk_i)
        // send (Register, sid, vk_i) to F_KR
        let (sk, pk) = MSP::gen();
        self.sk = Some(sk);
        kr.register(self.party_id, self.stake, pk);
    }

    pub fn retrieve_all(&mut self, kr: &KeyReg) {
        // Reg := (K(P_i), stake_i)
        // Reg is padded to length N using null entries of stake 0
        // AVK <- MT.Create(Reg)
        let reg = kr.retrieve_all();
        let avk = MerkleTree::create(&reg);
        self.avk = Some(avk);
    }

    /////////////////////
    // Operation phase //
    /////////////////////
    pub fn eligibility_check(&self, msg: &[u8], index: Unknown) -> bool {
        // let msg' <- AVK || msg
        // sigma <- MSP.Sig(msk, msg')
        // ev <- MSP.Eval(msg', index, sigma)
        // return 1 if ev < phi(stake) else return 0
        let mut msgp = msg.to_vec();
        let mut avk_bytes = self.avk.as_ref().unwrap().to_bytes();
        msgp.append(&mut avk_bytes);
        let sigma = MSP::sig(self.sk.as_ref().unwrap(), &msgp);
        let ev = MSP::eval(&msgp, index, &sigma);
        ev < scaling_function(self.stake)
    }

    pub fn create_sig(&self, msg: &[u8], index: Unknown) -> Option<Unknown> {
        if self.eligibility_check(msg, index) {
            // msg' <- AVK||msg
            // sigma <- MSP.Sig(msk,msg')
            // pi = (sigma, reg_i, i, p_i) where
            //      p_i is the users path inside the merkle tree AVK
            //      reg_i is (mvk_i, stake_i)
            // return pi
            Some(0)
        } else {
            None
        }
    }

    // TODO
    pub fn verify() {}
    pub fn aggregate() {}
    pub fn verify_aggregate() {}
}
