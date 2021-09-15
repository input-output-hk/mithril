use super::{Stake, PartyId, Index, Path, ev_lt_phi, Phi};
use crate::key_reg::KeyReg;
use crate::msp::{self, MSP};
use crate::merkle_tree::{MerkleTree, MerkleHashConstants};
use crate::proof::{ConcatProof, Witness};

static PHI: Phi = Phi(0.5); // TODO: Figure out how/when this gets configued

pub struct Party<'l> {
    party_id: PartyId,
    stake: Stake,
    rs: crate::ref_str::ReferenceString,
    avk: Option<MerkleTree<'l, typenum::U2>>,
    sk: Option<msp::SK>,
    pk: Option<msp::PK>,
    consts: &'l MerkleHashConstants<typenum::U2>,
    total_stake: Option<Stake>,
}

#[derive(Clone)]
pub struct Sig {
    sigma: msp::Sig,
    pk: msp::PK,
    party: PartyId,
    stake: Stake,
    path: Path,
}

#[derive(Clone)]
pub struct MultiSig {
    ivk: msp::MVK,
    mu: msp::Sig,
    proof: ConcatProof,
}

impl<'l> Party<'l> {
    //////////////////////////
    // Initialization phase //
    //////////////////////////
    pub fn setup(party_id: PartyId, stake: Stake, consts: &'l MerkleHashConstants<typenum::U2>) -> Self {
        Self {
            party_id,
            stake,
            rs: crate::ref_str::get_reference_string(),
            avk: None,
            sk: None,
            pk: None,
            consts: consts,
            total_stake: None,
        }
    }

    pub fn register(&mut self, kr: &mut KeyReg) {
        // (msk_i, mvk_i, k_i) <- MSP.Gen(Param)
        // (vk_i, sk_i) := ((mvk_i, k_i), msk_i)
        // send (Register, sid, vk_i) to F_KR
        let (sk, pk) = MSP::gen();
        self.sk = Some(sk);
        self.pk = Some(pk.clone());
        kr.register(self.party_id, self.stake, pk);
    }

    pub fn retrieve_all(&mut self, kr: &KeyReg) {
        // Reg := (K(P_i), stake_i)
        // Reg is padded to length N using null entries of stake 0
        // AVK <- MT.Create(Reg)
        let reg = kr.retrieve_all();
        // get total stake
        self.total_stake = Some(reg.iter().filter_map(|p| p.map(|(_,s)|s)).sum());
        let avk: MerkleTree<'l,typenum::U2> = MerkleTree::create(self.consts, &reg);
        self.avk = Some(avk);
    }

    /////////////////////
    // Operation phase //
    /////////////////////
    pub fn eligibility_check(&self, msg: &[u8], index: Index) -> bool {
        // let msg' <- AVK || msg
        // sigma <- MSP.Sig(msk, msg')
        // ev <- MSP.Eval(msg', index, sigma)
        // return 1 if ev < phi(stake) else return 0
        let msgp = self.avk.as_ref().unwrap().concat_with_msg(msg);
        let sigma = MSP::sig(self.sk.as_ref().unwrap(), &msgp);
        let ev = MSP::eval(&msgp, index, &sigma);
        ev_lt_phi(PHI, ev, self.stake, self.total_stake.unwrap())
    }

    pub fn create_sig(&self, msg: &[u8], index: Index) -> Option<Sig> {
        if self.eligibility_check(msg, index) {
            // msg' <- AVK||msg
            // sigma <- MSP.Sig(msk,msg')
            // pi = (sigma, reg_i, i, p_i) where
            //      p_i is the users path inside the merkle tree AVK
            //      reg_i is (mvk_i, stake_i)
            // return pi
            let msgp = self.avk.as_ref().unwrap().concat_with_msg(msg);
            let sigma = MSP::sig(self.sk.as_ref().unwrap(), &msgp);
            let path = self.avk.as_ref().unwrap().get_path(self.party_id);
            let pk = self.pk.as_ref().unwrap().clone();
            Some(Sig {
                sigma,
                pk,
                party: self.party_id,
                stake: self.stake,
                path,
            })
        } else {
            None
        }
    }

    pub fn verify(&self, sig: Sig, index: Index, msg: &[u8]) -> bool {
        let avk = self.avk.as_ref().unwrap();
        let msgp = avk.concat_with_msg(msg);
        let ev = MSP::eval(&msgp, index, &sig.sigma);
        if !ev_lt_phi(PHI, ev, sig.stake, self.total_stake.unwrap()) ||
            todo!() // !avk.check((sig.pk.clone(), sig.stake), index, sig.path)
        {
            return false;
        }
        MSP::ver(&msgp, &sig.pk.mvk, &sig.sigma)
    }

    pub fn aggregate(&self, sigs: &[Sig], indices: &[Index], msg: &[u8]) -> Option<MultiSig> {
        let avk = self.avk.as_ref().unwrap();
        let msgp = avk.concat_with_msg(msg);
        let mut seen_parties = std::collections::HashSet::new();
        let mut evals = Vec::new();
        for (sig, ix) in sigs.iter().zip(indices.iter()) {
            if !self.verify(sig.clone(), *ix, msg) ||
                seen_parties.contains(&sig.party)
            {
                return None;
            }
            seen_parties.insert(sig.party);
            evals.push(MSP::eval(&msgp, *ix, &sig.sigma));
        }
        let mvks = sigs.iter().map(|sig| sig.pk.mvk).collect::<Vec<_>>();
        let sigmas = sigs.iter().map(|sig| sig.sigma).collect::<Vec<_>>();
        let ivk = MSP::aggregate_keys(&mvks);
        let mu = MSP::aggregate_sigs(msg, &sigmas);
        let witness = Witness {
            sigs: sigs.to_vec(),
            indices: indices.to_vec(),
            evals,
        };
        let proof = ConcatProof::prove(avk, &ivk, msg, &witness);
        Some(MultiSig {
            ivk,
            mu,
            proof,
        })
    }

    pub fn verify_aggregate(&self, msig: &MultiSig, msg: &[u8]) -> bool {
        let avk = self.avk.as_ref().unwrap();
        if !msig.proof.verify(avk, &msig.ivk, msg) {
            return false;
        }
        let msgp = avk.concat_with_msg(msg);
        MSP::aggregate_ver(&msgp, &msig.ivk, &msig.mu)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_sig() {
        let nparties = 4;
        let msg = rand::random::<[u8;16]>();
        let cs = crate::merkle_tree::new_constants();
        let mut kr = KeyReg::new();
        let mut ps = (0..nparties).map(|pid| {
            let mut p = Party::setup(0, pid, &cs);
            p.register(&mut kr);
            p
        }).collect::<Vec<_>>();
        let p = &mut ps[0];
        p.retrieve_all(&kr);
    }
}
