use super::{Stake, PartyId, Index, Path, ev_lt_phi, Phi};
use crate::key_reg::KeyReg;
use crate::msp::{Msp, MspMvk, MspSig, MspPk, MspSk};
use crate::merkle_tree::MerkleTree;
use crate::proof::{ConcatProof, Witness};

static PHI: Phi = Phi(0.5); // TODO: Figure out how/when this gets configued
static M: u64 = 10; // TODO: ????

pub struct StmParty {
    party_id: PartyId,
    stake: Stake,
    avk: Option<MerkleTree>,
    sk: Option<MspSk>,
    pk: Option<MspPk>,
    reg: Option<Vec<Option<(MspPk, Stake)>>>, // map from PID -> (PK,Stake)
    total_stake: Option<Stake>,
}

#[derive(Clone)]
pub struct StmSig {
    pub sigma: MspSig,
    pub pk: MspPk,
    pub party: PartyId,
    pub stake: Stake,
    pub path: Path,
}

#[derive(Clone)]
pub struct StmMultiSig {
    ivk: MspMvk,
    mu: MspSig,
    proof: ConcatProof,
}

impl StmParty {
    //////////////////////////
    // Initialization phase //
    //////////////////////////
    pub fn setup(party_id: PartyId, stake: Stake) -> Self {
        Self {
            party_id,
            stake,
            avk: None,
            sk: None,
            pk: None,
            reg: None,
            total_stake: None,
        }
    }

    pub fn register(&mut self, kr: &mut KeyReg) {
        // (msk_i, mvk_i, k_i) <- MSP.Gen(Param)
        // (vk_i, sk_i) := ((mvk_i, k_i), msk_i)
        // send (Register, sid, vk_i) to F_KR
        let (sk, pk) = Msp::gen();
        self.sk = Some(sk);
        self.pk = Some(pk.clone());
        kr.register(self.party_id, self.stake, pk);
    }

    pub fn retrieve_all(&mut self, kr: &KeyReg) {
        // Reg := (K(P_i), stake_i)
        // Reg is padded to length N using null entries of stake 0
        // AVK <- MT.Create(Reg)
        self.reg = Some(kr.retrieve_all());
        // get total stake
        self.total_stake = Some(self.reg.as_ref().unwrap().iter().filter_map(|p| p.map(|(_,s)|s)).sum());
    }

    // Creating a MerkleTree is expensive. Only do it if you have to.
    pub fn create_avk(&mut self) {
        let avk: MerkleTree = MerkleTree::create(self.reg.as_ref().unwrap());
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
        let sigma = Msp::sig(self.sk.as_ref().unwrap(), &msgp);
        let ev = Msp::eval(&msgp, index, &sigma);
        ev_lt_phi(PHI, ev, self.stake, self.total_stake.unwrap())
    }

    pub fn create_sig(&self, msg: &[u8], index: Index) -> Option<StmSig> {
        if self.eligibility_check(msg, index) {
            // msg' <- AVK||msg
            // sigma <- MSP.Sig(msk,msg')
            // pi = (sigma, reg_i, i, p_i) where
            //      p_i is the users path inside the merkle tree AVK
            //      reg_i is (mvk_i, stake_i)
            // return pi
            let msgp = self.avk.as_ref().unwrap().concat_with_msg(msg);
            let sigma = Msp::sig(self.sk.as_ref().unwrap(), &msgp);
            let path = self.avk.as_ref().unwrap().get_path(self.party_id);
            let pk = self.pk.as_ref().unwrap().clone();
            Some(StmSig {
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

    pub fn verify(&self, sig: StmSig, index: Index, msg: &[u8]) -> bool {
        let avk = self.avk.as_ref().unwrap();
        let msgp = avk.concat_with_msg(msg);
        let ev = Msp::eval(&msgp, index, &sig.sigma);
        if !ev_lt_phi(PHI, ev, sig.stake, self.total_stake.unwrap()) ||
            !avk.check(&(sig.pk.clone(), sig.stake), sig.party, &sig.path)
        {
            return false;
        }
        Msp::ver(&msgp, &sig.pk.mvk, &sig.sigma)
    }

    pub fn aggregate(&self, sigs: &[StmSig], indices: &[Index], msg: &[u8]) -> Option<StmMultiSig> {
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
            evals.push(Msp::eval(&msgp, *ix, &sig.sigma));
        }
        let mvks = sigs.iter().map(|sig| sig.pk.mvk).collect::<Vec<_>>();
        let sigmas = sigs.iter().map(|sig| sig.sigma).collect::<Vec<_>>();
        let ivk = Msp::aggregate_keys(&mvks);
        let mu = Msp::aggregate_sigs(msg, &sigmas);
        let witness = Witness {
            sigs: sigs.to_vec(),
            indices: indices.to_vec(),
            evals,
        };
        let proof = ConcatProof::prove(avk, &ivk, msg, &witness);
        Some(StmMultiSig {
            ivk,
            mu,
            proof,
        })
    }

    pub fn verify_aggregate(&self, msig: &StmMultiSig, msg: &[u8]) -> bool {
        let avk = self.avk.as_ref().unwrap();
        if !msig.proof.verify(PHI, self.total_stake.unwrap(), M, avk, &msig.ivk, msg) {
            return false;
        }
        let msgp = avk.concat_with_msg(msg);
        Msp::aggregate_ver(&msgp, &msig.ivk, &msig.mu)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn setup_parties(nparties: usize) -> Vec<StmParty> {
        let mut kr = KeyReg::new();
        let mut ps = (0..nparties).map(|pid| {
            let mut p = StmParty::setup(pid, 1);
            p.register(&mut kr);
            p
        }).collect::<Vec<StmParty>>();
        for p in ps.iter_mut() {
            p.retrieve_all(&kr);
        }
        ps
    }

    #[test]
    fn test_sig() {
        let nparties = 128;
        let ntries = 100;
        let msg = rand::random::<[u8;16]>();
        let mut ps = setup_parties(nparties);
        let p = &mut ps[rand::random::<usize>() % nparties];
        p.create_avk();
        let mut index = 0;
        for _ in 0..ntries {
            if let Some(sig) = p.create_sig(&msg, index) {
                assert!(p.verify(sig, index, &msg));
                index += 1;
            }
        }
    }

    #[test]
    fn test_aggregate_sig() {
        for _ in 0..16 {
            let nparties = 16;
            let msg = rand::random::<[u8;16]>();
            let mut ps = setup_parties(nparties);
            ps.iter_mut().for_each(StmParty::create_avk);
            let mut sigs = Vec::new();
            let mut ixs = Vec::new();
            let mut ix = 0;
            for p in &ps {
                if let Some(sig) = p.create_sig(&msg, ix) {
                    sigs.push(sig);
                    ixs.push(ix);
                    ix += 1;
                }
            }
            if sigs.len() > 0 {
                let msig = ps[0].aggregate(&sigs, &ixs, &msg).unwrap();
                assert!(ps[1].verify_aggregate(&msig, &msg));
            }
        }
    }
}
