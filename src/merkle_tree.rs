use super::{Stake, Index, Path, PartyId};
use crate::msp::PK;
use neptune::poseidon::SimplePoseidonBatchHasher;
use typenum::U2;

pub struct MerkleTree {
    h: SimplePoseidonBatchHasher<U2>,
}

impl MerkleTree {
    pub fn create(items: &[Option<(PK, Stake)>]) -> Self {
        unimplemented!()
    }

    pub fn check(&self, item: (PK,Stake), index: Index, path: Path) -> bool {
        unimplemented!()
    }

    pub fn to_bytes(&self) -> Vec<u8> {
        unimplemented!()
    }

    pub fn concat_with_msg(&self, msg: &[u8]) -> Vec<u8> {
        let mut msgp = msg.to_vec();
        let mut avk_bytes = self.to_bytes();
        msgp.append(&mut avk_bytes);
        msgp
    }

    pub fn get_path(&self, party: PartyId) -> Path {
        unimplemented!()
    }
}
