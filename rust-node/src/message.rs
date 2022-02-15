use serde::{Deserialize, Serialize};

pub type Index = u64;
pub type PartyId = u64;
pub type Stake = u64;
pub type Bytes = Vec<u8>;
pub type PublicKey = Bytes;


// --------------------------------------------------------


#[derive(Serialize, Deserialize, Clone)]
pub struct Signature {
  index : Index,
  sig : Bytes,
}

#[derive(Serialize, Deserialize, Clone)]
pub struct Participant {
  party_id : PartyId,
  stake : Stake,
  public_key : PublicKey,
}

#[derive(Serialize, Deserialize, Clone)]
pub struct Parameters {
  pub k : u64,
  pub m : u64,
  pub phi_f : f64,
}

#[derive(Serialize, Deserialize, Clone)]
pub struct Certificate {
  id : u64,
  node_id : u64,
  hash : Bytes,
  prev_hash : Bytes,
  participants : Vec<Participant>,
  block_number : u64,
  block_hash : Bytes,
  merkle_root : Bytes,
  multi_sig : Bytes,
  started_at : u64,
  finished_at : u64,
}

#[derive(Serialize, Deserialize, Clone)]
pub enum Message {
  Ok(),
  Hello(Hello),
  SigRequest(SigRequest),
  SigResponse(SigResponse),
}

#[derive(Serialize, Deserialize, Clone)]
pub struct Hello {
  pub cardano_address : String,
  pub party_id : PartyId,
  pub stake : Stake,
  pub public_key : PublicKey,
}

#[derive(Serialize, Deserialize, Clone)]
pub struct SigRequest {
  pub id : u64,
  pub params : Parameters,
  participants : Vec<Participant>,
  certificate : Certificate,
}

#[derive(Serialize, Deserialize, Clone)]
pub struct SigResponse {
  request_id : u64,
  signatures : Vec<Signature>,
}

