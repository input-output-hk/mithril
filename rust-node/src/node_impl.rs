use crate::message::{Message, Hello, SigRequest, SigResponse, PartyId, Parameters, Stake};
use crate::network::Network;
use std::collections::HashMap;
use std::io::Cursor;
use std::io;
use std::time;
use std::io::Write;

use mithril::key_reg;
use mithril::stm;
use mithril::msp;
use rand_core;
use rand_chacha::ChaCha20Rng;
use rand_core::SeedableRng;
use ark_bls12_377::Bls12_377;
use ark_std;
use ark_ec;
use ark_ff;
use ark_ff::ToBytes;

const timeout : time::Duration = time::Duration::from_secs(5);

type StakeDistribution = HashMap<PartyId, Stake>;
type ValidationKey = msp::MspPk<Bls12_377>;
type H = blake2::Blake2b;

fn as_backend_params(p: Parameters) -> stm::StmParameters {
  stm::StmParameters {
    m: p.m,
    k: p.k,
    phi_f: p.phi_f
  }
}

fn bytes_to_key_io<PE>(bytes: &Vec<u8>) -> io::Result<msp::MspPk<PE>>
  where PE: ark_ec::PairingEngine
{
  let mut rdr = Cursor::new(bytes);
  let mvk = ark_ff::FromBytes::read(&mut rdr)?;
  let k1 = ark_ff::FromBytes::read(&mut rdr)?;
  let k2 = ark_ff::FromBytes::read(&mut rdr)?;

  Ok(msp::MspPk {
    mvk: mvk,
    k1: k1,
    k2: k2,
  })
}

fn str_err_result<R, E>(r: Result<R, E>) -> Result<R, String>
  where E : std::fmt::Display
{
  match r {
    Ok(v) => Ok(v),
    Err(e) => Err(e.to_string()),
  }
}

fn bytes_to_key<PE>(bytes: &Vec<u8>) -> Result<msp::MspPk<PE>, String>
  where PE: ark_ec::PairingEngine
{
  str_err_result(bytes_to_key_io(bytes))
}

fn key_to_bytes<PE>(key: &msp::MspPk<PE>) -> Vec<u8>
  where PE: ark_ec::PairingEngine
{
  let mut buf: Vec<u8> = Vec::new();
  let mut writer: Cursor<&mut Vec<u8>> = Cursor::new(&mut buf);
  key.mvk.write(&mut writer).unwrap();
  key.k1.write(&mut writer).unwrap();
  key.k2.write(&mut writer).unwrap();

  buf
}

fn mk_keyreg<PE>(stake_dist: &StakeDistribution, keys: &HashMap<PartyId, msp::MspPk<PE>>) -> Result<key_reg::ClosedKeyReg<PE, H>, String>
  where PE : ark_ec::PairingEngine,
        msp::MspPk<PE> : std::hash::Hash,
{
  // TODO: cleanup
  let keyreg_players: Vec<(usize, u64)> =
    stake_dist.iter()
              .map(|(p, s)| (*p as usize, *s))
              .collect();

  let mut keyreg: key_reg::KeyReg<PE> = key_reg::KeyReg::new(&keyreg_players);

  for(pid, stake) in stake_dist.iter() {
    let pk =
      keys.get(pid).map_or(Err("key not found for party in stake distrimbution"),
                        |key| Ok(key))?;
    keyreg.register(*pid as usize, *pk);
  }

  return Ok(keyreg.close());
}

fn node_impl<N>(network: N, params: Parameters, stake_dist: StakeDistribution) -> Result<(), String>
  where N : Network
{
  // -- Setup phase -----------------------------------------------------------

  let mut rng = ChaCha20Rng::from_entropy();
  let params_be = as_backend_params(params);
  // fixme
  let me_usize : usize = network.me().try_into().unwrap();
  let stake: Stake =
    stake_dist.get(&network.me())
              .map_or(Err("invalid stake distribution"), |s| Ok(*s))?;

  let init: stm::StmInitializer<Bls12_377> =
    stm::StmInitializer::setup(params_be, me_usize, stake, &mut rng);

  // -- Registration phase ----------------------------------------------------

  let hello =
    Message::Hello(Hello {
      cardano_address: "???".to_string(), // TODO
      party_id: network.me(),
      stake: stake,
      public_key: key_to_bytes(&init.verification_key()),
    });

  network.send(&hello)?;

  let mut peer_hello : HashMap<PartyId, Hello> = HashMap::new();
  let mut pks: HashMap<PartyId, ValidationKey> = HashMap::new();
  let mut queue : Vec<(PartyId, Message)> = Vec::new();

  // recv `Hello` until all peers have responded
  while !network.peers().iter().all(|p| peer_hello.contains_key(p)) {
    // TODO: configurable timeout
    let (from, msg) = network.recv(timeout)?;
    match msg {
      Message::Hello(h) => {
        // TODO: what should happen if we get two `Hello` messages?
        if !peer_hello.contains_key(&from) {
          pks.insert(from, bytes_to_key(&h.public_key)?);
          peer_hello.insert(from, h);
        }
      }

      _ => {
        // handle this message later
        queue.push((from, msg.clone()));
      }
    }
  }

  let key_reg = mk_keyreg(&stake_dist, &pks)?;
  let signer = init.new_signer(key_reg);

  // -- Asynchronous Phase ----------------------------------------------------

  todo!("not implemented!")

}

