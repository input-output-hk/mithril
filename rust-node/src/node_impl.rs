use crate::message::{Message, Hello, SigRequest, SigResponse, PartyId, Parameters, Stake};
use std::collections::HashMap;
use std::io::Cursor;
use std::io;
use mithril::key_reg;
use std::io::Write;
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

trait Network {
  fn me(&self) -> PartyId;
  fn peers(&self) -> Vec<PartyId>;
  // broadcast only?
  fn send(&self, to: &PartyId, message: &Message) -> Result<(), String>;
  fn recv(&self, timeout: u64) -> Result<(PartyId, Message), String>;
}

fn as_backend_params(p: Parameters) -> stm::StmParameters {
  stm::StmParameters {
    m: p.m,
    k: p.k,
    phi_f: p.phi_f
  }
}

fn bytes_to_key<PE>(bytes: &Vec<u8>) -> io::Result<msp::MspPk<PE>>
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

fn node_impl<N>(network: N, params: Parameters, stake: Stake) -> Result<(), String>
  where N : Network
{
  // -- Setup phase -----------------------------------------------------------

  let mut rng = ChaCha20Rng::from_entropy();
  let params_be = as_backend_params(params);
  // fixme
  let me_usize : usize = network.me().try_into().unwrap();

  let init: stm::StmInitializer<Bls12_377> =
    stm::StmInitializer::setup(params_be, me_usize, stake, &mut rng);

  // -- Initialization phase --------------------------------------------------

  let hello =
    Message::Hello(Hello {
      cardano_address: "???".to_string(), // TODO
      party_id: network.me(),
      stake: stake,
      public_key: key_to_bytes(&init.verification_key()),
    });

  for pid in &network.peers() {
    if *pid != network.me() {
      // broadcast
      network.send(pid, &hello)?;
    }
  }

  let mut peer_hello : HashMap<PartyId, Hello> = HashMap::new();
  let mut queue : Vec<(PartyId, Message)> = Vec::new();

  while !network.peers().iter().all(|p| peer_hello.contains_key(p)) {
    // TODO: configurable timeout
    let (from, msg) = network.recv(1000 * 60 * 5)?;
    match msg {
      Message::Hello(h) => {
        // TODO: what should happen if we get two `Hello` messages?
        if !peer_hello.contains_key(&from) {
          peer_hello.insert(from, h);
        }
      }

      _ => {
        // handle this message later
        queue.push((from, msg.clone()));
      }
    }
  }

  todo!("not implemented!")

}

