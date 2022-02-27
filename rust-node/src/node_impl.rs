// TODO: remove this later
#![allow(dead_code)]

use crate::message::{Hello, Message, Parameters, PartyId, SigResponse, Signature, Stake};
use crate::network::Network;
use std::collections::HashMap;
use std::io::Cursor;
use log::debug;

use ark_bls12_377::Bls12_377;
use ark_ec;
use ark_ff;
use mithril::key_reg;
use mithril::msp;
use mithril::stm;
use rand_chacha::ChaCha20Rng;
use rand_core;
use rand_core::SeedableRng;

type StakeDistribution = HashMap<PartyId, Stake>;
type ValidationKey = msp::MspPk<Bls12_377>;
type H = blake2::Blake2b;
type Signer = stm::StmSigner<blake2::Blake2b, Bls12_377>;

fn as_backend_params(p: &Parameters) -> stm::StmParameters {
    stm::StmParameters {
        m: p.m,
        k: p.k,
        phi_f: p.phi_f,
    }
}

fn str_err_result<R, E>(r: Result<R, E>) -> Result<R, String>
where
    E: std::fmt::Display,
{
    match r {
        Ok(v) => Ok(v),
        Err(e) => Err(e.to_string()),
    }
}

// TODO: is this function already in ark somewhere?
fn ark_to_bytes<T: ark_ff::ToBytes>(t: T) -> Vec<u8> {
    let mut buf: Vec<u8> = Vec::new();
    let mut writer: Cursor<&mut Vec<u8>> = Cursor::new(&mut buf);
    ark_ff::ToBytes::write(&t, &mut writer).unwrap(); // TODO - can this ever fail?

    buf
}
fn ark_from_bytes<T: ark_ff::FromBytes>(bytes: &Vec<u8>) -> Result<T, String> {
    let rdr = Cursor::new(bytes);
    str_err_result(ark_ff::FromBytes::read(rdr))
}

fn mk_keyreg<PE>(
    stake_dist: &StakeDistribution,
    keys: &HashMap<PartyId, msp::MspPk<PE>>,
) -> Result<key_reg::ClosedKeyReg<PE, H>, String>
where
    PE: ark_ec::PairingEngine,
    msp::MspPk<PE>: std::hash::Hash,
{
    // TODO: cleanup
    let keyreg_players: Vec<(usize, u64)> =
        stake_dist.iter().map(|(p, s)| (*p as usize, *s)).collect();

    let mut keyreg: key_reg::KeyReg<PE> = key_reg::KeyReg::new(&keyreg_players);

    for pid in stake_dist.keys() {
        let pk = keys.get(pid).map_or(
            Err(std::format!(
                "key not found for party {} in stake distribution",
                pid
            )),
            |key| Ok(key),
        )?;
        str_err_result(keyreg.register(*pid as usize, *pk))?;
    }

    return Ok(keyreg.close());
}

pub fn node_impl<N>(
    network: &N,
    params: &Parameters,
    stake_dist: &StakeDistribution,
) -> Result<(), String>
where
    N: Network,
{
    let mut ctx = node_init(network, &params, &stake_dist)?;
    node_oper(network, &params, &mut ctx)?;

    Ok(())
}

struct Context {
    signer: stm::StmSigner<blake2::Blake2b, Bls12_377>,
    participants: HashMap<PartyId, Hello>,
}

fn cache_sig_response(
    sig_cache: &mut HashMap<u64, HashMap<PartyId, SigResponse>>,
    from: PartyId,
    resp: &SigResponse,
) {
    let request_cache = match sig_cache.get_mut(&resp.request_id) {
        Some(m) => m,
        None => {
            let m = HashMap::new();
            sig_cache.insert(resp.request_id, m);
            sig_cache.get_mut(&resp.request_id).unwrap()
        }
    };

    // don't allow updates
    if !request_cache.contains_key(&from) {
        request_cache.insert(from, resp.clone());
    }
}

fn is_sig_complete(
    sig_cache: &mut HashMap<u64, HashMap<PartyId, SigResponse>>,
    ctx: &Context,
    request_id: u64,
) -> bool {
    let request_cache = match sig_cache.get(&request_id) {
        None => {
            return false;
        }
        Some(v) => v,
    };

    ctx.participants
        .keys()
        .all(|p| request_cache.contains_key(p))
}

fn node_oper<N>(network: &N, params: &Parameters, ctx: &mut Context) -> Result<(), String>
where
    N: Network,
{
    // TODO: make some kind of signature cache instead of keeping them around forever
    let mut sig_cache: HashMap<u64, HashMap<PartyId, SigResponse>> = HashMap::new();

    loop {
        let (from, msg) = network.recv()?; // TODO: handle transient errors and retry?  (or is this done in Network)
        match msg {
            Message::Hello(_) => { /* ignore */ }
            Message::SigMessage(req) => {
                let mut sigs: Vec<Signature> = Vec::new();
                for index in 0..params.m {
                    if let Some(sig) = ctx.signer.sign(&req.message, index) {
                        sigs.push(Signature {
                            index: index,
                            sig: ark_to_bytes(sig),
                        });
                    }
                }

                let response = SigResponse {
                    request_id: req.id,
                    signatures: sigs,
                };

                cache_sig_response(&mut sig_cache, network.me(), &response);

                // TODO: retry?
                network.send(&Message::SigResponse(response))?;
            }

            Message::SigResponse(resp) => {
                cache_sig_response(&mut sig_cache, from, &resp);
            }
        }
    }
}

fn node_init<N>(
    network: &N,
    params: &Parameters,
    stake_dist: &StakeDistribution,
) -> Result<Context, String>
where
    N: Network,
{
    // -- Setup phase -----------------------------------------------------------

    let mut rng = ChaCha20Rng::from_entropy();
    let params_be = as_backend_params(params);
    // fixme
    let me_usize: usize = network.me().try_into().unwrap();
    let stake: Stake = stake_dist
        .get(&network.me())
        .map_or(Err("invalid stake distribution"), |s| Ok(*s))?;

    let init: stm::StmInitializer<Bls12_377> =
        stm::StmInitializer::setup(params_be, me_usize, stake, &mut rng);

    // -- Registration phase ----------------------------------------------------

    let hello = Message::Hello(Hello {
        cardano_address: "???".to_string(), // TODO
        party_id: network.me(),
        stake: stake,
        public_key: ark_to_bytes(&init.verification_key()),
    });

    network.send(&hello)?;

    let mut peer_hello: HashMap<PartyId, Hello> = HashMap::new();
    let mut pks: HashMap<PartyId, ValidationKey> = HashMap::new();
    pks.insert(network.me(), init.verification_key().clone());

    // recv `Hello` until all peers have responded
    while !network.peers().iter().all(|p| peer_hello.contains_key(p)) {
        // TODO: configurable timeout
        let (from, msg) = network.recv()?; // TODO: timeout?
        match msg {
            Message::Hello(h) => {
                // TODO: what should happen if we get two `Hello` messages?
                if !peer_hello.contains_key(&from) {
                    pks.insert(from, ark_from_bytes(&h.public_key)?);
                    peer_hello.insert(from, h);
                }
            }

            _ => {
                // ignore message
                // TODO: handle this message later?
            }
        }
    }

    let key_reg = mk_keyreg(&stake_dist, &pks)?;
    let signer = init.new_signer(key_reg);

    Ok(Context {
        participants: peer_hello,
        signer: signer,
    })
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::network;
    use std::{collections::HashSet, thread};

    #[test]
    fn test_init() {
        let parameters = Parameters {
            m: 4,
            k: 3,
            phi_f: 0.4,
        };

        let mut stake_dist: HashMap<PartyId, Stake> = HashMap::new();
        for i in 0..4 {
            stake_dist.insert(i, 1);
        }

        let parties = HashSet::from_iter(stake_dist.keys().map(|k| *k));
        let mut networks = network::mk_testing_network(&parties);

        let mut handles: Vec<thread::JoinHandle<Option<String>>> = Vec::new();

        for p in &parties {
            let network = networks.remove(p).unwrap();
            let ps = parameters.clone();
            let dist = stake_dist.clone();
            let handle: std::thread::JoinHandle<Option<String>> = std::thread::spawn(move || {
                if let Err(s) = node_init(&network, &ps, &dist) {
                    return Some(s);
                }

                return None;
            });
            handles.push(handle);
        }

        // TODO: timeout?
        for handle in handles {
            match handle.join() {
                Ok(None) => {}
                Ok(Some(s)) => {
                    panic!("initialization failed: {}", s)
                }
                Err(_) => {
                    panic!("thread join failed with error")
                }
            }
        }
    }
}
