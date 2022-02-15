use crate::message::{PartyId, Message};
use std::collections::{HashMap,HashSet};
use std::sync::mpsc;
use std::time;

pub trait Network {
  fn me(&self) -> PartyId;
  fn peers(&self) -> Vec<PartyId>;
  // broadcast only?
  fn send(&self, message: &Message) -> Result<(), String>;
  fn recv(&self, timeout: time::Duration) -> Result<(PartyId, Message), String>;
}

// maybe this is the right abstraction and the Network trait is not really necessary?
pub struct ChannelNetwork {
  party_id: u64,
  input: mpsc::Receiver<(PartyId, Message)>,
  output: HashMap<PartyId, mpsc::Sender<(PartyId, Message)>>,
}

impl Network for ChannelNetwork {
    fn me(&self) -> PartyId {
      return self.party_id;
    }

    fn peers(&self) -> Vec<PartyId> {
      return self.output.keys().map(|a| *a).collect();
    }

    fn send(&self, message: &Message) -> Result<(), String> {
      for out_channel in self.output.values() {
        out_channel.send((self.me(), message.clone()));
      }

      Ok(())
    }

    fn recv(&self, timeout: time::Duration) -> Result<(PartyId, Message), String> {
      match self.input.recv_timeout(timeout) {
        Ok(m) => Ok(m),
        Err(e) => Err("Timeout exceeded".to_string())
      }
    }
}

pub fn mk_testing_network(parties: &HashSet<PartyId>) -> HashMap<PartyId, ChannelNetwork> {
  let mut recver : HashMap<PartyId, mpsc::Receiver<(PartyId, Message)>> = HashMap::new();
  let mut sender : HashMap<PartyId, mpsc::Sender<(PartyId, Message)>> = HashMap::new();

  for pid in parties {
    let (tx, rx) = mpsc::channel();
    recver.insert(*pid, rx);
    sender.insert(*pid, tx);
  }

  let mut result : HashMap<PartyId, ChannelNetwork> = HashMap::new();
  for me in parties {
    let mut peers = HashMap::new();
    for other in parties {
      if me != other {
        peers.insert(*other, sender.get(other).unwrap().clone());
      }
    }

    let net =
      ChannelNetwork {
        party_id: *me,
        input: recver.remove(me).unwrap(),
        output: peers,
      };

    result.insert(*me, net);
  }

  return result;
}

