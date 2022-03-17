use std::env::args;

// script to print example messages when needed
use rust_node::config;
use rust_node::message;
use serde_json as json;

fn example_config() -> config::Config {
  config::Config {
    nodes: vec![
      config::NodeConfig {
        id: 1,
        address_local: "localhost:8000".to_string(),
        address_endpoint: "http://localhost:8000/".to_string(),
        stake: 1,
      }
    ],
    parameters:
      message::Parameters {
        k: 0,
        m: 0,
        phi_f: 0.25,
    },
  }
}

fn example_hello() -> message::Message {
  let hello = message::Hello {
    cardano_address: "address".to_string(),
    party_id: 1,
    stake: 1,
    public_key: vec![1,2,3,4],
  };

  message::Message::Hello(hello)
}


pub fn print_config() {
  let config =
    config::Config {
        nodes: vec![
          config::NodeConfig {
            id: 1,
            address_local: "localhost:8000".to_string(),
            address_endpoint: "http://localhost:8000/".to_string(),
            stake: 1,
          }
        ],
        parameters:
          message::Parameters {
            k: 0,
            m: 0,
            phi_f: 0.25,
        },
    };

  println!("{}", json::to_string_pretty(&config).unwrap());

}

fn main() {
  let msgtype: Vec<String> = args().collect();

  let m = match msgtype[1].as_str() {
    "hello" => json::to_string_pretty(&example_hello()).unwrap(),
    "config" => json::to_string_pretty(&example_config()).unwrap(),
    _ => "Unknown message!".to_string(),
  };

  print!("{}\n", m)
}