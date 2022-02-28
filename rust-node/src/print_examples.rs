// script to print example messages when needed
use crate::config;
use crate::config::NodeConfig;
use crate::message;
use serde_json as json;


pub fn print() {
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
