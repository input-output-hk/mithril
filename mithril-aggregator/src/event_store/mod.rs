//! Event Store module
//! This module proposes tools to send messages between processes and how to
//! persist them in a separate database.
mod event;
mod runner;
mod transmitter_service;

pub use event::{Event, EventMessage, EventPersister};
pub use runner::EventStore;
pub use transmitter_service::TransmitterService;
