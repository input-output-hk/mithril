//! Event Store module
//! This module proposes tools to send messages between processes and how to
//! persist them in a separate database.
mod event;
mod event_store_entity;
mod transmitter_service;

pub use event::{Event, EventMessage, EventPersister};
pub use event_store_entity::EventStore;
pub use transmitter_service::TransmitterService;
