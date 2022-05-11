mod adapter;
mod dumb_adapter;

pub use adapter::Adapter;

#[cfg(test)]
pub use dumb_adapter::DumbAdapter;
