//! Golden tests lock in Halo2 circuit behavior for safe refactors and hardening.
//! cases.rs holds the test cases; support.rs provides shared helpers and witness builders.
//! Includes baseline plus positive and negative scenarios.

#[cfg(test)]
pub(crate) mod support;

#[cfg(test)]
pub(crate) mod cases;
