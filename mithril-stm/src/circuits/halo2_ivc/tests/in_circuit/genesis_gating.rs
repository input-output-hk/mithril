//! Tests that the circuit bypasses proof verification at genesis (step 0).
//!
//! At genesis the circuit must accept arbitrary certificate and IVC proof bytes
//! because there is no prior certificate or IVC proof to verify. These slow
//! MockProver checks confirm the gating condition is wired correctly.
