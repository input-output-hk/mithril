//! Single coupling point to the Midnight proving backend for the recursive (IVC) circuit.
//!
//! Every field, curve, and engine type the recursive circuit builds on derives from Midnight's
//! `BlstrsEmulation` — its self-emulation of BLS12-381 proof verification. Isolating them here
//! keeps the dependency on the Midnight backend in one place.

use midnight_circuits::ecc::curves::CircuitCurve as CircuitCurveTrait;
use midnight_circuits::verifier::{BlstrsEmulation, SelfEmulation};

/// Midnight self-emulation backend the recursive circuit is built on: BLS12-381 verification
/// emulated over its own scalar field. Parameterises the verifier gadget, accumulator, and MSM.
pub(crate) type RecursiveEmulation = BlstrsEmulation;

/// Native field the recursive circuit is defined over — the BLS12-381 scalar field. Every
/// in-circuit value lives here; it is the same field the certificate circuit calls `CircuitBase`.
pub(crate) type NativeField = <RecursiveEmulation as SelfEmulation>::F;

/// Curve verified in-circuit during recursive proof verification: BLS12-381 G1, on which the
/// recursive proof's commitments live. Its coordinates are emulated in [`EmulatedCurveBaseField`].
pub(crate) type EmulatedCurve = <RecursiveEmulation as SelfEmulation>::C;

/// Pairing engine backing the recursive circuit's KZG commitments (BLS12-381).
pub(crate) type PairingEngine = <RecursiveEmulation as SelfEmulation>::Engine;

/// Coordinate field of [`EmulatedCurve`] — the foreign field emulated in-circuit for BLS12-381 G1
/// arithmetic. Larger than and distinct from [`NativeField`]; not the certificate circuit's `CircuitBase`.
pub(crate) type EmulatedCurveBaseField = <EmulatedCurve as CircuitCurveTrait>::Base;
