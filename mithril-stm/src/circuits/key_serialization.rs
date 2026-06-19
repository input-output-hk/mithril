//! Uniform byte (de)serialization interface for the circuits' PLONK keys.
//!
//! Both circuits expose verifying and proving keys, but as different concrete types: the
//! certificate circuit uses the high-level, self-describing `MidnightVK` / `MidnightPK`; the IVC
//! circuit uses the raw PLONK `PlonkVerifyingKey` / `PlonkProvingKey`. This trait gives them a
//! common `serialize_key` / `deserialize_key` surface so the key cache can treat them uniformly.
//!
//! Implementations live next to each circuit (`circuits::halo2::key_serialization`,
//! `circuits::halo2_ivc::key_serialization`) and use `SerdeFormat::RawBytes`.

use crate::StmResult;

/// Byte (de)serialization for a circuit verifying or proving key.
pub(crate) trait CircuitKeySerialization: Sized {
    /// Serializes the key to its production byte encoding.
    ///
    /// Named `serialize_key` (not `to_bytes`) because the raw PLONK `VerifyingKey`/`ProvingKey`
    /// already have inherent `to_bytes`/`from_bytes` methods that would shadow a trait method of
    /// the same name.
    fn serialize_key(&self) -> StmResult<Vec<u8>>;

    /// Deserializes the key from its production byte encoding.
    fn deserialize_key(bytes: &[u8]) -> StmResult<Self>;
}
